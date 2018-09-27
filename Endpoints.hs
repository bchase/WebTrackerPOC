{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Endpoints where

import Data.ByteString (ByteString)
import Data.List (intersperse)
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Monoid ((<>))
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8', encodeUtf8)
import Data.Text.Lazy (fromStrict, pack)
import Data.Time.Clock (UTCTime)
import Data.UUID (fromText, toText, UUID)
import Hasql.Pool (Pool)
import Network.HTTP.Types.Status (paymentRequired402)
import Web.Scotty (ActionM, liftAndCatchIO, param, raise, status, text)
import Web.Scotty.Cookie (getCookie, setSimpleCookie)

import DBHelpers (dbPool, scottyActionFromEitherError, scottyDoesDB, scottyGuarenteesDB)
import DBTypes (addRow, getAllRows, getRow)
import qualified DBTypes.Account as Account (name, PrimaryKey(..), Row(..))
import qualified DBTypes.AuthSession as AuthSession (identifier, PrimaryKey(..), Row(..))
import qualified DBTypes.Consumption as Consumption (Row(..))

homepage :: Pool -> ActionM ()
homepage connections = do
  (accounts :: [Account.Row]) <- scottyDoesDB connections getAllRows
  let accountNames = mconcat $ intersperse ",\n" $ fmap (("\"" <>) . (<> "\"") Account.name) accounts
  text $ fromStrict accountNames

handleLogin :: Pool -> ActionM ()
handleLogin connections = do
  (username :: Text) <- param "username"
  (maybeExistingUser :: Maybe Account.Row) <- fmap listToMaybe $ fmap (filter((username ==) . Account.name)) $ scottyDoesDB connections getAllRows
  ((newAuthSession, key) :: (AuthSession.Row, ByteString)) <- maybe makeNewUserSession makeSessionForUser maybeExistingUser
  setSimpleCookie "authID" $ toText $ AuthSession.identifier newAuthSession
  either
    (text . pack . show)
    (setSimpleCookie "authKey")
    (decodeUtf8' key)
  text $ fromStrict username
  
noteConsumption :: Pool -> ActionM ()  -- This needs to be rewritten with better monadic interaction (ActionM <-> Maybe) and more informative error messages.
noteConsumption connections = do
  (referer' :: Maybe Text) <- getReferer
  (recievedAt :: UTCTime) <- getTime
  (authID' :: Maybe UUID) <- fmap (fmap fromText) $ getCookie "authID"
  (authKey' :: Maybe ByteString) <- fmap (fmap encodeUtf8) $ getCookie "authKey" 
  authSession' <- fromMaybe (return Nothing) $ do -- the maybe monad
    authID <- authID'
    authKey <- authKey'
    let getMaybeSession = scottyDoesDB connections $ getRow $ AuthSession.PrimaryKey authID
    let ifAuthenticated sess = if checkPassword (AuthSession.hash sess) authKey then Just sess else Nothing
    return $ fmap (>>= ifAuthenticated) getMaybeSession
  let consumption' = do -- the maybe monad
        referer <- referer'
        authSession <- authSession'
        return Consumption.Row {
	         Consumption.consumer = account authSession,
                 Consumption.item = referer,
                 Consumption.happened = recievedAt
               }
  maybe
    (do
      status paymentRequired402
      text "")
    (\consumption -> do
      scottyDoesDB connections $ addRow consumption
      user <- scottyGuarenteesDB $ getRow $ Account.PrimaryKey $ consumer consumption
      text $ name user)
    consumption'

