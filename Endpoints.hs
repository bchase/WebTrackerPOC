{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Endpoints where

import Control.Monad ((>>=))
import Data.ByteString (ByteString)
import Data.List (intersperse)
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Monoid ((<>))
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8', encodeUtf8)
import Data.Text.Lazy (fromStrict, pack, toStrict)
import Data.Time.Clock (UTCTime)
import Data.UUID (fromText, toText, UUID)
import Hasql.Pool (Pool)
import Network.HTTP.Types.Status (paymentRequired402)
import Web.Scotty (ActionM, liftAndCatchIO, param, raise, status, text)
import Web.Scotty.Cookie (getCookie, setSimpleCookie)

import AuthSessionHelpers (makeNewUserSession, makeSessionForUser)
import DBHelpers (dbPool, scottyActionFromEitherError, scottyDoesDB, scottyGuarenteesDB)
import DBTypes (addRow, getAllRows, getRow)
import qualified DBTypes.Account as Account (name, PrimaryKey(..), Row(..))
import qualified DBTypes.AuthSession as AuthSession (identifier, PrimaryKey(..), Row(..))
import qualified DBTypes.Consumption as Consumption (Row(..))
import ScottyHelpers (getReferer, getTime)

homepage :: Pool -> ActionM ()
homepage connections = do
  (accounts :: [Account.Row]) <- scottyDoesDB connections getAllRows
  let accountNames = mconcat $ intersperse ",\n" $ fmap (("\"" <>) . (<> "\"") . Account.name) accounts
  text $ fromStrict accountNames

handleLogin :: Pool -> ActionM ()
handleLogin connections = do
  (username :: Text) <- param "username"
  (maybeExistingUser :: Maybe Account.Row) <- fmap listToMaybe $ fmap (filter((username ==) . Account.name)) $ scottyDoesDB connections getAllRows
  ((newAuthSession, key) :: (AuthSession.Row, ByteString)) <- maybe
                                                                (makeNewUserSession connections $ fromStrict username)
                                                                (makeSessionForUser connections)
                                                                maybeExistingUser
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
  (authID'' :: Maybe Text) <- getCookie "authID"
  let (authID' :: Maybe UUID) = authID'' >>= fromText
  (authKey' :: Maybe ByteString) <- fmap (fmap encodeUtf8) $ getCookie "authKey" 
  authSession' <- fromMaybe (return Nothing) $ do -- the maybe monad
    authID <- authID'
    authKey <- authKey'
    let getMaybeSession = scottyDoesDB connections $ getRow $ AuthSession.PrimaryKey authID
    let checkPassword = undefined
    let ifAuthenticated sess = if checkPassword (AuthSession.hash sess) authKey then Just sess else Nothing
    return $ fmap (>>= ifAuthenticated) getMaybeSession
  let consumption' = do -- the maybe monad
        referer <- referer'
        authSession <- authSession'
        return Consumption.Row {
                 Consumption.consumer = AuthSession.account authSession,
                 Consumption.item = referer,
                 Consumption.happened = recievedAt
               }
  maybe
    (do
      status paymentRequired402
      text "")
    (\consumption -> do
      scottyDoesDB connections $ addRow consumption
      user <- scottyGuarenteesDB connections $ getRow $ Account.PrimaryKey $ Consumption.consumer consumption
      text $ fromStrict $ Account.name user)
    consumption'

