{-# LANGUAGE OverloadedStrings #-}

module Endpoints where

import Data.Monoid ((<>))
import Data.Text.Encoding (decodeUtf8', encodeUtf8)
import Hasql.Pool (Pool)
import Web.Scotty (ActionM, liftAndCatchIO, param, raise, text)
import Web.Scotty.Cookie (getCookie, setSimpleCookie)

import DBHelpers (addRow, dbPool, getAllRows, getRow, scottyActionFromEitherError, scottyDoesDB, scottyGuarenteesDB)
import qualified DBTypes.Account as Account (name, PrimaryKey, Row)
import qualified DBTypes.AuthSession as AuthSession (identifier, PrimaryKey, Row)
import qualified DBTypes.Consumption as Consumption (Row)

homepage :: Pool -> ActionM
homepage connections = do
  (accounts :: [Account.Row]) <- scottyDoesDB connections getAllRows
  let accountNames = intercalate ",\n" $ fmap (("\"" <>) . (<> "\"") . Account.name) accounts
  text accountNames

handleLogin :: Pool -> ActionM
handleLogin connections = do
  (username :: Text) <- param "username"
  (maybeExistingUser :: Account.Row) <- fmap listToMaybe $ fmap (filter((username ==) . Account.name)) $ scottyDoesDB connections getAllRows
  ((newAuthSession, key) :: (AuthSession.Row, ByteString)) <- maybe makeNewUserSession makeSessionForUser maybeExistingUser
  setSimpleCookie "authID" $ toString $ AuthSession.identifier newAuthSession
  either
    (text . show)
    (setSimpleCookie "authKey")
    (decodeUtf8' key)
  text username
  
noteConsumption :: Pool -> ActionM    -- This needs to be rewritten with better monadic interaction (ActionM <-> Maybe) and more informative error messages.
noteConsumption connections = do
  (referer' :: Maybe Text) <- getReferer
  (recievedAt :: UTCTime) <- getTime
  (authID' :: Maybe UUID) <- fmap fromText $ getCookie "authID"
  (authKey' :: Maybe ByteString) <- fmap encodeUtf8 $ getCookie "authKey" 
  authSession' <- fromMaybe (return Nothing) $ do -- the maybe monad
    authID <- authID'
    authKey <- authKey'
    let getMaybeSession = scottyDoesDB connections $ getRow $ AuthSession.PrimaryKey authID
    let ifAuthenticated sess = if checkPassword (hash sess) authKey then Just sess else Nothing
    return $ fmap (>>= ifAuthenticated) getMaybeSession
  let consumption' = do -- the maybe monad
    referer <- referer'
    authSession <- authSession'
    return Consumption { consumer = account authSession, item = referer, happened = recievedAt }
  maybe
    (do
      status paymentRequired402
      text "")
    (\consumption -> do
      scottyDoesDB connections $ addRow consumption
      user <- scottyGuarenteesDB $ getRow $ Account.PrimaryKey $ consumer consumption
      text $ name user)
    consumption'

