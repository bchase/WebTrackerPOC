{-# LANGUAGE OverloadedStrings #-}

import Web.Scotty (ActionM, get, liftAndCatchIO, param, raise, scotty, text)
import Data.Monoid ((<>))
import Data.Text.Lazy (pack)
import Hasql.Pool (acquire, Pool, release, use)
import Hasql.Session (Session)

import DBHelpers (dbPool, scottyActionFromEitherError, scottyDoesDB, scottyGuarenteesDB)
import qualified DBTypes.Account as Account (Row, PrimaryKey)
import qualified DBTypes.AuthSession as AuthSession (Row, PrimaryKey)
import qualified DBTypes.Consumption as Consumption (Row)

main = do
  pool <- dbPool settings
  scotty 3000 $ do
    get "/" $ homepage pool
    put "/login" $ handleLogin pool
    post "/consume" $ noteConsumption pool
  release pool -- should we do this? 

homepage connections = do
  (accounts :: [Account.Row]) <- scottyDoesDB connections getAllRows
  let accountNames = intercalate ",\n" $ fmap (("\"" <>) . (<> "\"") . Account.name) accounts
  text accountNames

handleLogin pool = do
  (username :: Text) <- param "username"
  (maybeExistingUser :: Account.Row) <- fmap listToMaybe $ fmap (filter((username ==) . Account.name)) $ scottyDoesDB connections getAllRows
  (newAuthSession :: AuthSession.Row, key :: ByteString) <- maybe makeNewUserSession makeSessionForUser maybeExistingUser
  setSimpleCookie "authID" $ toString $ AuthSession.identifier newAuthSession
  setSimpleCookie "authKey" $ decodeUtf8 key --TODO: make this not errorable!
  text username

noteConsumption connections = do
  (referer :: Text) <- getReferer
  (recievedAt <- getTime
  (authID :: UUID) <- fromText $ getCookie "authID"
  (authKey :: ByteString) <- encodeUtf8 $ getCookie "authKey" 
  authSession <- scottyDoesDB connections $ getRow $ AuthSession.PrimaryKey authID
  if checkPassword (hash authSession) authKey
    then do
           user = scottyGuarenteesDB $ getRow $ Account.PrimaryKey $ account authSession
           scottyDoesDB connections $ addRow $ Consumption { consumer = identifier user, item = referer, happened = recievedAt }
           text $ name user
    else do
           status paymentRequired402
           text ""

