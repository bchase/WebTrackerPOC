{-# LANGUAGE OverloadedStrings #-}

import Web.Scotty (ActionM, get, liftAndCatchIO, param, raise, scotty, text)
import Data.Monoid ((<>))
import Data.Text.Lazy (pack)
import Hasql.Pool (acquire, Pool, release, use)
import Hasql.Session (Session)

import DBHelpers (dbPool, scottyActionFromEitherError, scottyDoesDBIO, scottyGuarenteesDB)

main = do
  pool <- dbPool settings
  scotty 3000 $ do
    get "/" $ homepage pool
    put "/login" $ handleLogin pool
    post "/consume" $ noteConsumption pool
  release pool -- should we do this? 

homepage connections = do
  

