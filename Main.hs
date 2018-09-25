{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.ConfigFile (emptyCP, readfile, simpleAccess)
import Data.Monoid ((<>))
import Data.Text.Lazy (pack)
import Data.Tuple.Sequence (sequenceT)
import Hasql.Pool (release)
import System.Exit (die)
import Web.Scotty (ActionM, get, liftAndCatchIO, scotty)

dieOnConfigError = let handleError cpError = die $ concat ["There was a config file error: ", show $ fst cpError, " ", snd cpError]
                   in either handleError return

configurationParser fileName = do
  eitherErrorParser <- readfile emptyCP fileName
  dieOnConfigError eitherErrorParser

main = do
  conf <- configurationParser "defaults.config"
  dbSettings <- let dbConf = dieOnConfigError . (simpleAccess conf "DatabaseConnectionPool")
                in  sequenceT (
		  dbConf "maxConnections",
		  dbConf "maxIdleSeconds",
                  dbConf "host",
		  dbConf "port",
		  dbConf "user",
                  dbConf "password",
		  dbConf "database"
		)
  pool <- dbPool bdSettings
  scotty 3000 $ do
    get "/" $ homepage pool
    put "/login" $ handleLogin pool
    post "/consume" $ noteConsumption pool
  release pool -- should we do this? 


