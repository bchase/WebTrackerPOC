{-# LANGUAGE OverloadedStrings #-}

module DBHelpers where

import Data.Monoid ((<>))
import Data.Text.Lazy (pack)
import Data.Time.Clock () --NominalDiffTime instance Num
import Hasql.Connection (settings)
import Hasql.Pool (acquire, Pool, use)
import Hasql.Session (Session)
import System.Exit (die)
import Text.Read (readEither)
import Web.Scotty (ActionM, liftAndCatchIO, raise)

scottyActionFromEitherError kind =
  let raiseError = raise . pack . (("There was a " <> kind <> " error: ") <>) . show
  in either raiseError return

dbPool :: (String, String, String, String, String, String, String) -> IO Pool
dbPool (maxConnections', maxIdleSeconds', host', port', user', password', database') =
  either die id $ do -- the either monad
    maxConnections <- readEither maxConnections'
    maxIdleSeconds <- fmap fromInteger $ readEither maxIdleSeconds'
    host <- readEither host'
    port <- readEither port'
    user <- readEither user'
    password <- readEither password'
    database <- readEither database'
    return $ acquire (
      maxConnections,
      maxIdleSeconds,
      settings host port user password database
      )

scottyDoesDB :: Pool -> Session a -> ActionM a
scottyDoesDB pool session = do
  eitherErrorOrX <- liftAndCatchIO $ use pool session
  scottyActionFromEitherError "database session" eitherErrorOrX

scottyGuarenteesDB :: Pool -> Session (Maybe a) -> ActionM a
scottyGuarenteesDB connection session = do
  maybeX <- scottyDoesDB connection session
  let eitherX = maybe (Left "We couldn't find the thing in the DB.") (Right) maybeX
  scottyActionFromEitherError "database guarentee" eitherX

 
