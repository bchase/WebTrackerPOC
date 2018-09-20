module DBHelpers where

import Data.Monoid ((<>))
import Data.Text.Lazy (pack)
import Hasql.Connection (Connection, Settings)
import Hasql.Session (run, Session)
import Web.Scotty (ActionM, liftAndCatchIO, raise)

scottyActionFromEitherError kind =
  let raiseError = raise . pack . ("There was a " <> kind <> " error: " <>) . show
  in either raiseError return

dbConnection :: Settings -> ActionM Connection
dbConnection settings = do
  eitherErrorOrConnection <- liftAndCatchIO $ acquire settings
  scottyActionFromEitherError "database connection" eitherErrorOrConnection

scottyDoesDBIO :: Connection -> Session a -> ActionM a
scottyDoesDBIO connection session = do
  eitherErrorOrX <- liftAndCatchIO $ run session connection
  scottyActionFromEitherError "database session" eitherErrorOrX

scottyDoesDBWithMaybeAsError :: Connection -> Session (Maybe a) -> ActionM a
scottyDoesDBWithMaybeAsError connection session = do
  maybeX <- scottyDoesDBIO connection session
  maybe
    (raise "We couldn't find the thing in the DB.")
    (return)
    maybeX

