module DBHelpers where

import Data.Monoid ((<>), mconcat)
import Data.Text.Lazy (pack)
import Data.Time.Clock () --NominalDiffTime instance Num
import Hasql.Connection (settings)
import Hasql.Pool (acquire, Pool, use)
import Hasql.Session (run, Session, statement)
import Hasql.Statement (Statement)
import Web.Scotty (ActionM, liftAndCatchIO, raise)

scottyActionFromEitherError kind =
  let raiseError = raise . pack . ("There was a " <> kind <> " error: " <>) . show
  in either raiseError return

dbPool :: (Int,            Int,                                 ByteString, Word16, ByteString, ByteString, ByteString) -> IO Pool
dbPool (   maxConnections, maxIdleSeconds,                      host,       port,   user,       password,   database) =
  acquire (maxConnections, fromInteger maxIdleSeconds, settings host        port    user        password    database)

scottyDoesDBIO :: Pool -> Session a -> ActionM a
scottyDoesDBIO pool session = do
  eitherErrorOrX <- liftAndCatchIO $ use pool session
  scottyActionFromEitherError "database session" eitherErrorOrX

scottyGuarenteesDB :: Pool -> Session (Maybe a) -> ActionM a
scottyGuarenteesDB connection session = do
  maybeX <- scottyDoesDBIO connection session
  let eitherX = maybe (Left "We couldn't find the thing in the DB.") (Right) maybeX
  scottyActionFromEitherError "database guarentee" eitherX

class DBTuple t where
  columns :: Maybe t -> [String]
  encoder :: Maybe t -> Encode.Params t
  decoder :: Maybe t -> Decode.Row t
  names :: Maybe t -> String
  names m = intercalate ", " $ columns m
  placeholders :: Maybe t -> String
  placeholders m = let placeholder (i, n) = "$" <> (show i)
                   in intercalate ", " $ fmap placeholder $ zip [1..] $ columns m
  whereClauses :: Maybe t -> String
  whereClauses m = let placeholder (i, n) = n <> " = $" <> (show i)
                   in intercalate " AND " $ fmap placeholder $ zip [1..] $ columns m

class (DBTuple key, DBTuple row) => ReadableTable key row where
  table :: Maybe row -> String
  getRow :: key -> Session (Maybe row)
  getRow k =
    let template = (Nothing :: Maybe row)
        sql = mconcat ["SELECT ", names k, " FROM ", table template, " WHERE ", whereclauses k]
    in statement k $ Statement sql, (encoder k) (rowMaybe $ decoder template) False
  getAllRows :: Session [row]
  getAllRows =
    let template = (Nothing :: Maybe row)
        sql = mconcat ["SELECT ", names template, " FROM ", table template]
    in statement () $ Statement sql, Encode.unit (rowList $ decoder template) False

class (DBTuple row) => WritableTable row where
  table :: Maybe row -> String
  addRow :: row -> Session ()
  addRow newRow =
    let template = (Nothing :: Maybe row)
        sql = mconcat ["INSERT INTO ", table template, " (", names template, ") VALUES (", placeholders template,  ")"] 
    in statement newRow $ Statement sql (encoder template)  Decode.unit False
  
