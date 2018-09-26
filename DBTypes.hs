{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module DBTypes where

import Data.Monoid ((<>), mconcat)
import Data.List (intercalate)
import Data.Text (pack)
import Data.Text.Encoding (encodeUtf8)
import qualified Hasql.Decoders as Decode
import qualified Hasql.Encoders as Encode
import Hasql.Session (Session, statement)
import Hasql.Statement (Statement(Statement))

byteStringFromParts = encodeUtf8 . pack . mconcat

class DBTuple t where
  columns :: Maybe t -> [String]
  encoder :: Maybe t -> Encode.Params t
  decoder :: Maybe t -> Decode.Row t

names :: DBTuple t => Maybe t -> String
names m = intercalate ", " $ columns m

placeholders :: DBTuple t => Maybe t -> String
placeholders m = let placeholder (i, n) = "$" <> (show i)
                 in intercalate ", " $ fmap placeholder $ zip [1..] $ columns m

whereClauses :: DBTuple t => Maybe t -> String
whereClauses m = let placeholder (i, n) = n <> " = $" <> (show i)
                 in intercalate " AND " $ fmap placeholder $ zip [1..] $ columns m

class DBTuple row => Table row where
  table :: Maybe row -> String

getAllRows :: (DBTuple row, Table row) => Session [row]
getAllRows =
  let template = (Nothing :: Maybe row)
      sql = byteStringFromParts ["SELECT ", names template, " FROM ", table template]
  in statement () $ Statement sql Encode.unit (Decode.rowList $ decoder template) False

class (DBTuple key, Table row) => KeyedTable key row where -- It seems very likely that there should be a functional dependancy key -> row
  getRow :: key -> Session (Maybe row)
  getRow k =
    let template = (Nothing :: Maybe row)
        keyTemplate = (Nothing :: Maybe key)
        sql = byteStringFromParts ["SELECT ", names keyTemplate, " FROM ", table template, " WHERE ", whereClauses keyTemplate]
    in statement k $ Statement sql (encoder keyTemplate) (Decode.rowMaybe $ decoder template) False

class (Table row) => WritableTable row where
  addRow :: row -> Session ()
  addRow newRow =
    let template = (Nothing :: Maybe row)
        sql = byteStringFromParts ["INSERT INTO ", table template, " (", names template, ") VALUES (", placeholders template,  ")"] 
    in statement newRow $ Statement sql (encoder template)  Decode.unit False
  
