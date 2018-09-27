{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module DBTypes where

import Data.List (intercalate)
import Data.Monoid ((<>), mconcat)
import Data.Proxy (Proxy(Proxy))
import Data.Text (pack)
import Data.Text.Encoding (encodeUtf8)
import qualified Hasql.Decoders as Decode
import qualified Hasql.Encoders as Encode
import Hasql.Session (Session, statement)
import Hasql.Statement (Statement(Statement))

class DBTuple t where
  columns :: Proxy t -> [String]
  encoder :: Proxy t -> Encode.Params t
  decoder :: Proxy t -> Decode.Row t

class DBTuple row => Table row where
  table :: Proxy row -> String

class (DBTuple key, DBTuple row, Table row) => KeyedTable key row where {} -- It seems very likely that there should be a functional dependancy key -> row

class (DBTuple row, Table row) => WritableTable row where {}

buildSession sqlParts encoder' decoder' param =
  statement param $ Statement (encodeUtf8 $ pack $ mconcat sqlParts) encoder' decoder' False

names :: DBTuple t => Proxy t -> String
names p = intercalate ", " $ columns p

placeholders :: DBTuple t => Proxy t -> String
placeholders p = let placeholder (i, _) = "$" <> (show i)
                 in intercalate ", " $ fmap placeholder $ zip [1..] $ columns p

whereClauses :: DBTuple t => Proxy t -> String
whereClauses p = let placeholder (i, name) = name <> " = $" <> (show i)
                 in intercalate " AND " $ fmap placeholder $ zip [1..] $ columns p

getAllRows :: (DBTuple row, Table row) => Session [row]
getAllRows = let getAllRows' template = buildSession
                                          ["SELECT ", names template,
                                            " FROM ", table template]
                                          Encode.unit
                                          (Decode.rowList $ decoder template)
                                          ()
             in getAllRows' Proxy

getRow :: (DBTuple key, DBTuple row, Table row, KeyedTable key row) => key -> Session (Maybe row)
getRow = let getRow' keyTemplate rowTemplate k = buildSession
                                                   ["SELECT ", names keyTemplate,
                                                     " FROM ", table rowTemplate,
                                                     " WHERE ", whereClauses keyTemplate]
                                                   (encoder keyTemplate)
                                                   (Decode.rowMaybe $ decoder rowTemplate)
                                                   k
         in getRow' Proxy Proxy

addRow :: (DBTuple row, Table row) => row -> Session ()
addRow = let addRow' template newRow = buildSession
                                         ["INSERT INTO ", table template,
                                           " (", names template, ")",
                                           " VALUES (", placeholders template, ")"]
                                         (encoder template)
                                         Decode.unit
                                         newRow
         in addRow' Proxy


