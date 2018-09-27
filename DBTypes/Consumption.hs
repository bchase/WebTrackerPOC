{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module DBTypes.Consumption where

import Data.Functor.Contravariant ((>$<))
import Data.Monoid ((<>))
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import Data.UUID (UUID)
import qualified Hasql.Decoders as Decode
import qualified Hasql.Encoders as Encode

import DBTypes (DBTuple(..), KeyedTable, Table(..), WritableTable)

data Row = Row {
  consumer :: UUID,
  item :: Text,
  happened :: UTCTime
}

instance DBTuple Row where
  columns = const ["consumer", "item", "happened", "status"]
  encoder = const $ (  (consumer >$< (Encode.param Encode.uuid))
                    <> (item >$< (Encode.param Encode.text))
                    <> (happened >$< (Encode.param Encode.timestamptz))
                    )
  decoder = const $ do
                      consumer' <- Decode.column Decode.uuid
                      item' <- Decode.column Decode.text
                      happened' <- Decode.column Decode.timestamptz
                      return Row {
                        consumer = consumer',
                        item = item',
                        happened = happened'
                        }

instance Table Row where
  table =  const "consumption"

instance WritableTable Row where {}


