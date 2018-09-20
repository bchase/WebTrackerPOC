module DBTypes.Consumption where

import Data.ByteString
import Data.Functor.Contravariant ((>$<))
import Data.Monoid ((<>))
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import Data.UUID (UUID)
import qualified Hasql.Decoders as Decode
import qualified Hasql.Encoders as Encode
import Hasql.Query (Query, statement)

data Row = Row {
  consumer :: UUID,
  item :: Text,
  happened :: UTCTime,
}

addRow :: Query Row ()
addRow = statement
  "INSERT INTO consumption (consumer, item, happened, status) VALUES ($1, $2, $3, $4 ::consumption_status)"
  ( (consumer >$< (Encode.value Encode.uuid)) <>
    (item >$< (Encode.value Encode.uuid)) <>
    (happened >$< (Encode.value Encode.timestamptz)) <>
    (status >$< (Encode.value Encode.text)) )
  (Decode.unit)
  False
