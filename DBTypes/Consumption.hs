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

_table = const "consumption"

instance DBTuple Row where
  columns = const ["consumer", "item", "happened", "status"]
  encoder = const $ (  (consumer >$< (Encode.param Encode.uuid))
                    <> (item >$< (Encode.value Encode.uuid))
                    <> (happened >$< (Encode.value Encode.timestamptz))
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

instance WritableTable Row where
  table = _table


