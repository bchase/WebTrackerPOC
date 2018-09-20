module DBTypes.Account where

import Data.Functor.Contravariant ((>$<))
import Data.Monoid ((<>))
import Data.Text (Text)
import Data.UUID (UUID)
import qualified Hasql.Decoders as Decode
import qualified Hasql.Encoders as Encode 
import Hasql.Query (Query, statement)

data Row = Row {
  identifier :: UUID,
  name :: Text,
}

addRow :: Query Row ()
addRow = statement
  "INSERT INTO account (identifier, name) VALUES ($1, $2)"
  ( (identifier >$< (Encode.value Encode.uuid)) <>
    (name >$< (Encode.value Encode.text)) <>
  (Decode.unit)
  False
    
