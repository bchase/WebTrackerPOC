module DBTypes.Account where

import Data.Functor.Contravariant ((>$<))
import Data.Monoid ((<>))
import Data.Text (Text)
import Data.UUID (UUID)
import qualified Hasql.Decoders as Decode
import qualified Hasql.Encoders as Encode 

import DBTypes (DBTuple, KeyedTable, Table, WriteableTable)

data Row = Row {
  identifier :: UUID,
  name :: Text
}

instance DBTuple Row where
  columns = const ["identifier", "name"]
  encoder = const $ (  (identifier >$< (Encode.param Encode.uuid))
                    <> (name >$< (Encode.param Encode.text))
                    )
  decoder = const $ do
                      identifier' <- Decode.column Decode.uuid
		      name' <- Decode.column Decode.text
                      return Row {
                        identifier = identifier',
                        name = name'
                        }

instance Table Row where
  table = const "account"

instance WritableTable Row where {}

data PrimaryKey = PrimaryKey { u :: UUID }

instance DBTuple PrimaryKey where
  columns = const ["identifier"]
  encoder = const $ u >$< Encode.param Encode.uuid
  decoder = const do
                    u' <- Decode.column Decode.uuid
                    return PrimaryKey { u = u' }

instance KeyedTable PrimaryKey Row where {}

