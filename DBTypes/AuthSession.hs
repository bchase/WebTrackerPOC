module DBTypes.AuthSession where

import Data.ByteString (ByteString)
import Data.Functor.Contravariant ((>$<))
import Data.Monoid ((<>))
import Data.Time.Clock (UTCTime)
import Data.UUID (UUID)
import qualified Hasql.Decoders as Decode
import qualified Hasql.Encoders as Encode

import DBTypes (DBTuple, KeyedTable, Table, WriteableTable)

data Row = Row {
  identifier :: UUID,
  account :: UUID,
  hash :: ByteString,
  expires :: UTCTime
  }

instance DBTuple Row where
  columns = const ["identifier", "account", "hash", "expires"]
  encoder = const $ (  (identifier >$< (Encode.param Encode.uuid))
                    <> (account >$< (Encode.param Encode.uuid))
                    <> (hash >$< (Encode.value Encode.bytea))
                    <> (expires >$< (Encode.value Encode.timestamptz))
                    )
  decoder = const $ do
                      identifier' <- Decode.column Decode.uuid
                      account' <- Decode.column Decode.uuid
                      hash' <- Decode.column Decode.bytea
                      expires' <- Decode.column Decode.timestamptz
                      return Row {
                        identifier = identifier',
                        account = account',
                        hash = hash',
                        expires = expires'
                        }

instance Table Row where
  table = const "auth_session"

instance WriteableTable Row where {}

data PrimaryKey = PrimaryKey { u :: UUID }

instance DBTuple PrimaryKey where
  columns = const ["identifier"]
  encoder = const $ u >$< Encode.param Encode.uuid
  decoder = const $ do
                    u' <- Decode.column Decode.uuid
                    return PrimaryKey { u = u' }

instance KeyedTable PrimaryKey Row where {}

