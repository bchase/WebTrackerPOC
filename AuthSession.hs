module DBTypes.AuthSession where

import Data.ByteString (ByteString)
import Data.Functor.Contravariant ((>$<))
import Data.Monoid ((<>))
import Data.Time.Clock (UTCTime)
import Data.UUID (UUID)
import qualified Hasql.Decoders as Decode
import qualified Hasql.Encoders as Encode
import Hasql.Query (Query, statement)

data Row = Row {
  identifier :: UUID,
  account :: UUID,
  hash :: ByteString,
  expires :: UTCTime
}

addRow :: Query Row ()
addRow = statement
  "INSERT INTO auth_session (identifier, account, hash, expires) VALUES ($1, $2, $3, $4)"
  ( (identifier >$< (Encode.value Encode.uuid)) <>
    (account >$< (Encode.value Encode.uuid)) <>
    (hash >$< (Encode.value Encode.bytea)) <>
    (expires >$< (Encode.value Encode.timestamptz)) )
  (Decode.unit)
  False

getRow :: Query UUID (Maybe Row)
getRow = statement
  "SELECT identifier, account, hash, expires FROM auth_session WHERE identifier = $1"
  (Encode.value Encode.uuid)
  (Decode.maybeRow (do identifier <- Decode.value Decode.uuid
                       account <- Decode.value Decode.uuid
                       hash <- Decode.value Decode.bytea
                       expires <- Decode.value Decode.timestamptz
                       return Row{
                         identifier = identifier,
                         account = account,
                         hash = hash,
                         expires = expires } ))
  False

    
