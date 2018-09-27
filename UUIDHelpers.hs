module UUIDHelpers (
  UUID,
  v4uuidIO,
  v5uuidFromTime,
  asPassword,
  toEText,
  maybeFromEText
) where

import qualified Data.ByteString as EBS (unpack)
import qualified Data.ByteString.Lazy as LBS (toStrict)
import qualified Data.Text as EText (pack)
import Data.Text.Encoding (encodeUtf8)
import qualified Data.Text.Lazy as LText (toStrict)
import Data.Time.Clock (UTCTime)
-- import qualified Data.Time.Format.ISO8601 as ISO8601
import Data.UUID (fromText, toByteStrig, UUID)
import Data.UUID.V4 (nextRandom)
import Data.UUID.V5 (generateNamed)
import Web.Scotty (Parsable)

instance Scotty.Parsable UUID where
  -- parseParam :: LText.Text -> Either LText.Text UUID
  parseParam = (maybe (Left "Unable to parse UUID") Right) . fromText . LText.toStrict

randomUUID :: IO UUID
randomUUID = nextRandom

childUUIDFromTime :: UUID -> UTCTime -> UUID
childFromTime parent time =
  generateNamed parent $ EBS.unpack $ encodeUtf8 $ EText.pack $ "this is a dummy string" -- ISO8601.iso8601Show time

asPassword :: UUID -> EBS.ByteString
asPassword = LBS.toStrict . toByteString

toEText :: UUID ->EText.Text
toEText = toText
maybeFromEText :: EText.Text -> Maybe UUID
maybeFromEText = fromText
