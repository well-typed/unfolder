module Util.Aeson (
    -- * Re-exports
    FromJSON(..), ToJSON(..)
    -- * Files
  , writeJSON
  , tryReadJSON
    -- * Serialization
  , encodeText
  , decodeText
  ) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Aeson qualified as Aeson
import System.Directory
import Data.Text (Text)
import Data.Text.Encoding qualified as Text
import Data.ByteString.Lazy qualified as BSL

{-------------------------------------------------------------------------------
  Files
-------------------------------------------------------------------------------}

writeJSON :: ToJSON a => FilePath -> a -> IO ()
writeJSON = Aeson.encodeFile

tryReadJSON :: FromJSON a => FilePath -> (a -> IO r) -> IO r -> IO r
tryReadJSON path kExists kOtherwise = do
    fileExists <- doesFileExist path
    if not fileExists then
      kOtherwise
    else do
      mDecoded <- Aeson.eitherDecodeFileStrict path
      case mDecoded of
        Left  _err    -> kOtherwise
        Right decoded -> kExists decoded

{-------------------------------------------------------------------------------
  Serialization
-------------------------------------------------------------------------------}

encodeText :: ToJSON a => a -> Text
encodeText = Text.decodeUtf8Lenient . BSL.toStrict . Aeson.encode

decodeText :: FromJSON a => Text -> Either String a
decodeText = Aeson.eitherDecode . BSL.fromStrict . Text.encodeUtf8