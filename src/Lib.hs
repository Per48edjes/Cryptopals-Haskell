module Lib (module Lib) where

import Data.Bits (xor)
import Data.ByteString qualified as B
import Data.ByteString.Base16 qualified as B16
import Data.ByteString.Base64 qualified as B64

hexToBase64 :: B.ByteString -> Either String B.ByteString
hexToBase64 hexByteString = B64.encode <$> B16.decode hexByteString

applyXor :: B.ByteString -> B.ByteString -> Either String B.ByteString
applyXor hexByteString hexKey =
    B16.encode <$> do
        decodedBs <- B16.decode hexByteString
        decodedKey <- B16.decode hexKey
        return $ B.pack $ zipWith xor (B.unpack decodedBs) (cycle $ B.unpack decodedKey)

-- Function that scores plaintext
--
