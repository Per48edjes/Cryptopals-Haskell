module Lib (module Lib) where

import Data.Bits (xor)
import Data.ByteString (ByteString, pack, unpack)
import Data.ByteString.Base16 qualified as B16
import Data.ByteString.Base64 qualified as B64

hexToBase64 :: ByteString -> Either String ByteString
hexToBase64 hexByteString = B64.encode <$> B16.decode hexByteString

fixedXor :: ByteString -> ByteString -> Either String ByteString
fixedXor bs1 bs2 =
    B16.encode <$> do
        decoded1 <- B16.decode bs1
        decoded2 <- B16.decode bs2
        return $ pack $ zipWith xor (unpack decoded1) (unpack decoded2)
