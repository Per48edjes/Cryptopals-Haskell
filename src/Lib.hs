{-# LANGUAGE DeriveGeneric #-}

module Lib (module Lib) where

import Data.Bits (xor)
import Data.ByteString qualified as B
import Data.ByteString.Base64 qualified as B64
import Data.ByteString.Lazy qualified as BL
import Data.Csv qualified as CSV
import Data.Csv.Incremental
import Data.Map qualified as M
import Data.Text.Encoding
import Data.Vector qualified as V
import Data.Word
import GHC.Generics
import Text.Hex

-- Convert a hex string to base64
hexToBase64 :: Text -> Maybe B.ByteString
hexToBase64 hexString = B64.encode <$> decodeHex hexString

-- Apply XOR to two hex strings, the second of which is the key to be repeated
applyXor :: Text -> Text -> Maybe B.ByteString
applyXor hexString hexKey = do
    decodedBs <- decodeHex hexString
    decodedKey <- decodeHex hexKey
    return $ B.pack $ zipWith xor (B.unpack decodedBs) (cycle $ B.unpack decodedKey)

data CharFreq = CharFreq {character :: !Text, frequency :: !Double}
    deriving (Generic, Show)

instance CSV.FromRecord CharFreq

-- Read character frequencies (English) from a file
getCharFreqs :: FilePath -> IO (M.Map Word8 Double)
getCharFreqs filepath = do
    contents <- BL.readFile filepath
    case CSV.decode NoHeader contents of
        Left err -> error err
        Right records -> return $ vectorToMap records
  where
    vectorToMap :: V.Vector CharFreq -> M.Map Word8 Double
    vectorToMap = M.fromList . map (\(CharFreq c f) -> (B.head $ encodeUtf8 c, f)) . V.toList

-- Score a string based on comprising characters' frequencies (English)
charFreqScore :: M.Map Word8 Double -> B.ByteString -> Double
charFreqScore charFreqs = B.foldl' runScore 0.0
  where
    runScore :: Double -> Word8 -> Double
    runScore acc c = acc + M.findWithDefault 0.0 c charFreqs
