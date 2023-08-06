{-# LANGUAGE DeriveGeneric #-}

module Lib (module Lib) where

import Data.Bits (popCount, xor)
import Data.ByteString qualified as B
import Data.ByteString.Base64 qualified as B64
import Data.ByteString.Lazy qualified as BL
import Data.Csv qualified as CSV
import Data.Csv.Incremental
import Data.Function (on)
import Data.List qualified as L
import Data.List.Split (chunksOf)
import Data.Map.Strict qualified as M
import Data.Maybe
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Data.Vector qualified as V
import Data.Word
import GHC.Generics
import Text.Hex

-- Convert a hex string to base64 encoded bytestring
hexToBase64 :: Text -> Maybe B.ByteString
hexToBase64 hexString = B64.encode <$> decodeHex hexString

-- Convert a base64 string to hex string
base64ToHex :: Text -> Maybe Text
base64ToHex base64String =
    encodeHex <$> case B64.decode (T.encodeUtf8 base64String) of
        Left _ -> Nothing
        Right bs -> Just bs

-- Apply XOR to two hex strings, the second of which is the key to be repeated
decryptXor :: Text -> Text -> Maybe B.ByteString
decryptXor hexString hexKey = do
    decodedBs <- decodeHex hexString
    decodedKey <- decodeHex hexKey
    return $ B.pack $ zipWith xor (B.unpack decodedBs) (cycle $ B.unpack decodedKey)

-- Apply XOR to two plaintext strings, the second of which is the key to be repeated
encryptXor :: B.ByteString -> B.ByteString -> Maybe Text
encryptXor plainTextString plainTextKey =
    let encodedBs = encodeHex plainTextString
        encodedKey = encodeHex plainTextKey
     in encodeHex <$> decryptXor encodedBs encodedKey

data CharFreq = CharFreq {character :: !Text, frequency :: !Double}
    deriving (Generic, Show)

instance CSV.FromRecord CharFreq

-- Read character frequencies from a file
getCharFreqs :: FilePath -> IO (M.Map Word8 Double)
getCharFreqs filepath = do
    contents <- BL.readFile filepath
    case CSV.decode NoHeader contents of
        Left err -> error err
        Right records -> return $ vectorToMap records
  where
    vectorToMap :: V.Vector CharFreq -> M.Map Word8 Double
    vectorToMap = M.fromList . map (\(CharFreq c f) -> (B.head $ T.encodeUtf8 c, f)) . V.toList

-- Score strings based on comprising characters' frequencies
charFreqScorer :: M.Map Word8 Double -> B.ByteString -> Double
charFreqScorer charFreqs = B.foldl' runScore 0.0
  where
    runScore :: Double -> Word8 -> Double
    runScore acc c = acc + M.findWithDefault 0.0 c charFreqs

-- Bruteforce cipher with all 8-byte character keys and try scoring resultant plaintext based on character frequency
applyCharFreq :: M.Map Word8 Double -> Text -> (Text, B.ByteString)
applyCharFreq charFreqs hexString = L.maximumBy (compare `on` (charFreqScorer charFreqs . snd)) candidates
  where
    candidateKeys :: [Text]
    candidateKeys = map (encodeHex . B.singleton) ([0 .. 255] :: [Word8])
    candidatePlaintexts :: [B.ByteString]
    candidatePlaintexts = mapMaybe (decryptXor hexString) candidateKeys
    candidates = zip candidateKeys candidatePlaintexts

-- Calculate the Hamming distance between two equal-length lists
hammingDistance :: B.ByteString -> B.ByteString -> Int
hammingDistance bs1 bs2 = sum $ map popCount $ B.zipWith xor bs1 bs2

-- Take two consecutive 4 * keysize-length chunks of a bytestring and calculate the normalized Hamming distance between them
testKeysize :: B.ByteString -> Int -> Double
testKeysize bs keySize = fromIntegral (hammingDistance block1 block2) / fromIntegral keySize
  where
    block1 = B.take (4 * keySize) bs
    block2 = B.take (4 * keySize) $ B.drop (4 * keySize) bs

-- Find the most likely keysize for a given bytestring ciphertext by testing all keysize candidates and returning the one with the lowest normalized Hamming distance
bestKeysize :: B.ByteString -> Int
bestKeysize bs = fst $ L.minimumBy (compare `on` snd) $ zip candidateKeySizes $ map (testKeysize bs) candidateKeySizes
  where
    candidateKeySizes = [2 .. 40]

-- Break bytestring into chunks of a given number of bytes
chunkIntoKeysizeBlocks :: B.ByteString -> Int -> [B.ByteString]
chunkIntoKeysizeBlocks bs keySize = B.pack <$> chunksOf keySize (B.unpack bs)

-- Convert ciphertext into a list of transposed hex-encoded keysize-length blocks
transposedKeysizedBlocks :: T.Text -> [T.Text]
transposedKeysizedBlocks hexString =
    let decodedBs = fromJust $ decodeHex hexString
        candidateKeySize = bestKeysize decodedBs
     in encodeHex <$> B.transpose (chunkIntoKeysizeBlocks decodedBs candidateKeySize)
