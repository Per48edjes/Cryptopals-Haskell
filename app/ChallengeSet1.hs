module ChallengeSet1 (main) where

import Data.ByteString qualified as B
import Data.Map qualified as M
import Data.Maybe
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Data.Word (Word8)
import Lib

main :: IO ()
main = do
    print challenge1
    print challenge2
    englishCharFreqs <- getCharFreqs "data/char-freqs.csv"
    print $ challenge3 englishCharFreqs
    c4HexStrings <- T.lines <$> TIO.readFile "data/cs1_c4.txt"
    print $ challenge4 englishCharFreqs c4HexStrings

challenge1 :: B.ByteString
challenge1 = fromJust $ hexToBase64 c1HexString
  where
    c1HexString = "49276d206b696c6c696e6720796f757220627261696e206c696b65206120706f69736f6e6f7573206d757368726f6f6d"

challenge2 :: B.ByteString
challenge2 = fromJust $ applyXor c2HexString1 c2HexString2
  where
    c2HexString1 = "1c0111001f010100061a024b53535009181c"
    c2HexString2 = "686974207468652062756c6c277320657965"

challenge3 :: M.Map Word8 Double -> B.ByteString
challenge3 charFreqs = applyCharFreq charFreqs c3HexString
  where
    c3HexString = "1b37373331363f78151b7f2b783431333d78397828372d363c78373e783a393b3736"

challenge4 :: M.Map Word8 Double -> [T.Text] -> B.ByteString
challenge4 charFreqs hexStrings = charFreqScorer charFreqs candidatePlaintexts
  where
    candidatePlaintexts = map (applyCharFreq charFreqs) hexStrings
