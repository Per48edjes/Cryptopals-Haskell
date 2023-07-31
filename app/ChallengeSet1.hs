{-# LANGUAGE OverloadedStrings #-}

module ChallengeSet1 (main) where

import Data.ByteString qualified as B
import Data.Function (on)
import Data.List qualified as L
import Data.Maybe
import Data.Word (Word8)
import Lib
import Text.Hex

main :: IO ()
main = do
    print challenge1
    print challenge2
    challenge3 <- challenge3IO
    print challenge3

challenge1 :: B.ByteString
challenge1 = fromJust $ hexToBase64 c1HexString
  where
    c1HexString = "49276d206b696c6c696e6720796f757220627261696e206c696b65206120706f69736f6e6f7573206d757368726f6f6d"

challenge2 :: B.ByteString
challenge2 = fromJust $ applyXor c2HexString1 c2HexString2
  where
    c2HexString1 = "1c0111001f010100061a024b53535009181c"
    c2HexString2 = "686974207468652062756c6c277320657965"

challenge3IO :: IO B.ByteString
challenge3IO = do
    charFreqs <- getCharFreqs "data/char-freqs.csv"
    let englishScore = charFreqScore charFreqs
     in return $ L.maximumBy (compare `on` englishScore) c3CandidatePlaintexts
  where
    c3HexString = "1b37373331363f78151b7f2b783431333d78397828372d363c78373e783a393b3736"
    c3CandidateKeys :: [Text]
    c3CandidateKeys = map (encodeHex . B.singleton) ([0 .. 255] :: [Word8])
    c3CandidatePlaintexts :: [B.ByteString]
    c3CandidatePlaintexts = mapMaybe (applyXor c3HexString) c3CandidateKeys
