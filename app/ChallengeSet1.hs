{-# LANGUAGE OverloadedStrings #-}

module ChallengeSet1 (main) where

import Data.ByteString qualified as B (ByteString)
import Data.ByteString.Base16 qualified as B16
import Data.Serialize qualified as S
import Data.Word (Word8)
import Lib

main :: IO ()
main = do
    print $ hexToBase64 c1HexString
    print $ applyXor c2HexString1 c2HexString2
    print $ map (applyXor c3HexString) c3CandidateKeys

c1HexString :: B.ByteString
c1HexString = "49276d206b696c6c696e6720796f757220627261696e206c696b65206120706f69736f6e6f7573206d757368726f6f6d"

c2HexString1 :: B.ByteString
c2HexString1 = "1c0111001f010100061a024b53535009181c"

c2HexString2 :: B.ByteString
c2HexString2 = "686974207468652062756c6c277320657965"

c3HexString :: B.ByteString
c3HexString = "1b37373331363f78151b7f2b783431333d78397828372d363c78373e783a393b3736"

c3CandidateKeys :: [B.ByteString]
c3CandidateKeys = map (B16.encode . S.encode) ([0 .. 255] :: [Word8])
