{-# LANGUAGE OverloadedStrings #-}

module ChallengeSet1 (main) where

import Data.ByteString (ByteString)
import Lib

main :: IO ()
main = do
    print $ hexToBase64 c1HexString
    print $ fixedXor c2HexString1 c2HexString2

c1HexString :: ByteString
c1HexString = "49276d206b696c6c696e6720796f757220627261696e206c696b65206120706f69736f6e6f7573206d757368726f6f6d"

c2HexString1 :: ByteString
c2HexString1 = "1c0111001f010100061a024b53535009181c"

c2HexString2 :: ByteString
c2HexString2 = "686974207468652062756c6c277320657965"
