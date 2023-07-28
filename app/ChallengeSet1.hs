{-# LANGUAGE OverloadedStrings #-}

module ChallengeSet1 (main) where

import Data.ByteString (ByteString)
import Data.ByteString.Base16 qualified as B16
import Data.ByteString.Base64 qualified as B64

main :: IO ()
main = print (hexToBase64 c1HexString)

c1HexString :: ByteString
c1HexString = "49276d206b696c6c696e6720796f757220627261696e206c696b65206120706f69736f6e6f7573206d757368726f6f6d"

hexToBase64 :: ByteString -> ByteString
hexToBase64 hexByteString = case B16.decode hexByteString of
    Right decoded -> B64.encode decoded
    _ -> error "Invalid hex string"
