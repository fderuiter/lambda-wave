{-# LANGUAGE OverloadedStrings #-}
module Safety.Crypto (encryptLog, decryptLog, encryptWebsocket, decryptWebsocket) where

import Crypto.Cipher.AES (AES256)
import Crypto.Cipher.Types (BlockCipher(..), Cipher(..), nullIV)
import Crypto.Error (CryptoFailable(..))
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC

key :: B.ByteString
key = BC.pack "01234567890123456789012345678901"

processBytes :: B.ByteString -> B.ByteString
processBytes pt = 
    case cipherInit key :: CryptoFailable AES256 of
        CryptoPassed c -> ctrCombine c nullIV pt
        _ -> pt

encryptLog :: String -> B.ByteString
encryptLog str = processBytes (BC.pack str)

decryptLog :: B.ByteString -> String
decryptLog bs = BC.unpack (processBytes bs)

encryptWebsocket :: B.ByteString -> B.ByteString
encryptWebsocket = processBytes

decryptWebsocket :: B.ByteString -> B.ByteString
decryptWebsocket = processBytes
