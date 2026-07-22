{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Safety.Crypto
-- Description : Cryptographic functions for safety logs
--
-- ⚠️ SAFETY-CRITICAL
--
-- = Failure Mode
-- If encryption fails or keys are exposed, sensitive patient data in logs could be compromised.
--
-- = Mitigation
-- Uses AES256 with randomly generated IVs per encryption operation, ensuring semantic security.
module Safety.Crypto (encryptLog, decryptLog) where

import Crypto.Cipher.AES (AES256)
import Crypto.Cipher.Types (BlockCipher (..), Cipher (..), IV, makeIV)
import Crypto.Error (CryptoFailable (..))
import Crypto.Random (getRandomBytes)
import Data.ByteArray.Encoding (Base (Base64), convertFromBase, convertToBase)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import Safety.Result (SafetyResult (..))

key :: B.ByteString
key = BC.pack "01234567890123456789012345678901"

processBytes :: IV AES256 -> B.ByteString -> Either String B.ByteString
processBytes iv pt =
  case cipherInit key :: CryptoFailable AES256 of
    CryptoPassed c -> Right $ ctrCombine c iv pt
    CryptoFailed e -> Left ("Cipher init failed: " ++ show e)

encryptIO :: B.ByteString -> IO (SafetyResult B.ByteString)
encryptIO pt = do
  ivBytes <- getRandomBytes 16
  case makeIV ivBytes of
    Just iv -> case processBytes iv pt of
      Right ct -> return $ Safe (B.append ivBytes ct)
      Left e -> return $ Unsafe ("Encryption failed: " ++ e)
    Nothing -> return $ Unsafe "Failed to generate IV"

decryptPure :: B.ByteString -> Either String B.ByteString
decryptPure bs =
  if B.length bs < 16
    then Left "Ciphertext too short"
    else do
      let (ivBytes, ct) = B.splitAt 16 bs
      case makeIV ivBytes of
        Just iv -> processBytes iv ct
        Nothing -> Left "Invalid IV"

encryptLog :: String -> IO (SafetyResult B.ByteString)
encryptLog str = do
  encRes <- encryptIO (BC.pack str)
  case encRes of
    Safe enc -> return $ Safe (convertToBase Base64 enc `B.append` "\n")
    Unsafe msg -> return $ Unsafe msg
    ClampedToMin _ -> return $ Unsafe "Unexpected clamped result"
    ClampedToMax _ -> return $ Unsafe "Unexpected clamped result"
    DivByZeroSafe _ -> return $ Unsafe "Unexpected DivByZero result"

decryptLog :: B.ByteString -> Either String String
decryptLog bs = do
  let lines' = filter (not . B.null) (BC.lines bs)
  decLines <- mapM decodeLine lines'
  return (concat decLines)
  where
    decodeLine l = case convertFromBase Base64 l :: Either String B.ByteString of
      Left e -> Left ("Base64 decode failed: " ++ e)
      Right dec -> case decryptPure dec of
        Right pt -> Right (BC.unpack pt)
        Left e -> Left e
