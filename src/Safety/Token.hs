{-# LANGUAGE CPP #-}
-- | Native token generation to replace SOUP dependencies.
-- Mitigates Hazard H-SYS-007
module Safety.Token (generateToken) where

import Control.Exception (bracket)
import Control.Monad (foldM)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import System.Posix.IO (openFd, closeFd, OpenMode(ReadOnly), defaultFileFlags, fdReadBuf, OpenFileFlags(..))
import System.Posix.Files (getFdStatus, isCharacterDevice)
import System.Posix.Types (Fd)
import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Ptr (Ptr, castPtr, plusPtr)
import Foreign.C.Types (CChar)
import Text.Printf (printf)
import Safety.Result (SafetyResult(..))

-- | Read exactly 16 bytes into @buf@, retrying on short reads.
-- Returns the number of bytes accumulated.
readExact16 :: Fd -> Ptr CChar -> IO Int
readExact16 fd buf = fst <$> foldM step (0 :: Int, False) [1 .. (16 :: Int)]
  where
    step (n, done) _
      | done || n >= 16 = return (n, True)
      | otherwise = do
          r <- fdReadBuf fd (buf `plusPtr` n) (fromIntegral (16 - n))
          if r <= 0
              then return (n, True)
              else
                  let n' = n + fromIntegral r
                  in return (n', n' >= 16)

generateToken :: IO (SafetyResult B.ByteString)
generateToken = do
    resBytes <- bracket
#if MIN_VERSION_unix(2,8,0)
                    (openFd "/dev/urandom" ReadOnly defaultFileFlags{creat=Nothing})
#else
                    (openFd "/dev/urandom" ReadOnly Nothing defaultFileFlags)
#endif
                    closeFd $ \fd -> do
                        stat <- getFdStatus fd
                        if not (isCharacterDevice stat)
                            then return $ Unsafe "Security Violation - /dev/urandom is not a character device"
                            else allocaBytes 16 $ \ptr -> do
                                n <- readExact16 fd ptr
                                if n /= 16
                                    then return $ Unsafe "Security Violation - insufficient entropy: read fewer than 16 bytes"
                                    else Safe <$> B.packCStringLen (castPtr ptr, 16)
    return $ fmap (\tokenBytes -> BC.pack (concatMap (printf "%02x") (B.unpack tokenBytes))) resBytes
