{-# LANGUAGE CPP #-}

-- | Native token generation to replace SOUP dependencies.
-- Mitigates Hazard H-SYS-007
module Safety.Token (generateToken) where

import Control.Exception (bracket)
import Control.Monad (foldM)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import Foreign.C.Types (CChar)
import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Ptr (Ptr, castPtr, plusPtr)
import Safety.Result (SafetyResult (..))
import System.Posix.Files (getFdStatus, isCharacterDevice)
import System.Posix.IO (OpenFileFlags (..), OpenMode (ReadOnly), closeFd, defaultFileFlags, fdReadBuf, openFd)
import System.Posix.Types (Fd)
import Text.Printf (printf)

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

#if MIN_VERSION_unix(2,8,0)
openUrandomFd :: IO Fd
openUrandomFd = openFd "/dev/urandom" ReadOnly defaultFileFlags{creat=Nothing}
#else
openUrandomFd :: IO Fd
openUrandomFd = openFd "/dev/urandom" ReadOnly Nothing defaultFileFlags
#endif

generateToken :: IO (SafetyResult B.ByteString)
generateToken = do
  resBytes <- bracket openUrandomFd closeFd $ \fd -> do
    stat <- getFdStatus fd
    if not (isCharacterDevice stat)
      then return $ Unsafe "Security Violation - /dev/urandom is not a character device"
      else allocaBytes 16 $ \ptr -> do
        n <- readExact16 fd ptr
        if n /= 16
          then return $ Unsafe "Security Violation - insufficient entropy: read fewer than 16 bytes"
          else Safe <$> B.packCStringLen (castPtr ptr, 16)
  case resBytes of
    Safe tokenBytes -> return $ Safe (BC.pack (concatMap (printf "%02x") (B.unpack tokenBytes)))
    Unsafe msg -> return $ Unsafe msg
    ClampedToMin _ -> return $ Unsafe "Unexpected clamped result"
    ClampedToMax _ -> return $ Unsafe "Unexpected clamped result"
    DivByZeroSafe _ -> return $ Unsafe "Unexpected DivByZero result"
