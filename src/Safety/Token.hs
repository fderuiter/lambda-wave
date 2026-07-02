{-# LANGUAGE CPP #-}
-- | Native token generation to replace SOUP dependencies.
-- Mitigates Hazard H-SYS-007
module Safety.Token (generateToken) where

import Control.Exception (bracket)
import Control.Monad (foldM)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import System.Posix.IO (openFd, closeFd, OpenMode(ReadOnly), defaultFileFlags, OpenFileFlags(..), fdReadBuf)
import System.Posix.Files (getFdStatus, isCharacterDevice)
import System.Posix.Types (Fd)
import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Ptr (castPtr, plusPtr)
import Foreign.C.Types (CChar)
import Text.Printf (printf)

-- | Read exactly @total@ bytes into @buf@, retrying on short reads.
-- Performs at most @total@ iterations (one per byte), satisfying the fixed-loop
-- requirement. Returns the total bytes accumulated; a value less than @total@
-- indicates an unexpected end-of-file or device error.
readExact :: Fd -> Ptr CChar -> Int -> IO Int
readExact fd buf total = foldM step 0 [1..total]
  where
    step n _
      | n >= total = return n
      | otherwise  = do
          r <- fdReadBuf fd (buf `plusPtr` n) (fromIntegral (total - n))
          return (n + fromIntegral r)

generateToken :: IO B.ByteString
generateToken = do
    tokenBytes <- bracket
#if MIN_VERSION_unix(2,8,0)
                    (openFd "/dev/urandom" ReadOnly defaultFileFlags{creat=Nothing})
#else
                    (openFd "/dev/urandom" ReadOnly Nothing defaultFileFlags)
#endif
                    closeFd $ \fd -> do
                        stat <- getFdStatus fd
                        if not (isCharacterDevice stat)
                            then error "Security Violation - /dev/urandom is not a character device"
                            else allocaBytes 16 $ \ptr -> do
                                n <- readExact fd ptr 16
                                if n /= 16
                                    then error "Security Violation - insufficient entropy: read fewer than 16 bytes"
                                    else B.packCStringLen (castPtr ptr, 16)
    return $ BC.pack (concatMap (printf "%02x") (B.unpack tokenBytes))
