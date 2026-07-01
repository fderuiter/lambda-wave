{-# LANGUAGE CPP #-}
-- | Native token generation to replace SOUP dependencies.
-- Mitigates Hazard H-SYS-007
module Safety.Token (generateToken) where

import Control.Exception (bracket)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import System.Posix.IO (openFd, closeFd, OpenMode(ReadOnly), defaultFileFlags, OpenFileFlags(..), fdReadBuf)
import System.Posix.Files (getFdStatus, isCharacterDevice)
import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Ptr (castPtr)
import Text.Printf (printf)

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
                                bytesRead <- fdReadBuf fd ptr 16
                                B.packCStringLen (castPtr ptr, fromIntegral bytesRead)
    return $ BC.pack (concatMap (printf "%02x") (B.unpack tokenBytes))
