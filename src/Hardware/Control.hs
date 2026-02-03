{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE CPP #-}

module Hardware.Control
    ( configureSensor
    , parseConfig
    , configureRawSerial
    , getMonotonicTimeNS
    , getRealTimeNS
    ) where

import Control.Monad (forM_)
import Control.Concurrent (threadDelay)
import qualified Data.ByteString.Char8 as BC
import Control.Exception (try, IOException, bracket)
import Data.Char (isSpace)
import Data.List (dropWhileEnd)
import System.Posix.Terminal
import System.Posix.IO
import System.Posix.Files (ownerReadMode, ownerWriteMode, unionFileModes)
import System.Posix.Types (Fd(..))
import Data.ByteString (useAsCStringLen)
import Foreign.C.Types (CTime(..), CLong(..), CInt(..))
import Foreign.Ptr (Ptr, castPtr)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Storable (Storable(..))
import Data.Word (Word64)

-- | Parses the configuration file content into a list of commands.
parseConfig :: String -> [String]
parseConfig = filter (not . null) . map clean . lines
  where
    clean = trim . takeWhile (/= '#')
    trim = dropWhileEnd isSpace . dropWhile isSpace

-- | Configures the sensor by sending commands from the given config file.
configureSensor :: FilePath -> FilePath -> IO (Either String ())
configureSensor configPath portPath = do
    putStrLn $ "[Control] Configuring sensor on " ++ portPath ++ " with config " ++ configPath

    fileContentResult <- try $ readFile configPath
    case fileContentResult of
        Left ex -> return $ Left $ "Failed to read config file: " ++ show (ex :: IOException)
        Right content -> do
            let commands = parseConfig content

            -- Open Fd
            result <- try $ bracket
#if MIN_VERSION_unix(2,8,0)
                (openFd portPath ReadWrite defaultFileFlags { nonBlock = False, creat = Just (ownerReadMode `unionFileModes` ownerWriteMode) })
#else
                (openFd portPath ReadWrite (Just (ownerReadMode `unionFileModes` ownerWriteMode)) defaultFileFlags { nonBlock = False })
#endif
                closeFd
                (\fd -> do
                    -- Configure 115200 for CLI
                    configureCliSerial fd

                    forM_ commands $ \cmd -> do
                        let packet = BC.pack (cmd ++ "\n")
                        bytesSent <- useAsCStringLen packet $ \(ptr, len) ->
                            fdWriteBuf fd (castPtr ptr) (fromIntegral len)

                        -- Check if all bytes were written
                        if fromIntegral bytesSent < BC.length packet
                            then ioError (userError $ "Failed to send complete command: " ++ cmd)
                            else threadDelay 100000 -- 100ms delay
                )

            case result of
                Left ex -> do
                    let msg = "[Control] Configuration Failed: " ++ show (ex :: IOException)
                    putStrLn msg
                    return (Left msg)
                Right _ -> do
                    putStrLn "[Control] Configuration Complete."
                    return (Right ())

configureCliSerial :: Fd -> IO ()
configureCliSerial fd = do
    attrs <- getTerminalAttributes fd
    -- Set 115200, 8N1 is standard
    let newAttrs = attrs
            `withInputSpeed` B115200
            `withOutputSpeed` B115200
    setTerminalAttributes fd newAttrs Immediately

configureRawSerial :: Fd -> IO ()
configureRawSerial fd = do
    attrs <- getTerminalAttributes fd
    let rawAttrs = attrs
            `withoutMode` ProcessInput
            `withoutMode` EnableEcho
            `withoutMode` EchoLF
            `withoutMode` KeyboardInterrupts
            `withoutMode` ExtendedFunctions
            `withoutMode` MapCRtoLF
            `withoutMode` MapLFtoCR
            `withoutMode` StartStopOutput
            `withCC` (EndOfFile, '\1')
            `withCC` (EndOfLine, '\0')
            `withInputSpeed` B115200 -- B921600 missing in some unix versions, falling back to 115200 or assume config done externally
            `withOutputSpeed` B115200

    setTerminalAttributes fd rawAttrs Immediately
    putStrLn "[Control] Data Port Configured (Raw Mode, 115200 baud)"

--------------------------------------------------------------------------------
-- Time FFI
--------------------------------------------------------------------------------

data TimeSpec = TimeSpec { sec :: CTime, nsec :: CLong }

instance Storable TimeSpec where
    sizeOf _ = 16 -- struct timespec is usually 16 bytes (8+8) on 64-bit
    alignment _ = 8
    peek ptr = do
        s <- peekByteOff ptr 0
        ns <- peekByteOff ptr 8
        return $ TimeSpec s ns
    poke ptr (TimeSpec s ns) = do
        pokeByteOff ptr 0 s
        pokeByteOff ptr 8 ns

foreign import ccall unsafe "clock_gettime"
    c_clock_gettime :: CInt -> Ptr TimeSpec -> IO CInt

getMonotonicTimeNS :: IO Word64
getMonotonicTimeNS = alloca $ \ptr -> do
    _ <- c_clock_gettime 1 ptr -- CLOCK_MONOTONIC = 1
    TimeSpec (CTime s) (CLong ns) <- peek ptr
    return $ (fromIntegral s * 1_000_000_000) + fromIntegral ns

getRealTimeNS :: IO Word64
getRealTimeNS = alloca $ \ptr -> do
    _ <- c_clock_gettime 0 ptr -- CLOCK_REALTIME = 0
    TimeSpec (CTime s) (CLong ns) <- peek ptr
    return $ (fromIntegral s * 1_000_000_000) + fromIntegral ns
