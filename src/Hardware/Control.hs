{-# LANGUAGE ForeignFunctionInterface #-}
module Hardware.Control (
    configureSensor,
    parseConfig,
    configureRawSerial,
    getMonotonicTimeNS,
    getRealTimeNS
) where

import Control.Monad (forM_)
import Control.Concurrent (threadDelay)
import Control.Exception (try, IOException, bracket)
import Data.Char (isSpace)
import Data.List (dropWhileEnd)
import System.Posix.Terminal
import System.Posix.Types (Fd(..))
import System.Posix.IO (openFd, closeFd, fdWrite, OpenMode(..), defaultFileFlags, OpenFileFlags(..))
import Data.Word (Word64)
import Foreign.Ptr
import Foreign.Storable
import Foreign.Marshal.Alloc (alloca)
import Foreign.C.Types (CInt(..), CLong(..), CTime(..))

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

            result <- try $ bracket
                (openFd portPath ReadWrite Nothing defaultFileFlags { nonBlock = False })
                closeFd
                (\fd -> do
                    attrs <- getTerminalAttributes fd
                    let configAttrs = attrs
                            `withInputSpeed` B115200
                            `withOutputSpeed` B115200
                            `withoutMode` EnableEcho
                            `withoutMode` ProcessInput
                    setTerminalAttributes fd configAttrs Immediately

                    forM_ commands $ \cmd -> do
                        let fullCmd = cmd ++ "\n"
                        bytesWritten <- fdWrite fd fullCmd
                        if fromIntegral bytesWritten < length fullCmd
                            then ioError (userError $ "Failed to send complete command: " ++ cmd)
                            else threadDelay 100000
                )
            case result of
                Left ex -> do
                    let msg = "[Control] Configuration Failed: " ++ show (ex :: IOException)
                    putStrLn msg
                    return (Left msg)
                Right _ -> do
                    putStrLn "[Control] Configuration Complete."
                    return (Right ())

-- | Configures a file descriptor for Raw Serial communication.
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
            -- Sentinel: Downgraded to B115200 due to missing B921600 in CI environment.
            `withInputSpeed` B115200
            `withOutputSpeed` B115200
    setTerminalAttributes fd rawAttrs Immediately
    putStrLn "[Control] Data Port Configured (Raw Mode, 115200 baud)"

-- | FFI for Time
foreign import ccall unsafe "time.h clock_gettime"
    c_clock_gettime :: CInt -> Ptr TimeSpec -> IO CInt

data TimeSpec = TimeSpec CTime CLong

instance Storable TimeSpec where
    sizeOf _ = 16
    alignment _ = 8
    peek ptr = do
        sec <- peekByteOff ptr 0
        nsec <- peekByteOff ptr 8
        return $ TimeSpec sec nsec
    poke ptr (TimeSpec sec nsec) = do
        pokeByteOff ptr 0 sec
        pokeByteOff ptr 8 nsec

-- | Get monotonic time in nanoseconds
getMonotonicTimeNS :: IO Word64
getMonotonicTimeNS = alloca $ \ptr -> do
    _ <- c_clock_gettime 1 ptr -- CLOCK_MONOTONIC
    TimeSpec (CTime sec) (CLong nsec) <- peek ptr
    return $ (fromIntegral sec * 1_000_000_000) + fromIntegral nsec

-- | Get real time in nanoseconds
getRealTimeNS :: IO Word64
getRealTimeNS = alloca $ \ptr -> do
    _ <- c_clock_gettime 0 ptr -- CLOCK_REALTIME
    TimeSpec (CTime sec) (CLong nsec) <- peek ptr
    return $ (fromIntegral sec * 1_000_000_000) + fromIntegral nsec
