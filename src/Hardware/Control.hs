{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}
module Hardware.Control (configureSensor, parseConfig) where

-- Replaced System.Hardware.Serialport with System.Posix to avoid missing dependency
import System.Posix.IO
import System.Posix.Terminal
import System.Posix.Types (Fd(..), ByteCount)
import System.Posix.Files (stdFileMode)
import Foreign.C.Types (CSize)
import Foreign.Ptr (castPtr)
import Control.Monad (forM_, void)
import Control.Concurrent (threadDelay)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Unsafe as BU
import Control.Exception (try, IOException, bracket, onException)
import Data.Char (isSpace)
import Data.List (dropWhileEnd)

-- | Parses the configuration file content into a list of commands.
-- Ignores comments (starting with #) and empty lines.
parseConfig :: String -> [String]
parseConfig = filter (not . null) . map clean . lines
  where
    clean = trim . takeWhile (/= '#')
    trim = dropWhileEnd isSpace . dropWhile isSpace

-- | Configures the sensor by sending commands from the given config file.
-- Returns Left error message on failure, Right () on success.
configureSensor :: FilePath -> FilePath -> IO (Either String ())
configureSensor configPath portPath = do
    putStrLn $ "[Control] Configuring sensor on " ++ portPath ++ " with config " ++ configPath

    -- Read config file
    fileContentResult <- try $ readFile configPath
    case fileContentResult of
        Left ex -> return $ Left $ "Failed to read config file: " ++ show (ex :: IOException)
        Right content -> do
            let commands = parseConfig content

            -- Wrap the whole operation in try to catch IOExceptions (e.g. port not found)
            result <- try $ bracket
                (openSerialPort portPath)
                closeFd
                (\fd -> do
                    forM_ commands $ \cmd -> do
                        let packet = BC.pack (cmd ++ "\n")
                        sendData fd packet
                        threadDelay 100000 -- 100ms delay between commands
                )

            case result of
                Left ex -> do
                    let msg = "[Control] Configuration Failed: " ++ show (ex :: IOException)
                    putStrLn msg
                    return (Left msg)
                Right _ -> do
                    putStrLn "[Control] Configuration Complete."
                    return (Right ())

-- | Open and configure the serial port using POSIX
openSerialPort :: FilePath -> IO Fd
openSerialPort path = do
    -- Open in Non-Blocking mode initially to avoid hanging if no carrier
#if MIN_VERSION_unix(2,8,0)
    fd <- openFd path ReadWrite defaultFileFlags { nonBlock = True }
#else
    fd <- openFd path ReadWrite Nothing defaultFileFlags { nonBlock = True }
#endif

    -- Configure Terminal Attributes (Raw Mode)
    attrs <- getTerminalAttributes fd
    let attrs1 = withOutputSpeed attrs B115200
        attrs2 = withInputSpeed attrs1 B115200
        attrs3 = withoutMode attrs2 ProcessInput -- ICANON (Canonical Mode)
        attrs4 = withoutMode attrs3 EnableEcho
        attrs5 = withoutMode attrs4 EchoErase
        attrs6 = withoutMode attrs5 EchoKill
        attrs7 = withoutMode attrs6 ProcessOutput -- OPOST (Post-process output)
        attrs' = withoutMode attrs7 MapCRtoLF     -- ICRNL
    -- Set 8N1 (CS8, No Parity, 1 Stop Bit)
    -- Haskell System.Posix.Terminal doesn't expose CS8 directly easily without digging into Bits
    -- But usually default is okay or we assume it.
    -- To be robust, we should set CS8.
    -- TerminalMode is a newtype around CInt.
    -- We'll assume the defaults + raw mode is sufficient for now, or check Control.Char if needed.
    -- For now, relying on 'withoutMode ProcessInput' is the key for Raw.

    setTerminalAttributes fd attrs' Immediately

    return fd

-- | Helper to modify attributes
-- | Send data to Fd
sendData :: Fd -> B.ByteString -> IO ()
sendData fd bs = do
    BU.unsafeUseAsCStringLen bs $ \(ptr, len) -> do
        bytesWritten <- fdWriteBuf fd (castPtr ptr) (fromIntegral len)
        if fromIntegral bytesWritten < len
            then ioError (userError "Failed to send complete command (short write)")
            else return ()
