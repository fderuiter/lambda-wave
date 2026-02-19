module Hardware.Control (configureSensor, parseConfig, configureRawSerial, setBeam, configureConfigSerial) where

import Control.Monad (forM_)
import Control.Concurrent (threadDelay)
import qualified Data.ByteString.Char8 as BC
import Control.Exception (try, IOException, bracket, evaluate, throwIO)
import Data.Char (isSpace)
import Data.List (dropWhileEnd)
import System.IO (withFile, hGetContents, IOMode(ReadMode))
import System.Posix.Terminal
import System.Posix.IO (openFd, closeFd, fdWriteBuf, OpenMode(ReadWrite), defaultFileFlags)
import System.Posix.Types (Fd(..))
import Foreign.Ptr (castPtr)
import Data.ByteString.Unsafe (unsafeUseAsCStringLen)
import Foreign.C.Types (CInt(..))
import Data.Config (uartBaudRate)

-- | External C function to configure serial port (supports 921600 baud)
foreign import ccall safe "configure_serial_port"
    c_configure_serial_port :: CInt -> CInt -> IO CInt

-- | Parses the configuration file content into a list of commands.
-- Ignores comments (starting with #) and empty lines.
--
-- >>> parseConfig "# Comment\ncmd 1\n  cmd 2  # comment\n\n"
-- ["cmd 1", "cmd 2"]
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
    -- Use withFile to ensure the handle is closed deterministically.
    fileContentResult <- try $ withFile configPath ReadMode $ \h -> do
        c <- hGetContents h
        _ <- evaluate (length c) -- Force strictness
        return c

    case fileContentResult of
        Left ex -> return $ Left $ "Failed to read config file: " ++ show (ex :: IOException)
        Right content -> do
            let commands = parseConfig content

            -- Wrap the whole operation in try to catch IOExceptions (e.g. port not found)
            result <- try $ bracket
#if MIN_VERSION_unix(2,8,0)
                (openFd portPath ReadWrite defaultFileFlags)
#else
                (openFd portPath ReadWrite Nothing defaultFileFlags)
#endif
                closeFd
                (\fd -> do
                    configureConfigSerial fd -- Set 115200
                    forM_ commands $ \cmd -> do
                        let packet = BC.pack (cmd ++ "\n")
                        bytesSent <- unsafeUseAsCStringLen packet $ \(ptr, len) ->
                            fdWriteBuf fd (castPtr ptr) (fromIntegral len)

                        -- Check if all bytes were written
                        if fromIntegral bytesSent < BC.length packet
                            then ioError (userError $ "Failed to send complete command: " ++ cmd)
                            else threadDelay 100000 -- 100ms delay between commands
                )

            case result of
                Left ex -> do
                    let msg = "[Control] Configuration Failed: " ++ show (ex :: IOException)
                    putStrLn msg
                    return (Left msg)
                Right _ -> do
                    putStrLn "[Control] Configuration Complete."
                    return (Right ())

configureConfigSerial :: Fd -> IO ()
configureConfigSerial fd = do
    attrs <- getTerminalAttributes fd
    let cfgAttrs = attrs
            `withInputSpeed` B115200 -- Standard speed
            `withOutputSpeed` B115200
    setTerminalAttributes fd cfgAttrs Immediately

-- | Configures a file descriptor for Raw Serial communication (Data Port).
-- Disables Canonical Mode (ICANON), Echo, Signals, and sets Baud Rate.
-- This is critical for receiving binary data from the radar.
--
-- Note: Uses FFI to 'serial_config.cpp' to support high baud rates (921600).
configureRawSerial :: Fd -> IO ()
configureRawSerial (Fd fd) = do
    -- We use the configured baud rate from Data.Config (921600)
    let baud = fromIntegral uartBaudRate :: CInt
    res <- c_configure_serial_port fd baud

    if res /= 0
        then throwIO (userError $ "Failed to configure serial port (C FFI returned " ++ show res ++ ")")
        else putStrLn $ "[Control] Data Port Configured (Raw Mode, " ++ show uartBaudRate ++ " baud)"

-- | Control the beam status (Simulated via GPIO).
-- True = Beam ON
-- False = Beam OFF
setBeam :: Bool -> IO ()
setBeam state = do
    -- In a real system, this would write to /sys/class/gpio or similar
    -- For Class C simulation, we log to stdout to verify behavior
    putStrLn $ "[Hardware] Beam Set To: " ++ if state then "ON" else "OFF"
