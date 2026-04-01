{-# LANGUAGE CPP #-}
module Hardware.Control (
    configureSensor,
    configureSensorWithRetry,
    parseConfig,
    configureRawSerial,
    setBeam,
    configureConfigSerial
) where

import Control.Monad (forM_)
import Control.Concurrent (threadDelay)
import qualified Data.ByteString.Char8 as BC
import Control.Exception (try, IOException, bracket)
import Data.Char (isSpace)
import Data.List (dropWhileEnd)
import System.IO (withFile, IOMode(ReadMode))
import System.Posix.Terminal
import System.Posix.IO (openFd, closeFd, fdWriteBuf, OpenMode(ReadWrite), defaultFileFlags)
import System.Posix.Types (Fd(..))
import Foreign.Ptr (castPtr)
import Data.ByteString (useAsCStringLen)
import Foreign.C.Types (CInt(..))
import Data.Config (uartBaudRate)
import qualified Data.ByteString as B
import System.FilePath (isAbsolute, splitDirectories)

import Hardware.Types (HardwareError(..))

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

-- | Attempts to configure the sensor with automatic retries on failure.
-- Retries 'attempts' times with a 100ms delay between attempts.
configureSensorWithRetry :: Int -> FilePath -> FilePath -> IO (Either HardwareError ())
configureSensorWithRetry attempts configPath portPath = go attempts
  where
    go n
      | n <= 0 = return $ Left $ ConfigurationFailed "Max retries exceeded"
      | otherwise = do
          res <- configureSensor configPath portPath
          case res of
            Right () -> return $ Right ()
            Left _ -> do
                putStrLn $ "[Control] Retrying configuration (" ++ show (attempts - n + 1) ++ "/" ++ show attempts ++ ")..."
                threadDelay 100000 -- 100ms wait
                go (n - 1)

-- | Helper to prevent path traversal
isPathSafe :: FilePath -> Bool
isPathSafe path = not (isAbsolute path) && ".." `notElem` splitDirectories path

-- | Configures the sensor by sending commands from the given config file.
-- Returns typed 'HardwareError' on failure.
-- SENTINEL SAFETY EDIT: Uses bounded strict IO to prevent DoS via massive config files.
-- SENTINEL SAFETY EDIT: Added path traversal protection for config file.
configureSensor :: FilePath -> FilePath -> IO (Either HardwareError ())
configureSensor configPath portPath = do
    if not (isPathSafe configPath)
        then return $ Left $ ConfigurationFailed "Unsafe configuration path detected"
        else do
            putStrLn $ "[Control] Configuring sensor on " ++ portPath ++ " with config " ++ configPath

            -- Read config file (Bounded, Strict)
            -- Limit to 100KB to prevent OOM/DoS
            let maxConfigSize = 100 * 1024

            fileContentResult <- try $ withFile configPath ReadMode $ \h -> do
                bs <- B.hGet h maxConfigSize
                return (BC.unpack bs)

            case fileContentResult of
                Left ex -> return $ Left $ ConfigurationFailed $ "Failed to read config file: " ++ show (ex :: IOException)
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
                            res <- configureConfigSerial fd -- Set 115200
                            case res of
                                Left (ConfigurationFailed err) -> ioError (userError err)
                                Left err -> ioError (userError $ show err)
                                Right () -> do
                                    forM_ commands $ \cmd -> do
                                        let packet = BC.pack (cmd ++ "\n")
                                        bytesSent <- useAsCStringLen packet $ \(ptr, len) ->
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
                            return (Left $ ConfigurationFailed msg)
                        Right _ -> do
                            putStrLn "[Control] Configuration Complete."
                            return (Right ())

configureConfigSerial :: Fd -> IO (Either HardwareError ())
configureConfigSerial fd = do
    result <- try $ do
        attrs <- getTerminalAttributes fd
        let cfgAttrs = attrs
                `withInputSpeed` B115200 -- Standard speed
                `withOutputSpeed` B115200
        setTerminalAttributes fd cfgAttrs Immediately
        return ()

    case result of
        Left ex -> return $ Left $ ConfigurationFailed $ "Failed to configure config serial: " ++ show (ex :: IOException)
        Right () -> return $ Right ()

-- | Configures a file descriptor for Raw Serial communication (Data Port).
-- Disables Canonical Mode (ICANON), Echo, Signals, and sets Baud Rate.
-- This is critical for receiving binary data from the radar.
--
-- Note: Uses FFI to 'serial_config.cpp' to support high baud rates (921600).
-- Returns 'Left ConfigurationFailed' if the C FFI call fails.
configureRawSerial :: Fd -> IO (Either HardwareError ())
configureRawSerial (Fd fd) = do
    -- We use the configured baud rate from Data.Config (921600)
    let baud = fromIntegral uartBaudRate :: CInt
    res <- c_configure_serial_port fd baud

    if res /= 0
        then return $ Left $ ConfigurationFailed ("Failed to configure serial port (C FFI returned " ++ show res ++ ")")
        else do
            putStrLn $ "[Control] Data Port Configured (Raw Mode, " ++ show uartBaudRate ++ " baud)"
            return $ Right ()

-- | Control the beam status (Simulated via GPIO).
-- True = Beam ON
-- False = Beam OFF
setBeam :: Bool -> IO ()
setBeam state = do
    -- In a real system, this would write to /sys/class/gpio or similar
    -- For Class C simulation, we log to stdout to verify behavior
    putStrLn $ "[Hardware] Beam Set To: " ++ if state then "ON" else "OFF"
