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
import Control.Exception (try, IOException, bracket, evaluate)
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
import System.IO.Error (isDoesNotExistError, isPermissionError, isAlreadyInUseError)

import Hardware.Types (HardwareError(..), isTransient, toSeverity, logMessage)
import Data.Types (Severity(..))

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
-- Retries 'attempts' times with a 100ms delay between attempts for transient errors.
--
-- Complexity: O(N) where N is attempts (due to retries)
configureSensorWithRetry :: (Severity -> String -> IO ()) -> Int -> FilePath -> FilePath -> IO (Either HardwareError ())
configureSensorWithRetry logger attempts configPath portPath = go attempts
  where
    go n
      | n <= 0 = do
          logger Critical "Max retries exceeded for sensor configuration"
          return $ Left $ ConfigurationFailed "Max retries exceeded"
      | otherwise = do
          res <- configureSensor logger configPath portPath
          case res of
            Right () -> return $ Right ()
            Left err -> do
                if isTransient err
                   then do
                       logger Warning $ "[Control] Retrying configuration (" ++ show (attempts - n + 1) ++ "/" ++ show attempts ++ ") due to: " ++ logMessage err
                       threadDelay 100000 -- 100ms wait
                       go (n - 1)
                   else do
                       logger Critical $ "[Control] Permanent configuration failure: " ++ logMessage err
                       return $ Left err

-- | Configures the sensor by sending commands from the given config file.
-- Returns typed 'HardwareError' on failure.
--
-- Complexity: O(L) where L is length of config file (parsing) + O(C) serial overhead
configureSensor :: (Severity -> String -> IO ()) -> FilePath -> FilePath -> IO (Either HardwareError ())
configureSensor logger configPath portPath = do
    logger Info $ "[Control] Configuring sensor on " ++ portPath ++ " with config " ++ configPath

    -- Read config file
    -- Use withFile to ensure the handle is closed deterministically.
    fileContentResult <- try $ withFile configPath ReadMode $ \h -> do
        c <- hGetContents h
        _ <- evaluate (length c) -- Force strictness
        return c

    case fileContentResult of
        Left ex -> do
            let err = mapIOException ex
            logger (toSeverity err) $ logMessage err
            return $ Left err
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
                                bytesSent <- unsafeUseAsCStringLen packet $ \(ptr, len) ->
                                    fdWriteBuf fd (castPtr ptr) (fromIntegral len)

                                -- Check if all bytes were written
                                if fromIntegral bytesSent < BC.length packet
                                    then ioError (userError $ "Failed to send complete command: " ++ cmd)
                                    else threadDelay 100000 -- 100ms delay between commands
                )

            case result of
                Left ex -> do
                    let err = mapIOException ex
                    logger (toSeverity err) $ "[Control] Configuration Failed: " ++ logMessage err
                    return (Left err)
                Right _ -> do
                    logger Info "[Control] Configuration Complete."
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

-- | Maps IOExceptions to typed HardwareErrors
--
-- Complexity: O(1)
mapIOException :: IOException -> HardwareError
mapIOException ex
    | isDoesNotExistError ex = FileError (show ex)
    | isPermissionError ex   = DeviceBusy
    | isAlreadyInUseError ex = DeviceBusy
    | otherwise              = UnknownError (show ex)

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
