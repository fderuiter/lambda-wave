{-# LANGUAGE CPP #-}
module Hardware.Control (
    configureSensor,
    configureSensorWithRetry,
    parseConfig,
    configureRawSerial,
    setBeam,
    setBeamChannel,
    GpioChannel(..),
    configureConfigSerial,
    initGpio,
    setupWatchdog,
    readBeamChannel,
    setPolynomialOrder
) where

import Control.Monad (forM_)
import Control.Concurrent (threadDelay)
import qualified Data.ByteString.Char8 as BC
import Control.Exception (try, IOException, bracket)
import Data.Char (isSpace)
import Data.List (dropWhileEnd)
import System.IO (withFile, IOMode(ReadMode))
import System.Posix.Terminal
import System.Posix.IO (openFd, closeFd, OpenMode(ReadWrite), defaultFileFlags)
import System.Posix.IO.ByteString (fdWrite)
import System.Posix.Files (getFdStatus, isCharacterDevice)
import System.Posix.Types (Fd(..))
import Data.Config (uartBaudRate)
import qualified Data.ByteString as B
import System.FilePath (isAbsolute, splitDirectories)

import Hardware.Types (HardwareError(..))
import Hardware.FFI.Common

initGpio :: IO ()
initGpio = do
    res <- c_gpio_init
    case toHardwareResult res of
        Success -> return ()
        _ -> ioError (userError "Failed to initialize GPIO")

setupWatchdog :: IO ()
setupWatchdog = do
    res <- c_gpio_setup_watchdog 27
    case toHardwareResult res of
        Success -> return ()
        _ -> ioError (userError "Failed to setup watchdog")

readBeamChannel :: GpioChannel -> IO (Either HardwareError Bool)
readBeamChannel channel = do
    let pinNum = case channel of
            LogicChannel -> 17
            WatchdogChannel -> 27
    val <- c_gpio_read pinNum
    case val of
        0 -> return (Right False)
        1 -> return (Right True)
        _ -> return (Left ConnectionLost)

parseConfig :: String -> [String]
parseConfig = filter (not . null) . map clean . lines
  where
    clean = trim . takeWhile (/= '#')
    trim = dropWhileEnd isSpace . dropWhile isSpace

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
                threadDelay 100000
                go (n - 1)

isPathSafe :: FilePath -> Bool
isPathSafe path = not (isAbsolute path) && ".." `notElem` splitDirectories path

configureSensor :: FilePath -> FilePath -> IO (Either HardwareError ())
configureSensor configPath portPath = do
    if not (isPathSafe configPath)
        then return $ Left $ ConfigurationFailed "Unsafe configuration path detected"
        else do
            let maxConfigSize = 100 * 1024
            fileContentResult <- try $ withFile configPath ReadMode $ \h -> do
                bs <- B.hGet h maxConfigSize
                return (BC.unpack bs)
            case fileContentResult of
                Left ex -> return $ Left $ ConfigurationFailed $ "Failed to read config file: " ++ show (ex :: IOException)
                Right content -> do
                    let commands = parseConfig content
                    result <- try $ bracket
#if MIN_VERSION_unix(2,8,0)
                        (openFd portPath ReadWrite defaultFileFlags)
#else
                        (openFd portPath ReadWrite Nothing defaultFileFlags)
#endif
                        closeFd
                        (\fd -> do
                            fStatus <- getFdStatus fd
                            if not (isCharacterDevice fStatus)
                                then ioError (userError "Security Violation")
                                else do
                                    res <- configureConfigSerial fd
                                    case res of
                                        Left (ConfigurationFailed err) -> ioError (userError err)
                                        Left err -> ioError (userError $ show err)
                                        Right () -> do
                                            forM_ commands $ \cmd -> do
                                                let packet = BC.pack (cmd ++ "\n")
                                                bytesSent <- fdWrite fd packet
                                                if fromIntegral bytesSent < BC.length packet
                                                    then ioError (userError "Failed to send")
                                                    else threadDelay 100000
                        )
                    case result of
                        Left ex -> return (Left $ ConfigurationFailed $ show (ex :: IOException))
                        Right _ -> return (Right ())

configureConfigSerial :: Fd -> IO (Either HardwareError ())
configureConfigSerial fd = do
    result <- try $ do
        attrs <- getTerminalAttributes fd
        let cfgAttrs = attrs `withInputSpeed` B115200 `withOutputSpeed` B115200
        setTerminalAttributes fd cfgAttrs Immediately
        return ()
    case result of
        Left ex -> return $ Left $ ConfigurationFailed $ show (ex :: IOException)
        Right () -> return $ Right ()

configureRawSerial :: Fd -> IO (Either HardwareError ())
configureRawSerial (Fd fd) = do
    let baud = fromIntegral uartBaudRate
    res <- c_configure_serial_port fd baud
    case toHardwareResult res of
        Success -> return $ Right ()
        _       -> return $ Left $ ConfigurationFailed "Failed"

setBeam :: Bool -> IO (Either HardwareError ())
setBeam state = do
    setBeamChannel LogicChannel state

data GpioChannel = LogicChannel | WatchdogChannel
  deriving (Show, Eq)

setBeamChannel :: GpioChannel -> Bool -> IO (Either HardwareError ())
setBeamChannel channel state = do
    let pinNum = case channel of
            LogicChannel -> 17
            WatchdogChannel -> 27
    let stateStr = if state then "ON" else "OFF"
    let chanStr = case channel of
            LogicChannel -> "LOGIC Channel"
            WatchdogChannel -> "WATCHDOG Channel"
    putStrLn $ "[Hardware] " ++ chanStr ++ " Set To: " ++ stateStr
    res <- c_gpio_write pinNum (if state then 1 else 0)
    case toHardwareResult res of
        Success -> return (Right ())
        Failure msg -> do
            putStrLn $ "[Hardware] Failed to set " ++ chanStr ++ ": " ++ msg
            return (Left (UnknownError msg))
        PosixError -> do
            putStrLn $ "[Hardware] PosixError setting " ++ chanStr
            return (Left ConnectionLost)
        _ -> do
            putStrLn $ "[Hardware] Failed to set " ++ chanStr
            return (Left ConnectionLost)

-- | Configuration interface to adjust the polynomial order on the sensor
-- This sends a command over the serial port.
setPolynomialOrder :: Fd -> Int -> IO (Either HardwareError ())
setPolynomialOrder fd order = do
    let cmd = BC.pack ("surfaceOrder " ++ show order ++ "\n")
    bytesSent <- fdWrite fd cmd
    if fromIntegral bytesSent < BC.length cmd
        then return $ Left (ConfigurationFailed "Failed to send surfaceOrder command")
        else return $ Right ()
