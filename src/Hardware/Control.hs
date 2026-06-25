{-# LANGUAGE CPP #-}
module Hardware.Control (
    configureSensor,
    configureSensorWithRetry,
    parseConfig,
    configureRawSerial,
    setBeam,
    setBeamChannel,
    setBeamChannelDaemon,
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
import Hardware.Manifest (watchdogPin, logicPin, configBaudRate)
import qualified Data.ByteString as B
import System.FilePath (isAbsolute, splitDirectories)
import Control.Concurrent.STM (TVar)
import Data.Types (SystemState)

import Hardware.Types (HardwareError(..))
import Hardware.FFI.Common
import Hardware.FFI.Bridge

initGpio :: TVar SystemState -> IO (MustHandle ())
initGpio stateVar = bridgeHardwareCall stateVar "HardwareControl" c_gpio_init

setupWatchdog :: TVar SystemState -> IO (MustHandle ())
setupWatchdog stateVar = bridgeHardwareCall stateVar "HardwareControl" (c_gpio_setup_watchdog (fromIntegral watchdogPin))

readBeamChannel :: GpioChannel -> IO (Either HardwareError Bool)
readBeamChannel channel = do
    let pinNum = case channel of
            LogicChannel -> fromIntegral logicPin
            WatchdogChannel -> fromIntegral watchdogPin
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
        let termBaud = case configBaudRate of
                115200 -> B115200
                9600   -> B9600
                19200  -> B19200
                38400  -> B38400
                57600  -> B57600
                230400 -> B230400
                _      -> error "Unsupported config baud rate"
        attrs <- getTerminalAttributes fd
        let cfgAttrs = attrs `withInputSpeed` termBaud `withOutputSpeed` termBaud
        setTerminalAttributes fd cfgAttrs Immediately
        return ()
    case result of
        Left ex -> return $ Left $ ConfigurationFailed $ show (ex :: IOException)
        Right () -> return $ Right ()

configureRawSerial :: TVar SystemState -> Fd -> IO (MustHandle ())
configureRawSerial stateVar (Fd fd) = do
    let baud = fromIntegral uartBaudRate
    bridgeHardwareCall stateVar "HardwareControl" (c_configure_serial_port fd baud)

setBeam :: TVar SystemState -> Bool -> IO (MustHandle ())
setBeam stateVar state = setBeamChannel stateVar LogicChannel state

data GpioChannel = LogicChannel | WatchdogChannel
  deriving (Show, Eq)

setBeamChannel :: TVar SystemState -> GpioChannel -> Bool -> IO (MustHandle ())
setBeamChannel stateVar channel state = do
    let pinNum = case channel of
            LogicChannel -> fromIntegral logicPin
            WatchdogChannel -> fromIntegral watchdogPin
    let stateStr = if state then "ON" else "OFF"
    let chanStr = case channel of
            LogicChannel -> "LOGIC Channel"
            WatchdogChannel -> "WATCHDOG Channel"
    putStrLn $ "[Hardware] " ++ chanStr ++ " Set To: " ++ stateStr
    bridgeHardwareCall stateVar "HardwareControl" (c_gpio_write pinNum (if state then 1 else 0))

setBeamChannelDaemon :: (HardwareResult -> IO ()) -> GpioChannel -> Bool -> IO (MustHandle ())
setBeamChannelDaemon auditFn channel state = do
    let pinNum = case channel of
            LogicChannel -> fromIntegral logicPin
            WatchdogChannel -> fromIntegral watchdogPin
    let stateStr = if state then "ON" else "OFF"
    let chanStr = case channel of
            LogicChannel -> "LOGIC Channel"
            WatchdogChannel -> "WATCHDOG Channel"
    putStrLn $ "[Hardware] Daemon " ++ chanStr ++ " Set To: " ++ stateStr
    bridgeHardwareCallCustom auditFn (c_gpio_write pinNum (if state then 1 else 0))

-- | Configuration interface to adjust the polynomial order on the sensor
-- This sends a command over the serial port.
setPolynomialOrder :: Fd -> Int -> IO (Either HardwareError ())
setPolynomialOrder fd order = do
    let cmd = BC.pack ("surfaceOrder " ++ show order ++ "\n")
    bytesSent <- fdWrite fd cmd
    if fromIntegral bytesSent < BC.length cmd
        then return $ Left (ConfigurationFailed "Failed to send surfaceOrder command")
        else return $ Right ()
