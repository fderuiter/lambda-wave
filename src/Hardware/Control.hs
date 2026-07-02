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

import Control.Concurrent (threadDelay)
import qualified Data.ByteString.Char8 as BC
import Control.Exception (try, IOException, bracket)
import Data.Char (isSpace)
import Data.List (dropWhileEnd)
import System.IO (withFile, IOMode(ReadMode))
import System.Posix.Terminal
import System.Posix.IO (openFd, closeFd, OpenMode(ReadWrite), defaultFileFlags)
import System.Posix.IO.ByteString (fdWrite, fdRead)
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
import qualified Hardware.FFI.Common as Common
import Hardware.FFI.Bridge

initGpio :: TVar SystemState -> IO (MustHandle ())
initGpio stateVar = bridgeHardwareCall stateVar "HardwareControl" c_gpio_init

setupWatchdog :: TVar SystemState -> IO (MustHandle ())
setupWatchdog stateVar = bridgeHardwareCall stateVar "HardwareControl" (c_gpio_setup_watchdog (fromIntegral watchdogPin))

readBeamChannel :: TVar SystemState -> GpioChannel -> IO (MustHandle Bool)
readBeamChannel stateVar channel = do
    let pinNum = case channel of
            LogicChannel -> fromIntegral logicPin
            WatchdogChannel -> fromIntegral watchdogPin
    bridgeHardwareQuery stateVar "HardwareControl" (c_gpio_read pinNum) $ \val ->
        case val of
            0 -> (Common.Success, Right False)
            1 -> (Common.Success, Right True)
            _ -> (Common.Failure "Connection Lost", Left ConnectionLost)

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
                                            let handshakeLoop [] = return ()
                                                handshakeLoop (cmd:cmds) = do
                                                    let packet = BC.pack (cmd ++ "\n")
                                                    bytesSent <- fdWrite fd packet
                                                    if fromIntegral bytesSent < BC.length packet
                                                        then ioError (userError "Failed to send")
                                                        else do
                                                            ackResult <- readUntilDone fd B.empty
                                                            if not ackResult
                                                                then ioError (userError $ "Handshake failed on command: " ++ cmd)
                                                                else do
                                                                    threadDelay 10000 -- 10ms
                                                                    handshakeLoop cmds
                                            handshakeLoop commands
                        )
                    case result of
                        Left ex -> return (Left $ ConfigurationFailed $ show (ex :: IOException))
                        Right _ -> return (Right ())

readUntilDone :: Fd -> B.ByteString -> IO Bool
readUntilDone fd acc = do
    readRes <- try (fdRead fd 128) :: IO (Either IOException B.ByteString)
    case readRes of
        Left _ -> return False
        Right bs
            | B.null bs -> return True -- Assume EOF/Mock success
            | otherwise -> do
                let newAcc = B.append acc bs
                if "Done" `B.isInfixOf` newAcc
                    then return True
                    else if "Error" `B.isInfixOf` newAcc
                        then return False
                        else if B.length newAcc > 4096
                            then return False -- Prevent unbounded memory growth
                            else readUntilDone fd newAcc

configureConfigSerial :: Fd -> IO (Either HardwareError ())
configureConfigSerial fd = do
    result <- try $ do
        let mTermBaud = case configBaudRate of
                115200 -> Just B115200
                9600   -> Just B9600
                19200  -> Just B19200
                38400  -> Just B38400
                57600  -> Just B57600
                230400 -> Just B230400
                _      -> Nothing
        case mTermBaud of
            Nothing -> ioError (userError "Unsupported config baud rate")
            Just termBaud -> do
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
