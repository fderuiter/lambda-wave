{-# LANGUAGE CPP #-}
module Main (main) where

import Control.Concurrent (forkOS, setNumCapabilities, threadDelay)
#ifdef ENABLE_WEB_UI
import Control.Concurrent (forkIO)
#endif
import Control.Concurrent.STM
import Control.Exception (try, IOException)
import System.Environment (lookupEnv)
import Data.Maybe (fromMaybe)
import System.Posix.IO (openFd, closeFd, OpenMode(..), defaultFileFlags, OpenFileFlags(..))
import System.Posix.Files (getFdStatus, isCharacterDevice, FileStatus)
import System.Posix.Types (Fd(..))
import Control.Monad (forever, unless)
import qualified Data.Map.Strict as Map
import System.Exit (exitFailure)

import Data.Types
import Data.Config (targetHeight)
import SignalProcessing.Kalman (initKalman, KalmanConfig(..))
import qualified FFI.RingBuffer.IO as RingBuffer
import Hardware.Control (configureRawSerial)
import Hardware.Consumer (consumerLoop)
import Safety.Watchdog
import Safety.Audit
import Data.Time.HighRes (getMonotonicTimeNS)

#ifdef ENABLE_UI
import Control.UI.Window (initWindow)
import Control.UI.Renderer (renderLoop)
import Control.UI.Input (handleInput)
#endif

#ifdef ENABLE_WEB_UI
import Control.WebUI (runWebUI)
#endif

main :: IO ()
main = do
    -- lock capabilities to specific cores
    setNumCapabilities 2
    putStrLn "Initializing Lambda-Wave System..."

    startTime <- getMonotonicTimeNS

    let kConfig = KalmanConfig { procNoise = 10.0, measNoise = 2.0 }
    let initialKState = initKalman targetHeight kConfig

    -- Initialize High-Performance Audit Queue
    auditQ <- newTBQueueIO 1000

    audioAlertsStr <- fromMaybe "True" <$> lookupEnv "SGRT_AUDIO_ALERTS"
    let audioAlerts = audioAlertsStr == "True" || audioAlertsStr == "true" || audioAlertsStr == "1"

    let initialState = SystemState
          { currentPoints = []
          , beamState = BeamOff
          , lastFrameTime = startTime
          , isocenter = Point3D 0 0 0 0 0
          , threadHeartbeats = Map.empty
          , kalmanState = initialKState
          , auditQueue = auditQ
          , audioAlertEnabled = audioAlerts
          }

    systemState <- newTVarIO initialState

    -- Get Configuration from Environment
    sensorPort <- fromMaybe "/dev/ttyUSB0" <$> lookupEnv "SGRT_SENSOR_PORT"
    cliPort    <- fromMaybe "/dev/ttyUSB1" <$> lookupEnv "SGRT_CLI_PORT"

    putStrLn $ "Configuration: Sensor=" ++ sensorPort ++ ", CLI=" ++ cliPort

    -- Security Validation: Ensure the ports are character devices (TOCTOU safe)
    let validateAndOpenPort name path = do
#if MIN_VERSION_unix(2,8,0)
            let flags = defaultFileFlags { nonBlock = False, creat = Nothing }
            fdRes <- try (openFd path ReadWrite flags) :: IO (Either IOException Fd)
#else
            fdRes <- try (openFd path ReadWrite Nothing defaultFileFlags { nonBlock = False }) :: IO (Either IOException Fd)
#endif
            case fdRes of
                Left err -> do
                    putStrLn $ "FATAL: Could not open " ++ name ++ " " ++ path ++ ": " ++ show err
                    exitFailure
                Right fd -> do
                    statusRes <- try (getFdStatus fd) :: IO (Either IOException FileStatus)
                    case statusRes of
                        Left err -> do
                            closeFd fd
                            putStrLn $ "FATAL: Could not get status of " ++ name ++ " " ++ path ++ ": " ++ show err
                            exitFailure
                        Right status -> do
                            unless (isCharacterDevice status) $ do
                                closeFd fd
                                putStrLn $ "FATAL: Security Violation - " ++ path ++ " (" ++ name ++ ") is not a character device."
                                exitFailure
                            return fd

    fd <- validateAndOpenPort "sensor port" sensorPort
    _cliFd <- validateAndOpenPort "CLI port" cliPort

    -- the application expects cliFd to be used elsewhere or just opened to reserve it
    -- (We will leave it open, as it's meant to be the CLI port device)

    -- 1. Setup Ring Buffer (4MB)
    -- We use the new FFI.RingBuffer.IO directly.
    -- NOW RETURNS ForeignPtr RingBufferControl.
    -- This ensures the buffer is automatically freed when all references (Main thread, consumer thread, ingestion thread) are gone.
    ringBuffer <- RingBuffer.createRingBuffer (4 * 1024 * 1024)

    -- We already opened the sensorPort safely above, and obtained `fd`.
    -- The port is explicitly configured (baud rate 115200, raw mode) using 'configureRawSerial' below.

    -- Configure Port (Raw Mode) to prevent data corruption
    res <- configureRawSerial fd
    case res of
        Left err -> do
            putStrLn $ "FATAL: Failed to configure serial port: " ++ show err
            exitFailure
        Right () -> return ()

    -- 2. Hardware Ingestion (Dedicated Thread)
    -- ingestionLoop accepts ForeignPtr
    _ <- RingBuffer.ingestionLoop ringBuffer fd

    -- 3. Consumer/Parser (Dedicated Thread)
    _ <- forkOS $ consumerLoop ringBuffer systemState

    -- 3. Safety Watchdog (High Priority Thread)
    _ <- forkOS $ watchdogLoop systemState

    -- 4. Audit Logging
    _ <- forkOS $ auditLoop systemState "session.log"

    -- 5. Web UI (Optional)
#ifdef ENABLE_WEB_UI
    putStrLn "Starting Web UI..."
    _ <- forkIO $ runWebUI systemState
#endif

    -- 6. OpenGL UI (Optional, must be Main Thread if used)
#ifdef ENABLE_UI
    putStrLn "Starting OpenGL UI..."
    initWindow
    handleInput systemState
    renderLoop systemState
#else
    putStrLn "System Armed. Headless Mode."
    -- Keep Main Alive
    forever $ threadDelay 1000000
#endif
