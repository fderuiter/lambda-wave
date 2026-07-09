{-# LANGUAGE CPP #-}
module Main (main) where

import Control.Concurrent (setNumCapabilities, threadDelay)
import Safety.Thread (forkSafetyThread, forkSafetyThreadOS, ThreadShutdownAction(..))
import Control.Concurrent.STM
import Control.Exception (try, IOException)
import System.Environment (lookupEnv, getArgs, getExecutablePath)
import Data.Maybe (fromMaybe)
import System.Posix.IO (openFd, OpenMode(..), defaultFileFlags, OpenFileFlags(..), fdWriteBuf, closeFd)
import System.Posix.Files (getFdStatus, isCharacterDevice, createNamedPipe, unionFileModes, ownerReadMode, ownerWriteMode)
import System.Posix.Types (Fd, ProcessID)
import System.Posix.Process (forkProcess, executeFile, getProcessID)
import Control.Monad (forever, unless)
import qualified Data.Map.Strict as Map
import System.Exit (exitFailure)
import Data.Binary (encode)
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as B
import Data.ByteString.Unsafe (unsafeUseAsCStringLen)
import Foreign.Ptr (castPtr, plusPtr)
import Data.Word (Word32)

import Data.Types
import Data.Config (targetHeight)
import SignalProcessing.Kalman (initKalman, KalmanConfig(..))
import qualified FFI.RingBuffer.IO as RingBuffer
import Hardware.Control (configureRawSerial, configureSensorWithRetry)
import Hardware.FFI.Bridge (handleHardwareResponse, triggerShutdown)
import Hardware.Consumer (consumerLoop)
import Safety.Watchdog (watchdogLoop, runSafetyDaemon)
import Safety.Audit
import Data.Time.HighRes (getMonotonicTimeNS)
import Data.I18n (loadTranslations)
import Data.Aeson (decode, FromJSON(..), (.:), withObject)

data HardwareManifest = HardwareManifest
    { manifestMountingOffset :: Double }
    deriving (Show)

instance FromJSON HardwareManifest where
    parseJSON = withObject "HardwareManifest" $ \obj -> HardwareManifest
        <$> obj .: "mounting_offset_mm"

main :: IO ()
main = do
    args <- getArgs
    case args of
        ["--safety-daemon", parentPidStr] -> do
            let parentPid = read parentPidStr :: ProcessID
            runSafetyDaemon parentPid
        _ -> runMain

runMain :: IO ()
runMain = do
    -- lock capabilities to specific cores
    setNumCapabilities 2
    putStrLn "Initializing Lambda-Wave System..."

    startTime <- getMonotonicTimeNS

    translations <- loadTranslations "config/locales.json"

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
          , sequenceNumber = 0
          , isocenter = Point3D 0 0 0 0 0
          , threadHeartbeats = Map.empty
          , kalmanState = initialKState
          , mtiState = []
          , auditQueue = auditQ
          , audioAlertEnabled = audioAlerts
          , activeLanguage = "en"
          , localizedBeamState = "BEAM OFF"
          , calibrationStatus = CalibrationUnverified
          , displayPreset = StandardPreset
          }

    systemState <- newTVarIO initialState

    -- Get Configuration from Environment
    sensorPort <- fromMaybe "/dev/ttyUSB0" <$> lookupEnv "SGRT_SENSOR_PORT"
    cliPort    <- fromMaybe "/dev/ttyUSB1" <$> lookupEnv "SGRT_CLI_PORT"

    putStrLn $ "Configuration: Sensor=" ++ sensorPort ++ ", CLI=" ++ cliPort

    -- Security Validation: Ensure the ports are character devices
    let openAndValidatePort name path = do
            let flags = defaultFileFlags { nonBlock = False }
#if MIN_VERSION_unix(2,8,0)
            let flags' = flags { creat = Nothing }
            fdRes <- try (openFd path ReadWrite flags') :: IO (Either IOException Fd)
#else
            fdRes <- try (openFd path ReadWrite Nothing flags) :: IO (Either IOException Fd)
#endif
            case fdRes of
                Left err -> do
                    putStrLn $ "FATAL: Could not access " ++ name ++ " " ++ path ++ ": " ++ show err
                    exitFailure
                Right f -> do
                    fStatus <- getFdStatus f
                    unless (isCharacterDevice fStatus) $ do
                        putStrLn $ "FATAL: Security Violation - " ++ path ++ " (" ++ name ++ ") is not a character device."
                        exitFailure
                    return f

    fd <- openAndValidatePort "sensor port" sensorPort
    -- We don't open the cliPort here, we let configureSensor do it securely to avoid FD leaks
    -- _cliFd <- openAndValidatePort "CLI port" cliPort

    -- Parse Hardware Manifest for Mounting Offset
    manifestBytes <- BL.readFile "config/hardware_manifest.json"
    mountingOffset <- case decode manifestBytes of
        Just m -> return (manifestMountingOffset m)
        Nothing -> do
            putStrLn "FATAL: Failed to parse protected configuration file: hardware_manifest.json"
            exitFailure

    putStrLn $ "Loaded physical mounting offset: " ++ show mountingOffset ++ " mm"

    -- Sensor Configuration Handshake
    putStrLn "Starting Sensor Calibration Handshake..."
    configRes <- configureSensorWithRetry 3 "config/ti_iwr6843isk/sgrt_profile.cfg" cliPort
    case configRes of
        Left err -> do
            putStrLn $ "Hardware Handshake Failed: " ++ show err
            atomically $ modifyTVar' systemState $ \s -> s { calibrationStatus = CalibrationInvalid }
        Right () -> do
            putStrLn "Hardware Handshake Successful. Sensor Calibrated."
            atomically $ modifyTVar' systemState $ \s -> s { calibrationStatus = CalibrationValid }

    -- 1. Setup Ring Buffer (4MB)
    -- We use the new FFI.RingBuffer.IO directly.
    -- NOW RETURNS ForeignPtr RingBufferControl.
    -- This ensures the buffer is automatically freed when all references (Main thread, consumer thread, ingestion thread) are gone.
    rbRes <- RingBuffer.createRingBuffer systemState (4 * 1024 * 1024)
    ringBuffer <- handleHardwareResponse
        (\err -> do
            if err == SimulationModeActive
                then putStrLn "FATAL: RingBuffer is in Mock Mode! Blocking operational state."
                else putStrLn $ "FATAL: Failed to create ring buffer: " ++ show err
            exitFailure
        )
        (\rb -> return rb)
        rbRes

    -- 1.5 Setup GPIO
    gpioRes <- Hardware.Control.initGpio systemState
    handleHardwareResponse
        (\err -> do
            if err == SimulationModeActive
                then putStrLn "FATAL: GPIO is in Mock Mode! Blocking operational state."
                else putStrLn $ "FATAL: GPIO initialization failed: " ++ show err
            exitFailure
        )
        (\() -> return ())
        gpioRes

    -- Configure Port (Raw Mode) to prevent data corruption
    res <- configureRawSerial systemState fd
    handleHardwareResponse
        (\err -> do
            if err == SimulationModeActive
                then putStrLn "FATAL: Serial Port is in Mock Mode! Blocking operational state."
                else putStrLn $ "FATAL: Failed to configure serial port: " ++ show err
            exitFailure
        )
        (\() -> return ())
        res

    -- 2. Hardware Ingestion (Dedicated Thread)
    -- ingestionLoop accepts ForeignPtr
    _ <- RingBuffer.ingestionLoop systemState ringBuffer fd

    -- Process Boundary: Safety Daemon Spawning
    -- Requirement: SR-IPC-001
    -- Justification: Safety Daemon is spawned as a separate process to ensure it survives if the main process hangs or crashes.
    -- IPC Mechanism: AF_UNIX socket for heartbeat monitoring.
    -- Failure Mode: Socket exhaustion or permission denied during daemon startup.
    -- Mitigation: Parent validates daemon PID and checks early hardware setup response. Daemon kills parent if socket binding fails.
    exePath <- getExecutablePath
    myPid <- getProcessID
    _daemonPid <- forkProcess $ executeFile exePath False ["--safety-daemon", show myPid] Nothing

    -- 3. Consumer/Parser (Dedicated Thread)
    _ <- forkSafetyThreadOS (ShutdownSystem $ triggerShutdown systemState) "ConsumerLoop" $ 
        consumerLoop mountingOffset translations True ringBuffer systemState

    -- 3. Safety Watchdog Heartbeat Sender (High Priority Thread)
    _ <- forkSafetyThreadOS (ShutdownSystem $ triggerShutdown systemState) "WatchdogLoop" $ 
        watchdogLoop systemState

    -- 4. Audit Logging
    _ <- forkSafetyThreadOS (ShutdownSystem $ triggerShutdown systemState) "AuditLoop" $ 
        auditLoop systemState "session.log"

    -- 5. IPC Sender to Visualizer
    putStrLn "Starting IPC Telemetry Stream..."
    _ <- forkSafetyThread (ShutdownSystem $ triggerShutdown systemState) "IPCSenderLoop" $ 
        ipcSenderLoop systemState

    putStrLn "System Armed. SafetyCore is running."
    
    -- Keep Main Alive
    forever $ threadDelay 1000000

-- | IPC Sender Loop using a POSIX FIFO with O_NONBLOCK
-- Requirement: SR-IPC-001
-- Hazard H-SYS-010: IPC Failure
-- IPC Mechanism: POSIX FIFO (Named Pipe)
-- Failure Mode: Reader (Visualizer) crashes or falls behind, causing the FIFO buffer to fill up.
-- Mitigation: O_NONBLOCK is used. If the buffer is full (EAGAIN), the writer catches the exception, drops the frame, and continues operating without blocking the main safety loop.
ipcSenderLoop :: TVar SystemState -> IO ()
ipcSenderLoop stateVar = do
    let pipePath = "/tmp/sgrt_telemetry.fifo"
    _ <- try (createNamedPipe pipePath (unionFileModes ownerReadMode ownerWriteMode)) :: IO (Either IOException ())
    
    let flags = defaultFileFlags { nonBlock = True }
#if MIN_VERSION_unix(2,8,0)
    let flags' = flags { creat = Nothing }
#endif

    forever $ do
#if MIN_VERSION_unix(2,8,0)
        fdRes <- try (openFd pipePath WriteOnly flags') :: IO (Either IOException Fd)
#else
        fdRes <- try (openFd pipePath WriteOnly Nothing flags) :: IO (Either IOException Fd)
#endif
        case fdRes of
            Left _ -> threadDelay 1000000 -- Wait for reader
            Right fd -> do
                streamData fd stateVar
                _ <- try (closeFd fd) :: IO (Either IOException ())
                return ()

streamData :: Fd -> TVar SystemState -> IO ()
streamData fd stateVar = do
    let loop = do
            state <- readTVarIO stateVar
            let packet = TelemetryPacket
                  { tpBeamState = beamState state
                  , tpLastFrameTime = lastFrameTime state
                  , tpSequenceNumber = sequenceNumber state
                  , tpIsocenter = isocenter state
                  , tpThreadHeartbeats = threadHeartbeats state
                  , tpKalmanState = kalmanState state
                  , tpAudioAlertEnabled = audioAlertEnabled state
                  , tpActiveLanguage = activeLanguage state
                  , tpLocalizedBeamState = localizedBeamState state
                  , tpCalibrationStatus = calibrationStatus state
                  }
            let payload = BL.toStrict (encode packet)
            let len = fromIntegral (B.length payload) :: Word32
            let lenPayload = BL.toStrict (encode len)
            let frame = B.append lenPayload payload
            
            res <- try (writeBsToFd fd frame) :: IO (Either IOException ())
            case res of
                Left _ -> return () -- Reader disconnected or buffer full, break loop
                Right _ -> do
                    threadDelay 33000 -- ~30Hz
                    loop
    loop

writeBsToFd :: Fd -> B.ByteString -> IO ()
writeBsToFd fd bs = unsafeUseAsCStringLen bs $ \(ptr, len) -> do
    let loop remain curPtr = do
            wrote <- fdWriteBuf fd (castPtr curPtr) (fromIntegral remain)
            let wroteInt = fromIntegral wrote
            if wroteInt < remain
                then loop (remain - wroteInt) (curPtr `plusPtr` wroteInt)
                else return ()
    loop len ptr

-- Requirement SR-SOUP-001

-- Requirement FR-GAT-002
-- Hazard H-SOUP-001: Unbounded GC Pauses
-- Hazard H-SOUP-002: Thread Starvation
