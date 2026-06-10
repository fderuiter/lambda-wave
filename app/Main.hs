{-# LANGUAGE CPP #-}
module Main (main) where

import Control.Concurrent (forkOS, setNumCapabilities, threadDelay, forkIO)
import Control.Concurrent.STM
import Control.Exception (try, IOException)
import System.Environment (lookupEnv)
import Data.Maybe (fromMaybe)
import System.Posix.IO (openFd, OpenMode(..), defaultFileFlags, OpenFileFlags(..), fdWriteBuf, closeFd)
import System.Posix.Files (getFdStatus, isCharacterDevice, createNamedPipe, unionFileModes, ownerReadMode, ownerWriteMode)
import System.Posix.Types (Fd)
import Control.Monad (forever, unless, void)
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
import Hardware.Control (configureRawSerial)
import Hardware.Consumer (coreLoop)
import Safety.Watchdog (checkWatchdogInit)
import Safety.Audit
import Data.Time.HighRes (getMonotonicTimeNS)

main :: IO ()
main = runMain

runMain :: IO ()
runMain = do
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
          , sequenceNumber = 0
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
    _cliFd <- openAndValidatePort "CLI port" cliPort

    -- 1. Setup Ring Buffer (4MB)
    ringBuffer <- RingBuffer.createRingBuffer (4 * 1024 * 1024)

    -- Configure Port (Raw Mode) to prevent data corruption
    res <- configureRawSerial fd
    case res of
        Left err -> do
            putStrLn $ "FATAL: Failed to configure serial port: " ++ show err
            exitFailure
        Right () -> return ()

    -- Initialize Watchdog Hardware State
    checkWatchdogInit

    -- 2. Audit Logging
    _ <- forkOS $ auditLoop systemState "session.log"

    -- 3. IPC Sender to Visualizer
    putStrLn "Starting IPC Telemetry Stream..."
    _ <- forkIO $ ipcSenderLoop systemState

    putStrLn "System Armed. SafetyCore is running."
    
    -- 4. Start single-process Core Execution Loop directly in main thread
    coreLoop True ringBuffer fd systemState


-- | IPC Sender Loop using a POSIX FIFO with O_NONBLOCK
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
