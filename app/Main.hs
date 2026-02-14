{-# LANGUAGE CPP #-}
module Main (main) where

import Control.Concurrent (forkOS, setNumCapabilities, threadDelay)
import Control.Concurrent.STM
import System.Environment (lookupEnv)
import Data.Maybe (fromMaybe)
import System.Posix.IO (openFd, OpenMode(..), defaultFileFlags, OpenFileFlags(..), createPipe)
import System.Posix.Files (ownerReadMode, ownerWriteMode, unionFileModes)
import Control.Monad (forever)
import qualified Data.Map.Strict as Map

import qualified Simulation
import qualified UI.Web as UI

import Data.Types
import Data.Config (targetHeight)
import SignalProcessing.Kalman (initKalman, KalmanConfig(..))
import qualified FFI.RingBuffer.IO as RingBuffer
import Hardware.Control (configureRawSerial)
import Hardware.Consumer (consumerLoop)
import Safety.Watchdog
import Safety.Audit
import Data.Time.HighRes (getMonotonicTimeNS)

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

    let initialState = SystemState
          { currentPoints = []
          , beamState = BeamOff
          , lastFrameTime = startTime
          , isocenter = Point3D 0 0 0 0 0
          , threadHeartbeats = Map.empty
          , kalmanState = initialKState
          , auditQueue = auditQ
          }

    systemState <- newTVarIO initialState

    -- Get Configuration from Environment
    sensorPort <- fromMaybe "/dev/ttyUSB0" <$> lookupEnv "SGRT_SENSOR_PORT"
    cliPort    <- fromMaybe "/dev/ttyUSB1" <$> lookupEnv "SGRT_CLI_PORT"

    putStrLn $ "Configuration: Sensor=" ++ sensorPort ++ ", CLI=" ++ cliPort

    -- 0. Configure Hardware
    -- forkOS $ configureSensor cliPort

    -- 1. Setup Ring Buffer (4MB)
    -- We use the new FFI.RingBuffer.IO directly.
    -- NOW RETURNS ForeignPtr RingBufferControl.
    -- This ensures the buffer is automatically freed when all references (Main thread, consumer thread, ingestion thread) are gone.
    ringBuffer <- RingBuffer.createRingBuffer (4 * 1024 * 1024)

    -- Check for Simulation Mode
    simMode <- lookupEnv "SGRT_SIMULATION"
    fd <- case simMode of
        Just _ -> do
            putStrLn "STARTING IN SIMULATION MODE (No Hardware Required)"
            (readFd, writeFd) <- createPipe
            _ <- forkOS $ Simulation.simulationLoop writeFd
            return readFd
        Nothing -> do
            -- Open Serial Port using POSIX for the C++ driver
            -- We need to open it here to pass the Fd to the ingestion loop.
            -- Ideally, we should use 'SP.openSerial' then get the Fd, but 'serialport' doesn't expose Fd easily.
            -- So we use 'openFd' from 'unix'.
            -- The port is explicitly configured (baud rate 115200, raw mode) using 'configureRawSerial' below.

#if MIN_VERSION_unix(2,8,0)
            let flags = defaultFileFlags { nonBlock = False, creat = Just (ownerReadMode `unionFileModes` ownerWriteMode) }
            f <- openFd sensorPort ReadWrite flags
#else
            f <- openFd sensorPort ReadWrite (Just (ownerReadMode `unionFileModes` ownerWriteMode)) defaultFileFlags { nonBlock = False }
#endif

            -- Configure Port (Raw Mode) to prevent data corruption
            configureRawSerial f
            return f

    -- 2. Hardware Ingestion (Dedicated Thread)
    -- ingestionLoop accepts ForeignPtr
    _ <- RingBuffer.ingestionLoop ringBuffer fd

    -- 3. Consumer/Parser (Dedicated Thread)
    -- consumerLoop accepts ForeignPtr
    _ <- forkOS $ consumerLoop ringBuffer systemState

    -- 3. Safety Watchdog (High Priority Thread)
    _ <- forkOS $ watchdogLoop systemState

    -- 4. Audit Logging
    _ <- forkOS $ auditLoop systemState "session.log"

    -- 5. UI (Web-Based for Visualization)
    _ <- forkOS $ UI.runServer 8080 systemState
    putStrLn "System Armed. Web UI running on port 8080."

    -- Keep Main Alive
    forever $ threadDelay 1000000
