{-# LANGUAGE CPP #-}
module Main (main) where

import Control.Concurrent (forkOS, setNumCapabilities, threadDelay)
#ifdef ENABLE_WEB_UI
import Control.Concurrent (forkIO)
#endif
import Control.Concurrent.STM
import System.Environment (lookupEnv)
import Data.Maybe (fromMaybe)
import Control.Monad (forever)
import qualified Data.Map.Strict as Map

import Data.Types
import Data.Config (targetHeight)
import SignalProcessing.Kalman (initKalman, KalmanConfig(..))
import qualified FFI.RingBuffer.IO as RingBuffer
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

    -- 1. Setup Ring Buffer (4MB)
    -- We use the new FFI.RingBuffer.IO directly.
    -- NOW RETURNS ForeignPtr RingBufferControl.
    -- This ensures the buffer is automatically freed when all references (Main thread, consumer thread, ingestion thread) are gone.
    ringBuffer <- RingBuffer.createRingBuffer (4 * 1024 * 1024)

    -- 2. Hardware Ingestion (Dedicated Thread)
    -- ingestionLoop accepts ForeignPtr and the port path.
    -- It manages opening, configuring, and reconnecting to the port.
    _ <- RingBuffer.ingestionLoop ringBuffer sensorPort

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
