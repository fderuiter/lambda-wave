module Main where

import Control.Concurrent (forkOS)
import Control.Concurrent.STM
import System.Clock
import System.Environment (lookupEnv)
import Data.Maybe (fromMaybe)

import Data.Types
import Hardware.Ingestion
import Hardware.Parser
import Hardware.Control
import Safety.Watchdog
import Safety.Audit
import Control.UI.Window
import Control.UI.Renderer
import Control.UI.Input

main :: IO ()
main = do
    putStrLn "Initializing Lambda-Wave System..."

    startTime <- getTime Monotonic
    let initialState = SystemState
          { currentPoints = []
          , beamState = BeamOff
          , lastFrameTime = startTime
          , isocenter = Point3D 0 0 0 0 0
          }

    systemState <- newTVarIO initialState

    -- Get Configuration from Environment
    sensorPort <- fromMaybe "/dev/ttyUSB0" <$> lookupEnv "SGRT_SENSOR_PORT"
    cliPort    <- fromMaybe "/dev/ttyUSB1" <$> lookupEnv "SGRT_CLI_PORT"

    putStrLn $ "Configuration: Sensor=" ++ sensorPort ++ ", CLI=" ++ cliPort

    -- 0. Configure Hardware
    -- forkOS $ configureSensor cliPort

    -- 1. Hardware Ingestion (Dedicated Thread)
    rawDataChan <- newTChanIO -- Kept for compatibility if ingestionLoop requires it
    _ <- forkOS $ ingestionLoop rawDataChan sensorPort

    -- 2. Parsing & Gating Pipeline (Dedicated Thread)
    _ <- forkOS $ parserLoop systemState

    -- 3. Safety Watchdog (High Priority Thread)
    _ <- forkOS $ watchdogLoop systemState

    -- 4. Audit Logging
    _ <- forkOS $ auditLoop systemState "session.log"

    -- 5. UI (Main Thread)
    putStrLn "System Armed. Starting UI..."
    initWindow
    handleInput systemState
    renderLoop systemState
