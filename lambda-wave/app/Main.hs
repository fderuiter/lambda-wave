module Main where

import Control.Concurrent (forkOS)
import Control.Concurrent.STM
import System.Clock

import LambdaWave.Types
import LambdaWave.Hardware.Ingestion
import LambdaWave.Hardware.Parser
import LambdaWave.Hardware.Control
import LambdaWave.Safety.Watchdog
import LambdaWave.Safety.Audit
import LambdaWave.UI.Window
import LambdaWave.UI.Renderer
import LambdaWave.UI.Input

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

    -- 0. Configure Hardware
    -- forkOS $ configureSensor "/dev/ttyACM0"

    -- 1. Hardware Ingestion (Dedicated Thread)
    -- We assume the C buffer is initialized inside ingestionLoop
    -- Ingestion loop needs a channel only if we kept the old design, but now it uses Ring Buffer.
    -- However, ingestionLoop signature in my implementation is `TChan -> FilePath -> IO ()`.
    -- I should probably update `ingestionLoop` to not need TChan, or pass a dummy one.
    -- But wait, I updated `ingestionLoop` to keep the signature but unused TChan?
    -- Let's check `Ingestion.hs` content.
    -- "ingestionLoop :: TChan B.ByteString -> FilePath -> IO ()"
    -- Yes. So I need to pass a TChan even if unused, or fix the type.
    -- Let's pass a dummy for now to avoid breaking signature if I didn't change it.
    -- Actually I can just change the signature in Ingestion.hs if I want, but I already wrote the file.
    rawDataChan <- newTChanIO

    _ <- forkOS $ ingestionLoop rawDataChan "/dev/ttyACM1"

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
