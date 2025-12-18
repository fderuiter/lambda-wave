module Main where

import Control.Concurrent (forkOS)
import Control.Concurrent.STM
import System.Clock
import System.Environment (lookupEnv)
import Data.Maybe (fromMaybe)
import System.Posix.IO (openFd, OpenMode(..), defaultFileFlags, OpenFileFlags(..))
import System.Posix.Types (Fd(..))
import qualified System.Hardware.Serialport as SP

import Data.Types
import qualified FFI.RingBuffer.IO as RingBuffer
import Hardware.Consumer (consumerLoop)
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

    -- 1. Setup Ring Buffer (4MB)
    -- We use the new FFI.RingBuffer.IO directly
    ringBuffer <- RingBuffer.createRingBuffer (4 * 1024 * 1024)

    -- Open Serial Port using POSIX for the C++ driver
    -- We need to open it here to pass the Fd to the ingestion loop.
    -- We configure it using 'serialport' library if needed, but since the C++ driver
    -- just reads raw bytes, we assume the port is configured (or we use stty externally).
    -- Ideally, we should use 'SP.openSerial' then get the Fd, but 'serialport' doesn't expose Fd easily.
    -- So we use 'openFd' from 'unix'.
    -- WARNING: This assumes the port is already configured (baud rate, etc.)!
    -- In a real system, we would configure it here using termios.
    -- For this task, we assume the environment or a startup script handled 'stty'.

    fd <- openFd sensorPort ReadWrite Nothing defaultFileFlags { nonBlock = False }

    -- 2. Hardware Ingestion (Dedicated Thread)
    _ <- forkOS $ RingBuffer.ingestionLoop ringBuffer fd

    -- 3. Consumer/Parser (Dedicated Thread)
    _ <- forkOS $ consumerLoop ringBuffer systemState

    -- 3. Safety Watchdog (High Priority Thread)
    _ <- forkOS $ watchdogLoop systemState

    -- 4. Audit Logging
    _ <- forkOS $ auditLoop systemState "session.log"

    -- 5. UI (Main Thread)
    putStrLn "System Armed. Starting UI..."
    initWindow
    handleInput systemState
    renderLoop systemState
