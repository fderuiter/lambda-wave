module Main (main) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.STM
import qualified Data.Map.Strict as Map
import Data.Types
import Data.Time.HighRes (getMonotonicTimeNS)
import SignalProcessing.Kalman (initKalman, KalmanConfig(..))
import Safety.Watchdog (watchdogLoop, runSafetyDaemon)
import System.Environment (getArgs, getExecutablePath)
import System.Posix.Process (forkProcess, executeFile, getProcessID)
import System.Posix.Types (ProcessID)

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
    putStrLn "=== Watchdog Fault Injection Test ==="

    -- 1. Setup State
    now <- getMonotonicTimeNS
    let kConfig = KalmanConfig 0.1 0.1
    let kState = initKalman 0.0 kConfig
    q <- newTBQueueIO 100
    -- Initialize with "TestThread" heartbeat = now
    let heartbeats = Map.fromList [("TestThread", now)]
    let initialState = SystemState [] BeamOff now (Point3D 0 0 0 0 0) heartbeats kState q False
    stateVar <- newTVarIO initialState

    -- 2. Spawn Safety Daemon
    exePath <- getExecutablePath
    myPid <- getProcessID
    _daemonPid <- forkProcess $ executeFile exePath False ["--safety-daemon", show myPid] Nothing
    
    -- Small delay to let Daemon bind socket
    threadDelay 50000

    -- 3. Fork Watchdog Loop (Heartbeat Sender)
    _ <- forkIO $ watchdogLoop stateVar

    -- 4. Sleep for 250ms (Watchdog timeout is 100ms)
    -- This simulates the "TestThread" being frozen (not updating heartbeat)
    -- Watchdog should stop sending heartbeats.
    -- Safety Daemon should trip and kill us!
    threadDelay 250000

    -- 5. If we are here, Watchdog FAILED to kill us
    putStrLn "SURVIVED"
