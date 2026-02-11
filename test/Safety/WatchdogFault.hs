module Main (main) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.STM
import qualified Data.Map.Strict as Map
import Data.Types
import Data.Time.HighRes (getMonotonicTimeNS)
import SignalProcessing.Kalman (initKalman, KalmanConfig(..))
import Safety.Watchdog (watchdogLoop)

main :: IO ()
main = do
    putStrLn "=== Watchdog Fault Injection Test ==="

    -- 1. Setup State
    now <- getMonotonicTimeNS
    let kConfig = KalmanConfig 0.1 0.1
    let kState = initKalman 0.0 kConfig
    q <- newTBQueueIO 100
    -- Initialize with "TestThread" heartbeat = now
    let heartbeats = Map.fromList [("TestThread", now)]
    let initialState = SystemState [] BeamOff now (Point3D 0 0 0 0 0) heartbeats kState q
    stateVar <- newTVarIO initialState

    -- 2. Fork Watchdog
    _ <- forkIO $ watchdogLoop stateVar

    -- 3. Sleep for 200ms (Watchdog timeout is 100ms)
    -- This simulates the "TestThread" being frozen (not updating heartbeat)
    -- Watchdog should kill the process during this sleep.
    threadDelay 200000

    -- 4. If we are here, Watchdog FAILED to kill us
    putStrLn "SURVIVED"
