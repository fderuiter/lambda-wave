module Main (main) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.STM
import qualified Data.Map.Strict as Map
import Data.Types
import Data.Time.HighRes (getMonotonicTimeNS)
import SignalProcessing.Kalman (initKalman, KalmanConfig(..))
import Safety.Watchdog (checkWatchdogInit, checkWatchdog)

main :: IO ()
main = runMain

runMain :: IO ()
runMain = do
    putStrLn "=== Watchdog Fault Injection Test ==="

    -- 1. Setup State
    now <- getMonotonicTimeNS
    let kConfig = KalmanConfig 0.1 0.1
    let kState = initKalman 0.0 kConfig
    q <- newTBQueueIO 100
    
    checkWatchdogInit

    -- Initialize with "TestThread" heartbeat = now
    let heartbeats = Map.fromList [("TestThread", now)]
    let initialState = SystemState [] BeamOff now 0 (Point3D 0 0 0 0 0) heartbeats kState q False
    stateVar <- newTVarIO initialState

    -- Log stall start
    stallStart <- getMonotonicTimeNS
    putStrLn $ "STALL_START_NS: " ++ show stallStart

    -- 2. Sleep for 200ms (Watchdog timeout is 100ms)
    -- This simulates the "TestThread" being frozen (not updating heartbeat)
    threadDelay 200000

    -- 3. Run checkWatchdog manually
    -- checkWatchdog should trip and kill us using exitImmediately!
    checkWatchdog stateVar

    -- 4. If we are here, Watchdog FAILED to kill us
    putStrLn "SURVIVED"

