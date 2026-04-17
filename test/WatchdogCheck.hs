module Main (main) where

import Control.Concurrent.STM
import qualified Data.Map.Strict as Map
import Data.Types
import Control.Gating (processFrame)
import Data.Time.HighRes (getMonotonicTimeNS)
import SignalProcessing.Kalman (initKalman, KalmanConfig(..))

main :: IO ()
main = do
    putStrLn "=== Watchdog Logic Verification (P0-002) ==="

    -- 1. Setup State
    now <- getMonotonicTimeNS
    -- Initialize with default kalman state
    let kConfig = KalmanConfig 0.1 0.1
    let kState = initKalman 0.0 kConfig
    q <- newTBQueueIO 100
    let initialState = SystemState [] BeamOff now (Point3D 0 0 0 0 0) Map.empty kState q False
    stateVar <- newTVarIO initialState

    -- 2. Run Gating Process (which should update heartbeat)
    -- We pass empty points list
    processFrame stateVar []

    -- 3. Verify Heartbeat
    finalState <- readTVarIO stateVar
    let heartbeats = threadHeartbeats finalState
    case Map.lookup "Gating" heartbeats of
        Just t -> do
            if t >= now
               then putStrLn "PASS: Gating updated heartbeat."
               else putStrLn $ "FAIL: Heartbeat timestamp is old. (Now: " ++ show now ++ ", HB: " ++ show t ++ ")"
        Nothing -> putStrLn "FAIL: No heartbeat for 'Gating'."
