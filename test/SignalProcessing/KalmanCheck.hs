{-# LANGUAGE BangPatterns #-}

module Main where

import SignalProcessing.Kalman
import System.Exit (exitFailure, exitSuccess)
import Control.Monad (unless, when)
import Text.Printf (printf)

-- | Assert condition, print message, exit on failure
assert :: String -> Bool -> IO ()
assert msg condition = do
    if condition
        then printf "[PASS] %s\n" msg
        else do
            printf "[FAIL] %s\n" msg
            exitFailure

-- | Extract Position from State (V3 x y z -> x)
getPos :: KalmanState -> Double
getPos st = case x st of
    V3 pos _ _ -> pos

-- | Extract Velocity
getVel :: KalmanState -> Double
getVel st = case x st of
    V3 _ vel _ -> vel

main :: IO ()
main = do
    putStrLn "Running Kalman Filter Verification (P0-001)..."

    ----------------------------------------------------------------------------
    -- Test 1: Convergence on Static Value
    ----------------------------------------------------------------------------
    let config1 = KalmanConfig { procNoise = 0.001, measNoise = 0.1 }
    let startState1 = initKalman 10.0 config1
    let steps1 = replicate 100 (0.033, 10.0) -- 33ms dt, measurement 10.0

    let finalState1 = foldl (\st (dt, meas) ->
            update meas config1 (predict dt config1 st)) startState1 steps1

    let pos1 = getPos finalState1
    let vel1 = getVel finalState1

    assert "Static Convergence: Position near 10.0" (abs (pos1 - 10.0) < 0.1)
    assert "Static Convergence: Velocity near 0.0" (abs vel1 < 0.1)

    ----------------------------------------------------------------------------
    -- Test 2: RMSE on Noisy Sine Wave
    ----------------------------------------------------------------------------
    let config2 = KalmanConfig { procNoise = 1.0, measNoise = 0.5 }
    let dt = 0.033
    let totalTime = 5.0
    let timeSteps = [0, dt .. totalTime]

    -- Ground Truth: 10mm amplitude, 0.25Hz freq
    let trueSignal t = 10.0 * sin (2 * pi * 0.25 * t)

    -- Deterministic Noise
    let noise t = 2.0 * sin (2 * pi * 10 * t) * cos (2 * pi * 7 * t)
    let noisySignal t = trueSignal t + noise t

    let measurements = map (\t -> (t, noisySignal t)) timeSteps
    let startState2 = initKalman (noisySignal 0) config2

    let (finalState2, mseSum) = foldl (\(st, acc) (t, meas) ->
            let predSt = predict dt config2 st
                updSt  = update meas config2 predSt
                estimPos = getPos updSt
                errSq = (estimPos - trueSignal t) ** 2
            in (updSt, acc + errSq)
            ) (startState2, 0.0) measurements

    let mse = mseSum / fromIntegral (length timeSteps)
    let rmse = sqrt mse

    printf "RMSE: %.4f mm\n" rmse
    assert "RMSE < 1.0mm" (rmse < 1.0)

    ----------------------------------------------------------------------------
    -- Test 3: Safety (NaN/Inf)
    ----------------------------------------------------------------------------
    let config3 = KalmanConfig 0.1 0.1
    let st3 = initKalman 10.0 config3
    let stNan = update (0/0) config3 st3

    assert "Safety: Reject NaN Measurement" (stNan == st3)

    let stInf = update (1/0) config3 st3
    assert "Safety: Reject Infinity Measurement" (stInf == st3)

    let stSafe = safeUpdate (0/0) config3 st3
    assert "Safety: safeUpdate handles exceptions/NaN" (stSafe == st3)

    putStrLn "All Tests Passed."
    exitSuccess
