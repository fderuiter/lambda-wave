{-# LANGUAGE StrictData #-}
module Main where

-- Removing Numeric.LinearAlgebra dependency
-- import Numeric.LinearAlgebra
import SignalProcessing.Kalman
import System.Exit (exitFailure, exitSuccess)
import Control.Monad (unless, when)

-- Tolerance for float comparison
epsilon :: Double
epsilon = 1e-6

assert :: Bool -> String -> IO ()
assert condition msg = do
    unless condition $ do
        putStrLn $ "FAILED: " ++ msg
        exitFailure
    putStrLn $ "PASSED: " ++ msg

-- Helpers for V3 access since we don't have hmatrix "!" operator
getIdx :: V3 -> Int -> Double
getIdx (V3 v0 _ _) 0 = v0
getIdx (V3 _ v1 _) 1 = v1
getIdx (V3 _ _ v2) 2 = v2
getIdx _ _ = 0.0

main :: IO ()
main = do
    putStrLn "Running KalmanCheck (Zero-Dep)..."

    testStaticConvergence
    testRMSE
    testNaNRejection
    testInfRejection
    testSingularityHandling

    putStrLn "All tests passed."
    exitSuccess

testStaticConvergence :: IO ()
testStaticConvergence = do
    let config = KalmanConfig { procNoise = 0.001, measNoise = 0.1 }
    let startState = initKalman 10.0 config
    let steps = replicate 100 (0.033, 10.0)
    let finalState = foldl (\st (dt, meas) ->
            update meas config (predict dt config st)) startState steps

    let pos = getIdx (x finalState) 0
    let vel = getIdx (x finalState) 1

    assert (abs (pos - 10.0) < 0.1) "Static Convergence: Position"
    assert (abs vel < 0.1) "Static Convergence: Velocity"

testRMSE :: IO ()
testRMSE = do
    let config = KalmanConfig { procNoise = 1.0, measNoise = 0.5 }
    let dt = 0.033
    let totalTime = 5.0
    let steps = [0, dt .. totalTime]

    let trueSignal t = 10.0 * sin (2 * pi * 0.25 * t)
    let noise t = 2.0 * sin (2 * pi * 10 * t) * cos (2 * pi * 7 * t)
    let noisySignal t = trueSignal t + noise t

    let measurements = map (\t -> (t, noisySignal t)) steps
    let startState = initKalman (noisySignal 0) config

    let results = foldl (\(st, acc) (t, meas) ->
            let predSt = predict dt config st
                updSt  = update meas config predSt
                estimPos = getIdx (x updSt) 0
                errSq = (estimPos - trueSignal t) ** 2
            in (updSt, acc + errSq)
            ) (startState, 0.0) measurements

    let mse = snd results / fromIntegral (length steps)
    let rmse = sqrt mse

    putStrLn $ "RMSE: " ++ show rmse
    assert (rmse < 1.0) "RMSE < 1.0mm"

testNaNRejection :: IO ()
testNaNRejection = do
    let config = KalmanConfig 0.1 0.1
    let st = initKalman 10.0 config
    let st' = update (0/0) config st
    assert (st == st') "Reject NaN Measurement"

testInfRejection :: IO ()
testInfRejection = do
    let config = KalmanConfig 0.1 0.1
    let st = initKalman 10.0 config
    let st' = update (1/0) config st
    assert (st == st') "Reject Infinity Measurement"

testSingularityHandling :: IO ()
testSingularityHandling = do
    let config = KalmanConfig 0.1 0.1
    let st = initKalman 10.0 config
    -- Trigger singularity: P=0, R=0 -> S=0
    let configZero = KalmanConfig { procNoise = 0.0, measNoise = 0.0 }
    let stZero = initKalman 10.0 configZero

    -- update should detect S=0 and return state unchanged
    let st' = update 11.0 configZero stZero

    assert (st' == stZero) "Singularity returns previous state"
