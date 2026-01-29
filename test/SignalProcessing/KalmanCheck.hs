{-|
Module      : KalmanCheck
Description : Standalone verification script for Kalman Filter
Copyright   : (c) 2024-2026 Frederick de Ruiter, Ayoola Okuribido

NOTE: This script replaces the standard 'SignalProcessing.KalmanSpec' because
the development environment lacks the 'hspec' and 'hmatrix' dependencies required
for the standard test suite. This script uses only 'base' and verifies the
correctness of the zero-dependency 'SignalProcessing.Kalman' implementation.
-}

module Main where

import SignalProcessing.Kalman
import System.Exit (exitFailure, exitSuccess)
import Control.Monad (unless)
import Text.Printf (printf)

-- Helper to access V3 elements
getX :: V3 -> Double
getX (V3 x _ _) = x

getY :: V3 -> Double
getY (V3 _ y _) = y

assert :: String -> Bool -> IO ()
assert name cond = do
    if cond
        then printf "  [PASS] %s\n" name
        else do
            printf "  [FAIL] %s\n" name
            exitFailure

main :: IO ()
main = do
    putStrLn "Verifying SignalProcessing.Kalman..."

    testConvergence
    testRMSE
    testSingular
    testMatrixProperties

    putStrLn "All Tests Passed."
    exitSuccess

testConvergence :: IO ()
testConvergence = do
    putStrLn "Test: Convergence on Static Value"
    let config = KalmanConfig { procNoise = 0.001, measNoise = 0.1 }
    let startState = initKalman 10.0 config
    let steps = replicate 100 (0.033, 10.0)
    let finalState = foldl (\st (dt, meas) -> update meas config (predict dt config st)) startState steps

    let pos = getX (x finalState)
    let vel = getY (x finalState)

    assert "Position converges to ~10.0" (abs (pos - 10.0) < 0.1)
    assert "Velocity decays to ~0.0" (abs vel < 0.1)

testRMSE :: IO ()
testRMSE = do
    putStrLn "Test: RMSE on Noisy Sine Wave"
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
                estimPos = getX (x updSt)
                errSq = (estimPos - trueSignal t) ** 2
            in (updSt, acc + errSq)
            ) (startState, 0.0) measurements

    let mse = snd results / fromIntegral (length steps)
    let rmse = sqrt mse

    printf "  RMSE: %.4f\n" rmse
    assert "RMSE < 1.0" (rmse < 1.0)

testSingular :: IO ()
testSingular = do
    putStrLn "Test: Singular Covariance Safety"
    let config = KalmanConfig { procNoise = 0.0, measNoise = 0.0 }
    let st = initKalman 10.0 config
    let st' = update 11.0 config st

    assert "State Unchanged" (x st' == x st && p st' == p st)

testMatrixProperties :: IO ()
testMatrixProperties = do
    putStrLn "Test: Matrix Properties"
    -- Check if P stays symmetric
    let config = KalmanConfig { procNoise = 0.1, measNoise = 0.1 }
    let st = initKalman 0.0 config
    let st' = update 1.0 config (predict 0.033 config st)

    let (M33 r1 r2 r3) = p st'
    let (V3 p00 p01 p02) = r1
    let (V3 p10 p11 p12) = r2
    let (V3 p20 p21 p22) = r3

    assert "Symmetry (0,1 vs 1,0)" (abs (p01 - p10) < 1e-10)
    assert "Symmetry (0,2 vs 2,0)" (abs (p02 - p20) < 1e-10)
    assert "Symmetry (1,2 vs 2,1)" (abs (p12 - p21) < 1e-10)
