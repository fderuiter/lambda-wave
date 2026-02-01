{-# LANGUAGE BangPatterns #-}

module Main where

import SignalProcessing.Kalman
import Data.List (foldl')
import Text.Printf (printf)
import Control.Monad (unless, when)

-- | Test Convergence on Static Signal
testConvergence :: IO Bool
testConvergence = do
    putStr "Test 1: Convergence on Static Signal... "
    let config = KalmanConfig { procNoise = 0.001, measNoise = 0.1 }
    let startState = initKalman 10.0 config

    -- Simulate 100 frames of static measurement 10.0
    let steps = replicate 100 (0.033, 10.0) -- 33ms dt
    let finalState = foldl' (\st (dt, meas) ->
            update meas config (predict dt config st)) startState steps

    let (V3 pos vel _) = x finalState

    let posOk = abs (pos - 10.0) < 0.1
    let velOk = abs vel < 0.1

    if posOk && velOk
        then putStrLn "PASS" >> return True
        else do
            putStrLn $ printf "FAIL (Pos: %.4f, Vel: %.4f)" pos vel
            return False

-- | Test RMSE < 1.0mm on Noisy Sine Wave
testRMSE :: IO Bool
testRMSE = do
    putStr "Test 2: RMSE < 1.0mm on Noisy Sine Wave... "

    let config = KalmanConfig { procNoise = 1.0, measNoise = 0.5 }
    let dt = 0.033
    let totalTime = 5.0
    let steps = [0, dt .. totalTime]

    -- Ground Truth: 10mm amplitude, 0.25Hz freq
    let trueSignal t = 10.0 * sin (2 * pi * 0.25 * t)

    -- Deterministic "Noise": 2 * sin(10t) * cos(7t)
    let noise t = 2.0 * sin (2 * pi * 10 * t) * cos (2 * pi * 7 * t)
    let noisySignal t = trueSignal t + noise t

    let measurements = map (\t -> (t, noisySignal t)) steps
    let startState = initKalman (noisySignal 0) config

    -- Run Filter
    let results = foldl' (\(!st, !accSqErr) (t, meas) ->
            let predSt = predict dt config st
                updSt  = update meas config predSt
                (V3 estimPos _ _) = x updSt
                errSq = (estimPos - trueSignal t) ** 2
            in (updSt, accSqErr + errSq)
            ) (startState, 0.0) measurements

    let mse = snd results / fromIntegral (length steps)
    let rmse = sqrt mse

    if rmse < 1.0
        then putStrLn (printf "PASS (RMSE: %.4f mm)" rmse) >> return True
        else putStrLn (printf "FAIL (RMSE: %.4f mm)" rmse) >> return False

-- | Test Safety (NaN/Inf Handling)
testSafety :: IO Bool
testSafety = do
    putStr "Test 3: Safety (NaN/Inf Handling)... "
    let config = KalmanConfig 0.1 0.1
    let st0 = initKalman 10.0 config

    -- Update with NaN
    let st1 = update (0/0) config st0
    let ok1 = st1 == st0

    -- Update with Infinity
    let st2 = update (1/0) config st0
    let ok2 = st2 == st0

    -- Predict with invalid dt
    let st3 = predict (-1.0) config st0
    let ok3 = st3 == st0

    if ok1 && ok2 && ok3
        then putStrLn "PASS" >> return True
        else putStrLn "FAIL (State corrupted by invalid input)" >> return False

-- | Simple LCG for deterministic pseudo-random numbers
-- Returns a list of doubles in range [-100, 100]
lcgStream :: Int -> [Double]
lcgStream seed = map toDouble (iterate next seed)
  where
    next s = (1664525 * s + 1013904223) `rem` (2^(32::Int))
    toDouble s = (fromIntegral s / fromIntegral (2^(32::Int))) * 200.0 - 100.0

-- | Property Test: Symmetry of Covariance Matrix
-- P must be symmetric (approx)
propSymmetry :: Double -> Bool
propSymmetry val =
    let config = KalmanConfig 0.1 2.0
        st = initKalman val config
        -- Run one step
        stPred = predict 0.033 config st
        stUpd  = update val config stPred
        (M33 (V3 p11 p12 p13) (V3 p21 p22 p23) (V3 p31 p32 p33)) = p stUpd

        tol = 1e-10
        sym12 = abs (p12 - p21) < tol
        sym13 = abs (p13 - p31) < tol
        sym23 = abs (p23 - p32) < tol
    in sym12 && sym13 && sym23

-- | Property Test: Linearity of Prediction (Zero Noise)
-- F(a*x) = a*F(x)
propLinearity :: Double -> Double -> Bool
propLinearity val scaleFactor =
    let config = KalmanConfig 0.0 0.0 -- Zero noise
        dt = 0.1
        st = initKalman val config

        -- State X
        (V3 x1 x2 x3) = x st

        -- Scaled State X'
        stScaled = st { x = V3 (scaleFactor*x1) (scaleFactor*x2) (scaleFactor*x3) }

        -- Predict(X')
        predScaled = predict dt config stScaled
        (V3 ps1 ps2 ps3) = x predScaled

        -- Predict(X) * scale
        predNormal = predict dt config st
        (V3 pn1 pn2 pn3) = x predNormal

        tol = 1e-10
        ok1 = abs (ps1 - scaleFactor*pn1) < tol
        ok2 = abs (ps2 - scaleFactor*pn2) < tol
        ok3 = abs (ps3 - scaleFactor*pn3) < tol
    in ok1 && ok2 && ok3

testProperties :: IO Bool
testProperties = do
    putStr "Test 4: QuickCheck Properties (Symmetry, Linearity)... "
    let seeds = [1..100]
    let measurements = take 100 (lcgStream 42)
    let scales = take 100 (lcgStream 99)

    let symResults = map propSymmetry measurements
    let linResults = zipWith propLinearity measurements scales

    let allSym = and symResults
    let allLin = and linResults

    if allSym && allLin
        then putStrLn "PASS" >> return True
        else putStrLn "FAIL" >> return False

main :: IO ()
main = do
    putStrLn "=== Kalman Filter Verification (P0-001) ==="
    p1 <- testConvergence
    p2 <- testRMSE
    p3 <- testSafety
    p4 <- testProperties

    putStrLn "-------------------------------------------"
    if p1 && p2 && p3 && p4
        then putStrLn "VERIFICATION PASSED"
        else fail "VERIFICATION FAILED"
