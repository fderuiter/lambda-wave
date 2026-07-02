{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE PatternSynonyms #-}

module Main (main) where

import SignalProcessing.Kalman
import Data.List (foldl')
import Text.Printf (printf)

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

    let (pos, vel) = case x finalState of
            V3 pVal vVal _ -> (pVal, vVal)
            _ -> (0, 0)

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
                estimPos = case x updSt of
                    V3 pVal _ _ -> pVal
                    _ -> 0.0
                err = estimPos - trueSignal t
                errSq = err * err -- ⚡ Bolt Optimization: Replace ** with * for performance
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
    toDouble s = (fromIntegral s / fromIntegral (2^(32::Int) :: Integer)) * 200.0 - 100.0

-- | Property Test: Symmetry of Covariance Matrix
-- P must be symmetric (approx)
propSymmetry :: Double -> Bool
propSymmetry val =
    let config = KalmanConfig 0.1 2.0
        st = initKalman val config
        -- Run one step
        stPred = predict 0.033 config st
        stUpd  = update val config stPred
        (p12, p13, p21, p23, p31, p32) = case p stUpd of
            M33 (V3 _ p12Val p13Val) (V3 p21Val _ p23Val) (V3 p31Val p32Val _) ->
                (p12Val, p13Val, p21Val, p23Val, p31Val, p32Val)
            _ -> (0, 0, 0, 0, 0, 0)

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
        (x1, x2, x3) = case x st of
            V3 x1Val x2Val x3Val -> (x1Val, x2Val, x3Val)
            _ -> (0, 0, 0)

        -- Scaled State X'
        stScaled = st { x = V3 (scaleFactor*x1) (scaleFactor*x2) (scaleFactor*x3) }

        -- Predict(X')
        predScaled = predict dt config stScaled
        (ps1, ps2, ps3) = case x predScaled of
            V3 ps1Val ps2Val ps3Val -> (ps1Val, ps2Val, ps3Val)
            _ -> (0, 0, 0)

        -- Predict(X) * scale
        predNormal = predict dt config st
        (pn1, pn2, pn3) = case x predNormal of
            V3 pn1Val pn2Val pn3Val -> (pn1Val, pn2Val, pn3Val)
            _ -> (0, 0, 0)

        tol = 1e-10
        ok1 = abs (ps1 - scaleFactor*pn1) < tol
        ok2 = abs (ps2 - scaleFactor*pn2) < tol
        ok3 = abs (ps3 - scaleFactor*pn3) < tol
    in ok1 && ok2 && ok3

testProperties :: IO Bool
testProperties = do
    putStr "Test 4: QuickCheck Properties (Symmetry, Linearity)... "
    -- Removed unused seeds
    let measurements = take 100 (lcgStream 42)
    let scales = take 100 (lcgStream 99)

    let symResults = map propSymmetry measurements
    let linResults = zipWith propLinearity measurements scales

    let allSym = and symResults
    let allLin = and linResults

    if allSym && allLin
        then putStrLn "PASS" >> return True
        else putStrLn "FAIL" >> return False

-- | Verification Bridge: Directly verify 3rd-order kinematic model invariants.
-- These properties are derived from the documented PVA state-transition model
-- and must hold regardless of any reformatting of the reference documentation.
testVerificationBridge :: IO Bool
testVerificationBridge = do
    putStr "Test 5: 3rd-Order Kinematic Invariants... "
    let config = KalmanConfig 0.1 0.1
        st0 = initKalman 0.0 config

    -- Invariant: given x = [pos=0, vel=2, acc=4] and dt=1s, the prediction
    -- must satisfy the constant-acceleration kinematic equations:
    --   pos' = pos + vel*dt + 0.5*acc*dt^2 = 0 + 2 + 2  = 4.0
    --   vel' = vel + acc*dt                = 2 + 4      = 6.0
    --   acc' = acc                                       = 4.0
    let stInit = st0 { x = V3 0.0 2.0 4.0 }
    let stPred = predict 1.0 config stInit
    let (pos, vel, acc) = case x stPred of
            V3 pVal vVal aVal -> (pVal, vVal, aVal)
            _                 -> (0, 0, 0)

    let posOk = abs (pos - 4.0) < 1e-9
    let velOk = abs (vel - 6.0) < 1e-9
    let accOk = abs (acc - 4.0) < 1e-9

    if posOk && velOk && accOk
        then putStrLn "PASS" >> return True
        else putStrLn "FAIL (3rd-Order Kinematic Invariants Violated by Implementation)" >> return False

main :: IO ()
main = do
    putStrLn "=== Kalman Filter Verification (P0-001) ==="
    p1 <- testConvergence
    p2 <- testRMSE
    p3 <- testSafety
    p4 <- testProperties
    p5 <- testVerificationBridge

    putStrLn "-------------------------------------------"
    if p1 && p2 && p3 && p4 && p5
        then putStrLn "VERIFICATION PASSED"
        else fail "VERIFICATION FAILED"

-- Requirement FR-DSP-003
