module SignalProcessing.KalmanCheck (main) where

import SignalProcessing.Kalman
import Text.Printf (printf)
import System.Exit (exitFailure, exitSuccess)

-- Helper
assert :: String -> Bool -> IO ()
assert name cond = do
    if cond
        then printf "[PASS] %s\n" name
        else do
            printf "[FAIL] %s\n" name
            exitFailure

approx :: Double -> Double -> Double -> Bool
approx a b tol = abs (a - b) < tol

main :: IO ()
main = do
    putStrLn "Running Kalman Filter Verification (P0-001 / FR-DSP-003)..."

    testConvergence
    testRMSE
    testSafety

    putStrLn "All Kalman tests passed."
    exitSuccess

testConvergence :: IO ()
testConvergence = do
    let config = KalmanConfig { procNoise = 0.001, measNoise = 0.1 }
    let startState = initKalman 10.0 config
    let steps = replicate 100 (0.033, 10.0) -- 33ms dt

    let finalState = foldl (\st (dt, meas) ->
            update meas config (predict dt config st)) startState steps

    let (V3 pos vel _) = x finalState

    assert "Converges on static value (Pos)" $ approx pos 10.0 0.1
    assert "Converges on static value (Vel)" $ approx vel 0.0 0.1

testRMSE :: IO ()
testRMSE = do
    let config = KalmanConfig { procNoise = 1.0, measNoise = 0.5 }
    let dt = 0.033
    let totalTime = 5.0
    let steps = [0, dt .. totalTime]

    let trueSignal t = 10.0 * sin (2 * pi * 0.25 * t)
    let noise t = 2.0 * sin (2 * pi * 10 * t) * cos (2 * pi * 7 * t) -- Deterministic noise
    let noisySignal t = trueSignal t + noise t

    let measurements = map (\t -> (t, noisySignal t)) steps
    let startState = initKalman (noisySignal 0) config

    let (_, sumSqErr) = foldl (\(st, acc) (t, meas) ->
            let predSt = predict dt config st
                updSt  = update meas config predSt
                (V3 estimPos _ _) = x updSt
                errSq = (estimPos - trueSignal t) ** 2
            in (updSt, acc + errSq)
            ) (startState, 0.0) measurements

    let mse = sumSqErr / fromIntegral (length steps)
    let rmse = sqrt mse

    printf "RMSE: %.4f mm\n" rmse
    assert "RMSE < 1.0mm" (rmse < 1.0)

testSafety :: IO ()
testSafety = do
    let config = KalmanConfig 0.1 0.1
    let st = initKalman 10.0 config

    let stNaN = update (0/0) config st
    assert "Rejects NaN measurements" (x stNaN == x st)

    let stInf = update (1/0) config st
    assert "Rejects Infinity measurements" (x stInf == x st)

    let stSafe = safeUpdate (0/0) config st
    assert "safeUpdate handles exceptions" (x stSafe == x st)
