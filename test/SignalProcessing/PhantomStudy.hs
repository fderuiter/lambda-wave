{-# LANGUAGE BangPatterns #-}

module Main (main) where

import SignalProcessing.Kalman
import Data.List (foldl')
import Text.Printf (printf)
import System.Exit (exitFailure)

-- | Phantom Study Parameters
amplitude :: Double
amplitude = 10.0 -- mm

period :: Double
period = 4.0 -- seconds

frequency :: Double
frequency = 1.0 / period

dt :: Double
dt = 0.033 -- 33ms sampling interval

totalTime :: Double
totalTime = 20.0 -- 5 cycles

-- | Ground Truth Signal (Encoder Log)
-- y(t) = A * sin(2 * pi * f * t)
trueSignal :: Double -> Double
trueSignal t = amplitude * sin (2 * pi * frequency * t)

-- | Simulated Radar Noise
-- Deterministic pseudo-random noise
noise :: Double -> Double
noise t = 1.5 * sin (2 * pi * 13 * t) * cos (2 * pi * 7 * t) + 0.5 * sin(2 * pi * 53 * t)

-- | Pearson Correlation Coefficient
correlation :: [Double] -> [Double] -> Double
correlation xs ys =
    let n = fromIntegral (length xs)
        sumX = sum xs
        sumY = sum ys
        sumX2 = sum (map (^ (2::Int)) xs)
        sumY2 = sum (map (^ (2::Int)) ys)
        sumXY = sum (zipWith (*) xs ys)

        numerator = n * sumXY - sumX * sumY
        denomX = sqrt (n * sumX2 - sumX ^ (2::Int))
        denomY = sqrt (n * sumY2 - sumY ^ (2::Int))
    in numerator / (denomX * denomY)

main :: IO ()
main = do
    putStrLn "=== Phase 6.1: Simulated Phantom Study (PR-ACC-01) ==="

    let timeSteps = [0, dt .. totalTime]
    let measurements = map (\t -> (t, trueSignal t + noise t)) timeSteps

    -- Initialize Kalman Filter
    -- We start with the first noisy measurement
    z0 <- case measurements of
            ((_, val):_) -> return val
            [] -> do
                putStrLn "Measurements list is empty"
                exitFailure

    let config = KalmanConfig { procNoise = 2.0, measNoise = 1.0 }
    let startState = initKalman z0 config

    -- Run Simulation
    let (_, estimatesRev) = foldl' (\(!st, !acc) (_, z) ->
            let predSt = predict dt config st
                updSt  = update z config predSt
                (V3 pos _ _) = x updSt
            in (updSt, pos : acc)
            ) (startState, []) measurements

    let estimates = reverse estimatesRev

    -- Calculate Correlation
    let groundTruth = map trueSignal timeSteps
    let r = correlation groundTruth estimates

    putStrLn $ printf "Total Frames: %d" (length timeSteps)
    putStrLn $ printf "Correlation Coefficient (r): %.6f" r

    if r > 0.98
        then putStrLn "PASS: Correlation > 0.98"
        else do
            putStrLn "FAIL: Correlation <= 0.98"
            exitFailure
