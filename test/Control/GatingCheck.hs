{-# LANGUAGE BangPatterns #-}
module Main (main) where

import System.Exit (exitFailure, exitSuccess)
import Control.Monad () -- kept as suggested if instances needed, otherwise remove. But warning said redundant. Remove it.
import SignalProcessing.Kalman (KalmanState(..), V3(..), M33(..))
import Data.Types (BeamState(..))
import qualified Control.Gating as Gating

-- | Mock implementation or imports if Gating isn't ready
-- Since Control.Gating is not yet updated, we might need to rely on the plan to update it.
-- But for "Test First", we write the test expecting the API.

main :: IO ()
main = do
    putStrLn "Running Gating Logic Verification..."

    let failures = concat
            [ testHysteresis
            , testLatencyCompensation
            , testSafety
            ]

    if null failures
        then do
            putStrLn "All Checks Passed."
            exitSuccess
        else do
            putStrLn $ "Failures detected:\n" ++ unlines failures
            exitFailure

-- | Helper to create a dummy state
mkState :: Double -> Double -> KalmanState
mkState pos vel = KalmanState
    { x = V3 pos vel 0
    , p = M33 (V3 0 0 0) (V3 0 0 0) (V3 0 0 0)
    }

-- | Test Hysteresis Logic
-- Target: 10.0, Tol: 3.0.
-- Range: [7.0, 13.0] is safe.
-- Hysteresis: 0.5. OFF threshold: > 3.5 error.
testHysteresis :: [String]
testHysteresis =
    let target = 10.0
        tol = 3.0
        hyst = 0.5
        lat = 0.0 -- No latency for this test

        -- Helper
        eval = Gating.evaluateGating target tol hyst lat

        -- Case 1: Inside Tolerance -> ON
        s1 = mkState 10.0 0.0
        r1 = eval s1 BeamOff

        -- Case 2: Just outside Tolerance (3.1 error), was OFF -> OFF
        s2 = mkState 13.1 0.0
        r2 = eval s2 BeamOff

        -- Case 3: Just outside Tolerance (3.1 error), was ON -> ON (Hysteresis)
        s3 = mkState 13.1 0.0
        r3 = eval s3 BeamOn

        -- Case 4: Far outside (3.6 error), was ON -> OFF (Exceeds Hysteresis)
        s4 = mkState 13.6 0.0
        r4 = eval s4 BeamOn

    in catMaybes
        [ check "Inside Tol -> ON" BeamOn r1
        , check "Outside Tol (Fresh) -> OFF" BeamOff r2
        , check "Outside Tol (Held) -> ON" BeamOn r3
        , check "Far Outside -> OFF" BeamOff r4
        ]

-- | Test Latency Compensation
-- Target: 10.0. Pos: 13.1 (Outside). Vel: -10 (Moving towards target).
-- Latency: 0.05s.
-- PredPos = 13.1 + (-10 * 0.05) = 13.1 - 0.5 = 12.6.
-- 12.6 is inside [7, 13]. Should be ON.
testLatencyCompensation :: [String]
testLatencyCompensation =
    let target = 10.0
        tol = 3.0
        hyst = 0.0
        lat = 50000000.0 -- 50ms in NS

        eval = Gating.evaluateGating target tol hyst lat

        -- Moving towards target, latency comp puts it inside
        s1 = mkState 13.1 (-10.0)
        r1 = eval s1 BeamOff -- Should turn ON

        -- Moving away. Pos 12.9 (Inside). Vel 10.
        -- PredPos = 12.9 + 0.5 = 13.4 (Outside).
        s2 = mkState 12.9 10.0
        r2 = eval s2 BeamOn -- Should turn OFF

    in catMaybes
        [ check "Latency Comp (Inbound)" BeamOn r1
        , check "Latency Comp (Outbound)" BeamOff r2
        ]

testSafety :: [String]
testSafety =
    let eval = Gating.evaluateGating 10.0 3.0 0.5 0.05
        nanState = mkState (0/0) 0
        infState = mkState (1/0) 0
    in catMaybes
        [ check "NaN State -> Off" BeamOff (eval nanState BeamOn)
        , check "Inf State -> Off" BeamOff (eval infState BeamOn)
        ]

check :: (Show a, Eq a) => String -> a -> a -> Maybe String
check name expected actual
    | expected == actual = Nothing
    | otherwise = Just $ name ++ ": Expected " ++ show expected ++ ", got " ++ show actual

catMaybes :: [Maybe a] -> [a]
catMaybes [] = []
catMaybes (Nothing:xs) = catMaybes xs
catMaybes (Just val:xs) = val : catMaybes xs

-- Requirement FR-GAT-001
