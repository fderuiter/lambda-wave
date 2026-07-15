-- Requirement FR-GAT-002
module Main (main) where

import Control.Concurrent.STM
import qualified Data.Map.Strict as Map
import qualified Data.HashMap.Strict as HM
import qualified Data.ByteString as B
import Data.Time.HighRes (getMonotonicTimeNS)
import Control.Monad (forM_)
import Text.Printf (printf)

import Data.Types
import Data.Config (targetHeight)
import SignalProcessing.Kalman (initKalman, KalmanConfig(..))
import Control.Gating (processFrame)

main :: IO ()
main = do
    putStrLn "Running Latency Benchmark (Custom implementation)..."

    -- Setup
    t <- getMonotonicTimeNS
    let kConfig = KalmanConfig 10.0 2.0
    let kState = initKalman targetHeight kConfig
    q <- newTBQueueIO 20000
    audioQ <- newTBQueueIO 100
    let s = SystemState
            { currentPoints = []
            , beamState = BeamOff
            , lastFrameTime = t
            , sequenceNumber = 0
            , isocenter = Point3D 0 0 0 0 0
            , threadHeartbeats = Map.empty
            , kalmanState = kState
            , mtiState = []
            , auditQueue = q
            , audioQueue = audioQ
            , audioAlertEnabled = False
            , audioVolume = 1.0
            , audioFrequency = 440.0
            , activeLanguage = "en"
            , localizedBeamState = "Off"
            , calibrationStatus = CalibrationValid
            , displayPreset = StandardPreset
            }
    var <- newTVarIO s

    let pts = [Point3D (fromIntegral i) 0.0 10.0 0.0 10.0 | i <- [0..(100 :: Int)]]
    let frame = RadarFrame B.empty 1 pts
    let translations = HM.empty

    -- Warmup
    forM_ [1..1000 :: Int] $ \_ -> processFrame translations var frame

    -- Bench
    let iterations = 10000 :: Int
    start <- getMonotonicTimeNS
    forM_ [1..iterations] $ \_ -> processFrame translations var frame
    end <- getMonotonicTimeNS

    let totalNS = end - start
    let avgNS = fromIntegral totalNS / fromIntegral iterations :: Double
    let avgMS = avgNS / 1_000_000.0

    printf "Iterations: %d\n" iterations
    printf "Total Time: %d ns\n" totalNS
    printf "Average Latency: %.4f ms\n" avgMS

    if avgMS < 15.0
        then putStrLn "PASS: Latency < 15ms"
        else error $ "FAIL: Latency " ++ show avgMS ++ "ms > 15ms"
