module Main (main) where

import Control.Concurrent (forkIO, killThread, threadDelay)
import Control.Concurrent.STM
import Control.Gating (processFrame)
import Control.Monad (forever, replicateM_)
import Data.Config (targetHeight)
import qualified Data.HashMap.Strict as HM
import Data.IORef
import Data.Map.Strict as Map
import Data.Types
import SignalProcessing.Kalman (KalmanConfig (..), initKalman)
import System.Exit (exitFailure, exitSuccess)

-- | Dummy State
mkState :: BeamState -> IO SystemState
mkState bs = do
  q <- newTBQueueIO 10000
  audioQ <- newTBQueueIO 100
  let kConfig = KalmanConfig {procNoise = 10.0, measNoise = 2.0}
  let kState = initKalman targetHeight kConfig
  return $
    SystemState
      { currentPoints = [],
        beamState = bs,
        lastFrameTime = 0,
        sequenceNumber = 0,
        isocenter = Point3D 0 0 0 0 0,
        threadHeartbeats = Map.empty,
        kalmanState = kState,
        mtiState = [],
        auditQueue = q,
        audioAlertEnabled = False,
        activeLanguage = "en",
        localizedBeamState = "BEAM OFF",
        calibrationStatus = CalibrationValid,
        displayPreset = StandardPreset,
        audioQueue = audioQ,
        audioVolume = 1.0,
        audioFrequency = 440.0
      }

main :: IO ()
main = do
  putStrLn "Running BeamHold Race Condition Check..."

  s0 <- mkState BeamOn -- Start ON
  stateVar <- newTVarIO s0

  raceCount <- newIORef (0 :: Int)

  userThread <- forkIO $ forever $ do
    threadDelay 1000 -- 1ms
    atomically $ modifyTVar' stateVar $ \s -> s {beamState = BeamHold}

  let pts = [Point3D 10.0 0 0 0 0]
  let frame = RadarFrame "" 0 pts

  consumerThread <- forkIO $ forever $ do
    processFrame HM.empty stateVar frame
    threadDelay 1000

  replicateM_ 100 $ do
    threadDelay 10000 -- 10ms
    s <- readTVarIO stateVar
    let bs = beamState s
    case bs of
      BeamOn -> modifyIORef raceCount (+ 1)
      _ -> return ()

  killThread userThread
  killThread consumerThread

  count <- readIORef raceCount
  if count > 50
    then do
      putStrLn "FAILURE: BeamOn persisted despite User holding. Race Condition Likely."
      exitFailure
    else do
      putStrLn "SUCCESS: BeamHold seems respected."
      exitSuccess
