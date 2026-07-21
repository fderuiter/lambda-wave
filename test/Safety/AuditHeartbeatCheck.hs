{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import Control.Concurrent (forkIO, killThread, threadDelay)
import Control.Concurrent.STM
import Control.Exception (IOException, catch)
import Data.Config (targetHeight)
import qualified Data.Map.Strict as Map
import Data.Time.HighRes (getMonotonicTimeNS)
import Data.Types
import Safety.Audit (auditLoop)
import SignalProcessing.Kalman (KalmanConfig (..), initKalman)
import System.Exit (exitFailure, exitSuccess)
import System.Posix.Files (removeLink)

-- | Dummy State
mkState :: IO (TVar SystemState)
mkState = do
  q <- newTBQueueIO 10000
  audioQ <- newTBQueueIO 100
  let kConfig = KalmanConfig {procNoise = 10.0, measNoise = 2.0}
  let kState = initKalman targetHeight kConfig
  let s0 =
        SystemState
          { currentPoints = [],
            beamState = BeamOff,
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
            calibrationStatus = CalibrationUnverified,
            displayPreset = StandardPreset,
            audioQueue = audioQ,
            audioVolume = 1.0,
            audioFrequency = 440.0
          }
  newTVarIO s0

main :: IO ()
main = do
  putStrLn "Running Audit Heartbeat Check..."

  stateVar <- mkState

  -- Cleanup old log
  let logPath = "test_audit.log"
  catch (removeLink logPath) (\(_ :: IOException) -> return ())

  -- Start Audit Loop
  tid <- forkIO $ auditLoop stateVar logPath

  -- Wait 200ms (simulate idle system)
  -- Watchdog timeout is usually ~100ms or 1s depending on config.
  -- Here we just check if heartbeat updates.
  threadDelay 200000

  -- Check Heartbeat
  s <- readTVarIO stateVar
  let hbMap = threadHeartbeats s
  now <- getMonotonicTimeNS

  _ <- case Map.lookup "Audit" hbMap of
    Nothing -> do
      putStrLn "FAILURE: No heartbeat recorded for Audit thread."
      exitFailure
    Just lastHb -> do
      let diffNS = now - lastHb
      let diffMS = fromIntegral diffNS / 1_000_000.0 :: Double
      putStrLn $ "Last Heartbeat age: " ++ show diffMS ++ " ms"

      -- If auditLoop blocks on readTBQueue, it updates heartbeat ONCE at start, then blocks.
      -- So age should be ~200ms.
      -- If fixed, it should wake up periodically (e.g. every 100ms) and update.
      -- So age should be < 150ms (allowing for jitter).

      if diffMS > 150.0
        then do
          putStrLn "FAILURE: Heartbeat is stale! Audit thread blocked on empty queue."
          exitFailure
        else do
          putStrLn "SUCCESS: Heartbeat is fresh."
          exitSuccess

  killThread tid
