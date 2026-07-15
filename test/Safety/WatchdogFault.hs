module Main (main) where

import Control.Concurrent (forkOS, threadDelay)
import Control.Concurrent.STM
import qualified Data.Map.Strict as Map
import Data.Types
import Data.Time.HighRes (getMonotonicTimeNS)
import SignalProcessing.Kalman (initKalman, KalmanConfig(..))
import Safety.Watchdog (watchdogLoop, runSafetyDaemon)
import System.Environment (getArgs, getExecutablePath)
import System.Process (spawnProcess)
import System.Posix.Process (getProcessID)
import System.Posix.Types (ProcessID)
import System.IO (hFlush, stdout, hSetBuffering, BufferMode(LineBuffering))

main :: IO ()
main = do
    hSetBuffering stdout LineBuffering
    args <- getArgs
    case args of
        ["--safety-daemon", parentPidStr] -> do
            let parentPid = read parentPidStr :: ProcessID
            runSafetyDaemon parentPid
        _ -> runMain

runMain :: IO ()
runMain = do
    putStrLn "=== Watchdog Fault Injection Test ==="
    hFlush stdout

    -- 1. Setup State
    now <- getMonotonicTimeNS
    let kConfig = KalmanConfig 0.1 0.1
    let kState = initKalman 0.0 kConfig
    q <- newTBQueueIO 10000
    audioQ <- newTBQueueIO 100
    -- Initialize with "TestThread" heartbeat = now
    let heartbeats = Map.fromList [("TestThread", now)]
    let initialState = SystemState
            { currentPoints = []
            , beamState = BeamOff
            , lastFrameTime = now
            , sequenceNumber = 0
            , isocenter = Point3D 0 0 0 0 0
            , threadHeartbeats = heartbeats
            , kalmanState = kState
            , mtiState = []
            , auditQueue = q
            , audioQueue = audioQ
            , audioAlertEnabled = False
            , audioVolume = 1.0
            , audioFrequency = 440.0
            , activeLanguage = "en"
            , localizedBeamState = "BEAM OFF"
            , calibrationStatus = CalibrationUnverified
            , displayPreset = StandardPreset
            }
    stateVar <- newTVarIO initialState

    -- 2. Spawn Safety Daemon
    exePath <- getExecutablePath
    putStrLn $ "EXE: " ++ exePath
    hFlush stdout
    myPid <- getProcessID
    _daemonPid <- spawnProcess exePath ["--safety-daemon", show myPid]
    
    -- Small delay to let Daemon bind socket
    threadDelay 50000

    -- 3. Fork Watchdog Loop (Heartbeat Sender)
    putStrLn "About to forkOS watchdogLoop"
    hFlush stdout
    _ <- forkOS $ watchdogLoop stateVar
    putStrLn "forkOS complete"
    hFlush stdout

    -- Log stall start
    stallStart <- getMonotonicTimeNS
    putStrLn $ "STALL_START_NS: " ++ show stallStart
    hFlush stdout

    -- 4. Sleep for 200ms (Watchdog timeout is 100ms)
    -- This simulates the "TestThread" being frozen (not updating heartbeat)
    -- Watchdog should stop sending heartbeats.
    -- Safety Daemon should trip and kill us!
    threadDelay 200000

    -- 5. If we are here, Watchdog FAILED to kill us
    putStrLn "SURVIVED"
