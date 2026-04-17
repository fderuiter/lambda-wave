{-# LANGUAGE BangPatterns #-}
module Main (main) where

import Control.Concurrent (forkIO, threadDelay, killThread)
import Control.Concurrent.STM
import Control.Monad (forever, replicateM_)
import Data.Map.Strict as Map
import Data.IORef
import System.Exit (exitFailure, exitSuccess)

import Data.Types
import Control.Gating (processFrame)
import SignalProcessing.Kalman (KalmanState(..), V3(..), M33(..), KalmanConfig(..), initKalman)
import Data.Config (targetHeight)

-- | Dummy State
mkState :: BeamState -> IO SystemState
mkState bs = do
    q <- newTBQueueIO 100
    let kConfig = KalmanConfig { procNoise = 10.0, measNoise = 2.0 }
    let kState = initKalman targetHeight kConfig
    return $ SystemState
        { currentPoints = []
        , beamState = bs
        , lastFrameTime = 0
        , isocenter = Point3D 0 0 0 0 0
        , threadHeartbeats = Map.empty
        , kalmanState = kState
        , auditQueue = q
        , audioAlertEnabled = False
        }

main :: IO ()
main = do
    putStrLn "Running BeamHold Race Condition Check..."

    s0 <- mkState BeamOn -- Start ON
    stateVar <- newTVarIO s0

    raceCount <- newIORef (0 :: Int)

    -- Thread 1: The "User" - Sets BeamHold
    -- We want to set it and check if it gets overwritten.
    userThread <- forkIO $ forever $ do
        threadDelay 1000 -- 1ms
        atomically $ modifyTVar' stateVar $ \s -> s { beamState = BeamHold }

    -- Thread 2: The "Consumer" - Calls processFrame
    -- We feed it points that would keep it ON (Target 10.0, Input 10.0)
    -- If it reads ON, it calculates ON.
    -- If user sets HOLD in between, it overwrites with ON.
    let points = [Point3D 10.0 0 0 0 0]

    consumerThread <- forkIO $ forever $ do
        processFrame stateVar points
        -- threadDelay 0 -- Run as fast as possible to maximize race chance

    -- Monitor
    -- Check frequently. If state becomes ON while User is spamming HOLD, we have a race.
    -- But since User spams HOLD every 1ms, and Consumer runs tight loop,
    -- Consumer likely runs 100 times in 1ms.
    -- It will overwrite HOLD immediately.
    -- So we should see ON most of the time.

    replicateM_ 100 $ do
        threadDelay 10000 -- 10ms
        s <- readTVarIO stateVar
        let bs = beamState s
        -- If we see BeamOn, it means the HOLD was overwritten (or not yet set).
        -- But since user sets it every 1ms, we expect to see HOLD sometimes.
        -- If Consumer overwrites it instantly, we might never see HOLD.
        putStrLn $ "State: " ++ show bs
        case bs of
             BeamOn -> modifyIORef raceCount (+1)
             _ -> return ()

    killThread userThread
    killThread consumerThread

    count <- readIORef raceCount
    putStrLn $ "Seen BeamOn " ++ show count ++ " times (out of 100 checks)."

    -- If we see BeamOn consistently, it means the race is real:
    -- User sets HOLD, Consumer overwrites with ON.
    -- With the fix, once HOLD is set, Consumer should respect it (and set it to HOLD, or keep it HOLD).
    -- Wait, with the fix:
    -- Consumer reads ON. Calculates ON.
    -- User sets HOLD.
    -- Consumer sees HOLD. Updates to HOLD.
    -- So state stays HOLD.
    -- So with fix, we should see predominantly HOLD (or at least alternating properly, never sticking to ON).
    -- If we see ON a lot, it implies overwrite.

    -- A better check:
    -- User sets HOLD *once*. Then stops.
    -- Consumer runs *once* (slowly).
    -- We check if HOLD survived.
    -- But we can't control scheduling.

    -- Let's just run the test and see failure.
    if count > 50
        then do
            putStrLn "FAILURE: BeamOn persisted despite User holding. Race Condition Likely."
            exitFailure
        else do
            putStrLn "SUCCESS: BeamHold seems respected."
            exitSuccess
