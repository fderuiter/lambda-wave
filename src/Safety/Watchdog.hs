{-|
Module      : Safety.Watchdog
Description : The "Dead Man's Switch" for Beam Safety
Copyright   : (c) 2024
License     : MIT
Maintainer  : atlas@code-cartographer.com

= The Watchdog 🐉

This module implements a high-priority thread that monitors the system's "heartbeat".
In a real-time safety-critical system (Class II/III), we cannot assume the software will always work.
We must assume it *will* fail, and ensure that when it does, it fails safely (Beam Off).

== Mechanism
1. The Gating Loop updates a 'lastFrameTime' timestamp in STM every time it completes a cycle.
2. This Watchdog thread wakes up every 10ms.
3. If 'lastFrameTime' is older than 'watchdogTimeoutNS' (e.g. 100ms), the Watchdog kills the process.

== Dragon 🐉
*   **Debugging:** If you pause the program with a debugger, this Watchdog WILL trip and kill your session.
    Disable it or increase the timeout when debugging.
-}
module Safety.Watchdog (watchdogLoop) where

import Data.Types
import Data.Config (watchdogTimeoutNS)
import Control.Concurrent.STM
import Control.Concurrent (threadDelay)
import Data.Time.HighRes (getMonotonicTimeNS)
import Control.Monad (forever, when, forM_)
import System.Exit (exitFailure)
import qualified Data.Map.Strict as Map

-- | The Watchdog Loop
-- Kills the process if any critical thread has not reported progress within the timeout.
watchdogLoop :: TVar SystemState -> IO ()
watchdogLoop stateVar = forever $ do
    now <- getMonotonicTimeNS
    state <- readTVarIO stateVar
    let heartbeats = threadHeartbeats state

    -- Iterate over all monitored threads
    forM_ (Map.toList heartbeats) $ \(threadName, lastTime) -> do
        let diff = now - lastTime

        -- Check if difference exceeds timeout (cast Integer to Word64 safely for comparison)
        -- watchdogTimeoutNS is Integer (100ms = 100_000_000). Word64 max is huge.
        when (diff > fromIntegral watchdogTimeoutNS) $ do
            putStrLn $ "!!! WATCHDOG TRIP: Thread '" ++ threadName ++ "' FROZEN !!!"
            putStrLn $ "!!! Time since last heartbeat: " ++ show diff ++ " ns"
            putStrLn "!!! FORCING BEAM OFF !!!"
            -- In real HW, this would toggle a GPIO pin immediately
            exitFailure

    threadDelay 10000 -- Check every 10ms
