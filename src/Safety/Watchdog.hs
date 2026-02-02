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
import Control.Monad (forever, when)
import System.Exit (exitFailure)
import Hardware.Control (getMonotonicTimeNS)

-- | The Watchdog Loop
-- Kills the process if the main Gating Loop has not reported progress within the timeout.
watchdogLoop :: TVar SystemState -> IO ()
watchdogLoop stateVar = forever $ do
    now <- getMonotonicTimeNS
    lastTime <- lastFrameTime <$> readTVarIO stateVar

    -- lastFrameTime is now Word64 (ns), now is Word64 (ns)
    -- We need to handle potential wrap around or just standard substraction (unsigned)
    -- But since it's monotonic starting from boot/start, and Word64 is huge (584 years), we are fine.

    let diff = if now >= lastTime then now - lastTime else 0 -- Should not happen with monotonic clock unless restart

    when (diff > fromIntegral watchdogTimeoutNS) $ do
        putStrLn "!!! WATCHDOG TRIP: SYSTEM FROZEN !!!"
        putStrLn "!!! FORCING BEAM OFF !!!"
        -- In real HW, this would toggle a GPIO pin immediately
        exitFailure

    threadDelay 10000 -- Check every 10ms
