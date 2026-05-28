{-# LANGUAGE ForeignFunctionInterface #-}
{-|
Module      : Safety.Watchdog
Description : The "Dead Man's Switch" for Beam Safety
Copyright   : (c) 2024
License     : AGPL-3.0-only
Maintainer  : atlas@code-cartographer.com
-}
module Safety.Watchdog (watchdogLoop) where

import Data.Types
import Data.Config (watchdogTimeoutNS)
import Control.Concurrent.STM
import Control.Concurrent (threadDelay)
import System.IO (hFlush, stdout)
import Data.Time.HighRes (getMonotonicTimeNS)
import Control.Monad (forever, when, unless, forM_)
import Foreign.C.Types (CInt(..))
import Data.Word (Word64)
import qualified Data.Map.Strict as Map

foreign import ccall safe "start_safety_sidecar"
    c_start_safety_sidecar :: IO CInt

foreign import ccall safe "update_heartbeat"
    c_update_heartbeat :: Word64 -> IO ()

-- | The Watchdog Loop
watchdogLoop :: TVar SystemState -> IO ()
watchdogLoop stateVar = do
    res <- c_start_safety_sidecar
    when (res /= 0) $ do
        putStrLn "Failed to start C++ safety sidecar. Defaulting to Beam Off."
        -- In real HW, this would toggle GPIO. Handled by C++ now.

    forever $ do
        now <- getMonotonicTimeNS
        state <- readTVarIO stateVar
        let heartbeats = threadHeartbeats state

        -- Check if all threads are healthy in Haskell
        let allHealthy = all (\(_, lastTime) -> (now - lastTime) <= fromIntegral watchdogTimeoutNS) (Map.toList heartbeats)
        
        -- And check if there is at least one heartbeat
        if allHealthy && not (Map.null heartbeats) then do
            -- Update C++ sidecar heartbeat
            c_update_heartbeat now
        else do
            -- If not healthy, we do NOT update the sidecar.
            -- The sidecar will timeout independently and kill the process!
            
            -- We can optionally try to log the event here if the Haskell thread is still running
            -- and it's a specific thread that froze, rather than a global GC pause.
            forM_ (Map.toList heartbeats) $ \(threadName, lastTime) -> do
                let diff = now - lastTime
                when (diff > fromIntegral watchdogTimeoutNS) $ do
                    let msg = "Thread '" ++ threadName ++ "' FROZEN (Age: " ++ show diff ++ "ns). FORCING BEAM OFF."
                    atomically $ do
                        let q = auditQueue state
                        full <- isFullTBQueue q
                        unless full $
                            writeTBQueue q (AuditEvent now Critical "Watchdog" msg)
                    putStrLn $ "!!! WATCHDOG LOG: " ++ msg
                    hFlush stdout

        -- Check every 10ms
        threadDelay 10000 
