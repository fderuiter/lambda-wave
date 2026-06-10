module Safety.Watchdog (checkWatchdog, checkWatchdogInit) where

import Data.Types
import Control.Concurrent.STM
import System.IO (hFlush, stdout)
import Data.Word (Word64)
import Data.Time.HighRes (getMonotonicTimeNS)
import Control.Monad (when, unless, forM_)
import System.Exit (ExitCode(..))
import System.Posix.Process (exitImmediately)
import qualified Data.Map.Strict as Map
import Control.Exception (try, IOException)

import Hardware.Control (setBeamChannel, GpioChannel(..))
import Numeric.Kinematics

checkWatchdogInit :: IO ()
checkWatchdogInit = do
    setBeamChannel WatchdogChannel True

-- | Non-blocking heartbeat check to be integrated directly into the core execution loop
checkWatchdog :: TVar SystemState -> IO ()
checkWatchdog stateVar = do
    now <- getMonotonicTimeNS
    state <- readTVarIO stateVar
    let heartbeats = threadHeartbeats state
    
    let Time timeoutSec = watchdogTimeoutTime (Proxy :: Proxy WatchdogTimeoutMs)
        timeoutNS = round (timeoutSec * 1_000_000_000) :: Word64

    let isHealthy = all (\(_, lastTime) -> (now - lastTime) <= timeoutNS) (Map.toList heartbeats)

    unless (isHealthy || Map.null heartbeats) $ do
        let msg = "!!! SAFETY TRIP: Lost Heartbeat. FORCING BEAM OFF."
        putStrLn msg
        setBeamChannel WatchdogChannel False
        
        let auditMsg = show now ++ " [CRITICAL] [Watchdog] " ++ msg ++ "\n"
        _ <- try (appendFile "session.log" auditMsg) :: IO (Either IOException ())
        
        forM_ (Map.toList heartbeats) $ \(threadName, lastTime) -> do
            let diff = now - lastTime
            when (diff > timeoutNS) $ do
                let msgFrozen = "Component '" ++ threadName ++ "' FROZEN (Age: " ++ show diff ++ "ns)."
                atomically $ do
                    let q = auditQueue state
                    full <- isFullTBQueue q
                    unless full $
                        writeTBQueue q (AuditEvent now Critical "Watchdog" msgFrozen)
                putStrLn $ "!!! WATCHDOG: " ++ msgFrozen
        
        hFlush stdout
        exitImmediately (ExitFailure 1)


-- Hazard H-SYS-001: Beam ON during motion
-- Hazard H-SYS-003: Latency spike
-- Hazard H-SOUP-004: Deadlocks
