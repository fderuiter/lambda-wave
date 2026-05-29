module Safety.Watchdog (watchdogLoop, runSafetyDaemon) where

import Data.Types
import Control.Concurrent.STM
import Control.Concurrent (threadDelay)
import System.IO (hFlush, stdout)
import Data.Word (Word64)
import Data.Time.HighRes (getMonotonicTimeNS)
import Control.Monad (forever, when, unless, forM_)
import System.Exit (ExitCode(..))
import System.Posix.Process (exitImmediately)
import System.Posix.Types (ProcessID)
import System.Posix.Signals (signalProcess, sigKILL)
import qualified Data.Map.Strict as Map
import Network.Socket
import Network.Socket.ByteString (sendTo, recvFrom)
import qualified Data.ByteString.Char8 as BC
import Control.Exception (try, IOException)
import System.Timeout (timeout)
import System.Posix.Files (removeLink)

import Hardware.Control (setBeamChannel, GpioChannel(..))
import Numeric.Kinematics

udsPath :: String
udsPath = "/tmp/sgrt_heartbeat.sock"

-- | The Watchdog Heartbeat Sender Loop (Runs in Main Process)
-- Evaluates thread heartbeats. If everything is fine, it sends a heartbeat to the Daemon.
-- If any thread is frozen, it stops sending heartbeats, causing the Daemon to trip.
watchdogLoop :: TVar SystemState -> IO ()
watchdogLoop stateVar = do
    sock <- socket AF_UNIX Datagram 0
    let addr = SockAddrUnix udsPath
    let Time timeoutSec = watchdogTimeoutTime (Proxy :: Proxy WatchdogTimeoutMs)
        timeoutNS = round (timeoutSec * 1_000_000_000) :: Word64
    forever $ do
        now <- getMonotonicTimeNS
        state <- readTVarIO stateVar
        let heartbeats = threadHeartbeats state

        -- Check all monitored threads
        let isHealthy = all (\(_, lastTime) -> (now - lastTime) <= timeoutNS) (Map.toList heartbeats)

        if isHealthy && not (Map.null heartbeats)
            then do
                -- Send heartbeat
                _ <- try (sendTo sock (BC.pack "HB") addr) :: IO (Either IOException Int)
                return ()
            else do
                -- Find frozen threads for logging
                forM_ (Map.toList heartbeats) $ \(threadName, lastTime) -> do
                    let diff = now - lastTime
                    when (diff > timeoutNS) $ do
                        let msg = "Thread '" ++ threadName ++ "' FROZEN (Age: " ++ show diff ++ "ns)."
                        atomically $ do
                            let q = auditQueue state
                            full <- isFullTBQueue q
                            unless full $
                                writeTBQueue q (AuditEvent now Critical "Watchdog" msg)
                        putStrLn $ "!!! MAIN WATCHDOG: " ++ msg
                        hFlush stdout
                
        threadDelay 10000 -- Check every 10ms

-- | Runs the independent Safety Daemon process
runSafetyDaemon :: ProcessID -> IO ()
runSafetyDaemon parentPid = do
    putStrLn "[Safety Daemon] Started and monitoring parent process."
    
    -- Ensure clean socket
    _ <- try (removeLink udsPath) :: IO (Either IOException ())
    
    sock <- socket AF_UNIX Datagram 0
    bind sock (SockAddrUnix udsPath)
    
    -- Ensure Beam is ON for Watchdog Channel initially
    setBeamChannel WatchdogChannel True

    -- Loop waiting for heartbeats
    let loop = do
            -- Receive with timeout
            -- timeout takes microseconds.
            let Time timeoutSec = watchdogTimeoutTime (Proxy :: Proxy WatchdogTimeoutMs)
                timeoutUS = round (timeoutSec * 1_000_000) :: Int
            res <- try (timeout timeoutUS $ recvFrom sock 16) :: IO (Either IOException (Maybe (BC.ByteString, SockAddr)))
            
            case res of
                Right (Just (msg, _)) | msg == BC.pack "HB" -> do
                    loop
                _ -> do
                    -- Timeout, IO error, or invalid message -> TRIP!
                    tripDaemon parentPid
    
    loop

tripDaemon :: ProcessID -> IO ()
tripDaemon parentPid = do
    let msg = "!!! SAFETY DAEMON TRIP: Lost Heartbeat. FORCING BEAM OFF."
    putStrLn msg
    -- Dual-Channel Safety: Force Watchdog channel off
    setBeamChannel WatchdogChannel False
    
    -- Independent Audit Log recording
    now <- getMonotonicTimeNS
    let auditMsg = show now ++ " [CRITICAL] [SafetyDaemon] " ++ msg ++ "\n"
    _ <- try (appendFile "session.log" auditMsg) :: IO (Either IOException ())
    
    -- Terminate main application process
    putStrLn $ "!!! SAFETY DAEMON: Terminating Parent PID " ++ show parentPid
    _ <- try (signalProcess sigKILL parentPid) :: IO (Either IOException ())
    
    hFlush stdout
    exitImmediately (ExitFailure 1)

