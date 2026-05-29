module Safety.Watchdog (watchdogLoop, runSafetyDaemon) where

import Data.Types
import Data.Config (watchdogTimeoutNS)
import Control.Concurrent.STM
import Control.Concurrent (threadDelay, forkIO)
import System.IO (hFlush, stdout)
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
import Control.Exception (try, IOException, catch)
import System.Timeout (timeout)
import System.Posix.Files (removeLink)

import Hardware.Control (setBeamChannel, GpioChannel(..))

udsPath :: String
udsPath = "/tmp/sgrt_heartbeat.sock"

-- | The Watchdog Heartbeat Sender Loop (Runs in Main Process)
watchdogLoop :: TVar SystemState -> IO ()
watchdogLoop stateVar = do
    sock <- socket AF_UNIX Datagram 0
    let addr = SockAddrUnix udsPath
    forever $ do
        now <- getMonotonicTimeNS
        state <- readTVarIO stateVar
        let heartbeats = threadHeartbeats state

        let isHealthy = all (\(_, lastTime) -> (now - lastTime) <= fromIntegral watchdogTimeoutNS) (Map.toList heartbeats)

        if isHealthy && not (Map.null heartbeats)
            then do
                _ <- try (sendTo sock (BC.pack "HB") addr) :: IO (Either IOException Int)
                return ()
            else do
                forM_ (Map.toList heartbeats) $ \(threadName, lastTime) -> do
                    let diff = now - lastTime
                    when (diff > fromIntegral watchdogTimeoutNS) $ do
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
    putStrLn "[Safety Daemon] Started and monitoring parent process and UI."
    
    _ <- try (removeLink udsPath) :: IO (Either IOException ())
    
    sock <- socket AF_UNIX Datagram 0
    bind sock (SockAddrUnix udsPath)
    
    setBeamChannel WatchdogChannel True

    now <- getMonotonicTimeNS
    coreHBRef <- newTVarIO now
    uiHBRef <- newTVarIO now
    uiLoggedDeadRef <- newTVarIO False

    -- Receiver thread
    _ <- forkIO $ forever $ do
        res <- try (recvFrom sock 16) :: IO (Either IOException (BC.ByteString, SockAddr))
        case res of
            Right (msg, _) -> do
                t <- getMonotonicTimeNS
                if msg == BC.pack "HB" then
                    atomically $ writeTVar coreHBRef t
                else if msg == BC.pack "UI_HB" then
                    atomically $ do
                        writeTVar uiHBRef t
                        writeTVar uiLoggedDeadRef False
                else return ()
            Left _ -> return ()

    -- Monitor loop
    let loop = do
            threadDelay 10000 -- 10ms
            t <- getMonotonicTimeNS
            (lastCore, lastUI, uiLoggedDead) <- atomically $ do
                c <- readTVar coreHBRef
                u <- readTVar uiHBRef
                l <- readTVar uiLoggedDeadRef
                return (c, u, l)
            
            let coreDiff = t - lastCore
            let uiDiff = t - lastUI
            
            -- UI tracking (Status Monitor) - does NOT trip Safety Core
            when (uiDiff > fromIntegral watchdogTimeoutNS && not uiLoggedDead) $ do
                -- UI is dead/frozen
                putStrLn $ "[STATUS MONITOR] UI Process is unresponsive! (Age: " ++ show (uiDiff `div` 1000000) ++ "ms)"
                hFlush stdout
                atomically $ writeTVar uiLoggedDeadRef True

            -- Safety Core tracking
            if coreDiff > fromIntegral watchdogTimeoutNS
                then tripDaemon parentPid
                else loop
    
    loop

tripDaemon :: ProcessID -> IO ()
tripDaemon parentPid = do
    let msg = "!!! SAFETY DAEMON TRIP: Lost Heartbeat. FORCING BEAM OFF."
    putStrLn msg
    setBeamChannel WatchdogChannel False
    
    now <- getMonotonicTimeNS
    let auditMsg = show now ++ " [CRITICAL] [SafetyDaemon] " ++ msg ++ "\n"
    _ <- try (appendFile "session.log" auditMsg) :: IO (Either IOException ())
    
    putStrLn $ "!!! SAFETY DAEMON: Terminating Parent PID " ++ show parentPid
    _ <- try (signalProcess sigKILL parentPid) :: IO (Either IOException ())
    
    hFlush stdout
    exitImmediately (ExitFailure 1)

