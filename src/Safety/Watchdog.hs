{-# LANGUAGE CPP #-}
module Safety.Watchdog (watchdogLoop, runSafetyDaemon) where

import Data.Types
import Control.Concurrent.STM
import Control.Concurrent (threadDelay)
import System.IO (hFlush, stdout)
import Data.Word (Word64)
import Data.Time.HighRes (getMonotonicTimeNS)
import Control.Monad (forever, when, unless, forM_)
import System.Exit (ExitCode(..))
import System.Posix.Process (exitImmediately, getProcessID)
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

#ifdef SANDBOX
isDebuggerAttached :: ProcessID -> IO Bool
isDebuggerAttached pid = do
    let path = "/proc/" ++ show pid ++ "/status"
    res <- try (BC.readFile path) :: IO (Either IOException BC.ByteString)
    case res of
        Left _ -> return False
        Right content -> do
            let ls = BC.lines content
            let tracerLines = filter (BC.isPrefixOf (BC.pack "TracerPid:")) ls
            case tracerLines of
                (l:_) -> do
                    let valStr = BC.dropWhile (== ' ') $ BC.dropWhile (== '\t') $ BC.drop 10 l
                    case BC.readInt valStr of
                        Just (tracerPid, _) -> return (tracerPid > 0)
                        _ -> return False
                _ -> return False
#else
isDebuggerAttached :: ProcessID -> IO Bool
isDebuggerAttached _ = return False
#endif

-- | The Watchdog Heartbeat Sender Loop (Runs in Main Process)
watchdogLoop :: TVar SystemState -> Word64 -> IO ()
watchdogLoop stateVar timeoutNS = do
    sock <- socket AF_UNIX Datagram 0
    let addr = SockAddrUnix udsPath
    myPid <- getProcessID
    forever $ do
        now <- getMonotonicTimeNS
        state <- readTVarIO stateVar
        let heartbeats = threadHeartbeats state

        -- Check all monitored threads
        let isHealthy = all (\(_, lastTime) -> (now - lastTime) <= timeoutNS) (Map.toList heartbeats)

        isDbg <- isDebuggerAttached myPid

        if isHealthy && not (Map.null heartbeats)
            then do
                -- Send heartbeat
                let msg = if isDbg then "DEBUG" else "HB"
                _ <- try (sendTo sock (BC.pack msg) addr) :: IO (Either IOException Int)
                return ()
            else do
                if isDbg
                    then do
                        _ <- try (sendTo sock (BC.pack "DEBUG") addr) :: IO (Either IOException Int)
                        return ()
                    else do
                        -- Explicitly send TRIP message to daemon to guarantee <110ms response time
                        _ <- try (sendTo sock (BC.pack "TRIP") addr) :: IO (Either IOException Int)
                        
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
runSafetyDaemon :: ProcessID -> Word64 -> IO ()
runSafetyDaemon parentPid timeoutNS = do
    putStrLn "[Safety Daemon] Started and monitoring parent process."
    
    -- Ensure clean socket
    _ <- try (removeLink udsPath) :: IO (Either IOException ())
    
    sock <- socket AF_UNIX Datagram 0
    bind sock (SockAddrUnix udsPath)
    
    -- Ensure Beam is ON for Watchdog Channel initially
    setBeamChannel WatchdogChannel True

    let timeoutUS = fromIntegral (timeoutNS `div` 1000) :: Int

    let suspendedLoop = do
            res <- try (recvFrom sock 16) :: IO (Either IOException (BC.ByteString, SockAddr))
            case res of
                Right (msg, _) | msg == BC.pack "HB" -> do
                    putStrLn "[Safety Daemon] Resuming from suspension (Grace Period active)."
                    -- Re-arm the watchdog. 
                    loop
                Right (msg, _) | msg == BC.pack "TRIP" -> do
                    tripDaemon parentPid
                _ -> suspendedLoop

        loop = do
            -- Receive with timeout
            res <- try (timeout timeoutUS $ recvFrom sock 16) :: IO (Either IOException (Maybe (BC.ByteString, SockAddr)))
            
            case res of
                Right (Just (msg, _)) | msg == BC.pack "HB" -> do
                    loop
                Right (Just (msg, _)) | msg == BC.pack "DEBUG" -> do
                    -- Main process requested suspension
                    suspendedLoop
                _ -> do
                    -- Timeout, IO error, or invalid message -> TRIP!
                    isDbg <- isDebuggerAttached parentPid
                    if isDbg
                        then suspendedLoop
                        else tripDaemon parentPid
    
    loop

-- Requirement SR-WD-002
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
