{-# LANGUAGE ScopedTypeVariables #-}
module Safety.Watchdog (watchdogLoop, runSafetyDaemon) where

import Data.Types
import Control.Concurrent.STM
import Control.Concurrent (threadDelay)
import System.IO (hFlush, stdout)
import Data.Word (Word64)
import Data.Time.HighRes (getMonotonicTimeNS)
import Control.Monad (forever, when, unless, forM_)
import System.Exit (ExitCode(..), exitWith)
import System.Posix.Process (getProcessID)
import System.Posix.Types (ProcessID)
import System.Posix.Signals (signalProcess, sigKILL)
import qualified Data.Map.Strict as Map
import Network.Socket
import Network.Socket.ByteString (sendTo, recv)
import qualified Data.ByteString.Char8 as BC
import Control.Exception (try, IOException, catch, SomeException)
import System.Timeout (timeout)
import System.Posix.Files (removeLink)
import qualified Data.ByteString as B
import Safety.Crypto (encryptLog)
import Safety.Result (SafetyResult(..))

import Hardware.Control (setBeamChannelDaemon, GpioChannel(..))
import Hardware.FFI.Bridge (handleHardwareResponse)
import Hardware.FFI.Common (HardwareResult)
import Numeric.Kinematics

udsPath :: ProcessID -> String
udsPath pid = "/tmp/sgrt_heartbeat_" ++ show pid ++ ".sock"

daemonAudit :: HardwareResult -> IO ()
daemonAudit res = do
    now <- getMonotonicTimeNS
    let auditMsg = show now ++ " [INFO] [SafetyDaemon] Hardware Bridge: " ++ show res ++ "\n"
    encRes <- encryptLog auditMsg
    case encRes of
        Safe encAudit -> do
            _ <- try (B.appendFile "session.log" encAudit) :: IO (Either IOException ())
            return ()
        Fault e -> do
            putStrLn $ "!!! SAFETY DAEMON: Crypto Fault in audit: " ++ e
            return ()

watchdogLoop :: TVar SystemState -> IO ()
watchdogLoop stateVar = (`catch` \e -> do putStrLn $ "WATCHDOG CRASHED: " ++ show (e :: SomeException); hFlush stdout) $ do
    putStrLn "WATCHDOG LOOP START"
    hFlush stdout
    myPid <- getProcessID
    sock <- socket AF_UNIX Datagram 0
    let addr = SockAddrUnix (udsPath myPid)

    let Time timeoutSec = watchdogTimeoutTime (Proxy :: Proxy WatchdogTimeoutMs)
        timeoutNS = round (timeoutSec * 1_000_000_000) :: Word64
    forever $ do
        now <- getMonotonicTimeNS
        state <- readTVarIO stateVar
        let heartbeats = threadHeartbeats state

        let isHealthy = all (\(_, lastTime) -> (now - lastTime) <= timeoutNS) (Map.toList heartbeats)

        if isHealthy && not (Map.null heartbeats)
            then do
                -- putStrLn $ "DEBUG WATCHDOG HEALTHY: now=" ++ show now
                -- hFlush stdout
                _ <- try (sendTo sock (BC.pack "HB") addr) :: IO (Either IOException Int)
                return ()
            else do
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
                        threadDelay 1000

                _ <- try (sendTo sock (BC.pack "TRIP") addr) :: IO (Either IOException Int)
                return ()
                
        threadDelay 2000

runSafetyDaemon :: ProcessID -> IO ()
runSafetyDaemon parentPid = do
    putStrLn "[Safety Daemon] Started and monitoring parent process."
    
    let path = udsPath parentPid
    _ <- try (removeLink path) :: IO (Either IOException ())
    
    sock <- socket AF_UNIX Datagram 0
    bind sock (SockAddrUnix path)
    
    res <- setBeamChannelDaemon daemonAudit WatchdogChannel True
    handleHardwareResponse 
        (\err -> do
            putStrLn $ "[Safety Daemon] Failed to initialize Watchdog hardware channel: " ++ show err
            exitWith (ExitFailure 1)
        )
        (\() -> return ())
        res

    let loop = do
            let Time timeoutSec = watchdogTimeoutTime (Proxy :: Proxy WatchdogTimeoutMs)
                timeoutUS = round (timeoutSec * 1_000_000) + 5000 :: Int
            resSock <- try (timeout timeoutUS $ recv sock 16) :: IO (Either IOException (Maybe BC.ByteString))
            
            case resSock of
                Right (Just msg) -> do
                    if msg == BC.pack "HB"
                        then loop
                        else do
                            putStrLn $ "[Safety Daemon] Invalid message received: " ++ show msg
                            tripDaemon parentPid
                Right Nothing -> do
                    putStrLn "[Safety Daemon] Timeout reached!"
                    tripDaemon parentPid
                Left err -> do
                    putStrLn $ "[Safety Daemon] IO Error: " ++ show err
                    tripDaemon parentPid
    
    loop

tripDaemon :: ProcessID -> IO ()
tripDaemon parentPid = do
    let msg = "!!! SAFETY DAEMON TRIP: Lost Heartbeat. FORCING BEAM OFF."
    putStrLn msg
    res <- setBeamChannelDaemon daemonAudit WatchdogChannel False
    handleHardwareResponse 
        (\err -> putStrLn $ "!!! SAFETY DAEMON: Hardware Actuation Error during TRIP: " ++ show err)
        (\() -> return ())
        res
    
    now <- getMonotonicTimeNS
    let auditMsg = show now ++ " [CRITICAL] [SafetyDaemon] " ++ msg ++ "\n"
    encRes <- encryptLog auditMsg
    
    case encRes of
        Safe encAudit -> do
            resFile <- try (B.appendFile "session.log" encAudit) :: IO (Either IOException ())
            case resFile of
                Left err -> do
                    putStrLn $ "!!! SAFETY DAEMON IO ERROR writing session.log: " ++ show err
                    hFlush stdout
                    _ <- try (B.appendFile "fallback_audit.log" encAudit) :: IO (Either IOException ())
                    return ()
                Right () -> return ()
        Fault e -> putStrLn $ "!!! SAFETY DAEMON: Crypto Fault writing audit: " ++ e

    putStrLn $ "!!! SAFETY DAEMON: Terminating Parent PID " ++ show parentPid
    hFlush stdout
    
    _ <- try (signalProcess sigKILL parentPid) :: IO (Either IOException ())
    
    exitWith (ExitFailure 1)

