{-# LANGUAGE GADTs #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Hardware.FFI.Bridge (
    MustHandle,
    executeBridgeCall,
    bridgeHardwareCall,
    bridgeHardwareCallCustom,
    bridgeRingBufferCall,
    handleHardwareResponse,
    triggerShutdown
) where

import Hardware.FFI.Common
import Hardware.Types
import Data.Types (SystemState(..), AuditEvent(..), Severity(..), BeamState(..))
import Control.Concurrent.STM
import Control.Concurrent (threadDelay)
import Foreign.C.Error (getErrno, Errno(..))
import Data.Time.HighRes (getMonotonicTimeNS)
import Foreign.C.Types (CInt, CSsize)
import Control.Exception (try, SomeException)

-- | Opaque type to ensure the caller explicitly handles the result.
-- No Functor/Monad instances are provided to prevent `_ <-` ignoring.
newtype MustHandle a = MustHandle (Either HardwareError a)

-- | Explicit elimination function for MustHandle.
handleHardwareResponse :: (HardwareError -> IO b) -> (a -> IO b) -> MustHandle a -> IO b
handleHardwareResponse onErr onSuccess (MustHandle (Left err)) = onErr err
handleHardwareResponse onErr onSuccess (MustHandle (Right val)) = onSuccess val

-- | Centralized function to trigger a controlled system shutdown on failure
triggerShutdown :: TVar SystemState -> String -> IO ()
triggerShutdown stateVar reason = do
    now <- getMonotonicTimeNS
    atomically $ do
        s <- readTVar stateVar
        writeTVar stateVar s { beamState = BeamOff }
        -- We try to write one last critical event but this shouldn't block shutdown
        let evt = AuditEvent now Critical "Bridge" ("SYSTEM SHUTDOWN TRIGGERED: " ++ reason)
        full <- isFullTBQueue (auditQueue s)
        if not full
            then writeTBQueue (auditQueue s) evt
            else return ()

-- | Pipes outcomes to the audit system. Shuts down if audit system is unreachable.
auditHardwareEvent :: TVar SystemState -> String -> HardwareResult -> IO ()
auditHardwareEvent stateVar component res = do
    now <- getMonotonicTimeNS
    let (sev, msg) = case res of
            Success -> (Info, "Hardware call succeeded")
            PartialData n -> (Info, "Hardware call returned partial data: " ++ show n)
            Busy -> (Warning, "Hardware resource busy")
            EOF -> (Info, "Hardware EOF")
            Failure s -> (Critical, "Hardware failure: " ++ s)
            SystemError e -> (Critical, "Hardware POSIX System Error (errno): " ++ show e)
            DriverError s -> (Critical, "Hardware driver error: " ++ s)
            TransientError s -> (Warning, "Hardware transient error: " ++ s)
            InvalidConfiguration -> (Critical, "Hardware invalid configuration")
    
    let evt = AuditEvent now sev component msg
    
    -- "Failure to log an event to the audit system results in a controlled system shutdown."
    -- We can try to write to the TBQueue. If it's full, or we hit an exception, we shutdown.
    writeRes <- try $ atomically $ do
        s <- readTVar stateVar
        let q = auditQueue s
        full <- isFullTBQueue q
        if full 
            then error "Audit queue full" 
            else writeTBQueue q evt

    case writeRes of
        Left (_ :: SomeException) -> triggerShutdown stateVar "Audit logging failed"
        Right _ -> return ()

-- | Automated retry logic for recoverable errors like transient serial port disconnects.
executeBridgeCall :: (HardwareResult -> IO ()) -> IO HardwareResult -> IO (MustHandle ())
executeBridgeCall auditFn action = go 3
  where
    go :: Int -> IO (MustHandle ())
    go 0 = do
        auditFn (Failure "Max retries exceeded")
        return $ MustHandle (Left Timeout)
    go retries = do
        res <- action
        auditFn res
        case res of
            Success -> return $ MustHandle (Right ())
            TransientError _ -> do
                threadDelay 10000 -- 10ms wait
                go (retries - 1)
            SystemError err -> return $ MustHandle (Left (SystemError err))
            DriverError err -> return $ MustHandle (Left (DriverError err))
            Failure err -> return $ MustHandle (Left (UnknownError err))
            EOF -> return $ MustHandle (Left ConnectionLost)
            _ -> return $ MustHandle (Left (UnknownError "Unexpected result"))

-- | Helper to call C functions that return CInt
bridgeHardwareCall :: TVar SystemState -> String -> IO CInt -> IO (MustHandle ())
bridgeHardwareCall stateVar component c_call = executeBridgeCall (auditHardwareEvent stateVar component) $ do
    ret <- c_call
    (Errno err) <- getErrno
    return $ toHardwareResult (fromIntegral err) ret

-- | Helper to call C functions with a custom logger (for the isolated daemon)
bridgeHardwareCallCustom :: (HardwareResult -> IO ()) -> IO CInt -> IO (MustHandle ())
bridgeHardwareCallCustom auditFn c_call = executeBridgeCall auditFn $ do
    ret <- c_call
    (Errno err) <- getErrno
    return $ toHardwareResult (fromIntegral err) ret

-- | Helper to call C functions that return CSsize
bridgeRingBufferCall :: TVar SystemState -> String -> IO CSsize -> IO HardwareResult
bridgeRingBufferCall stateVar component c_call = do
    ret <- c_call
    (Errno err) <- getErrno
    let res = toRingBufferResult (fromIntegral err) ret
    auditHardwareEvent stateVar component res
    return res
