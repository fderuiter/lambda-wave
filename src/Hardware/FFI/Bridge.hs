{-# LANGUAGE TupleSections #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- |
-- = Failure Mode
-- Unhandled hardware FFI exceptions can corrupt system state or drop critical sensor readings.
--
-- = Mitigation
-- The MustHandle type forces explicit error handling mapping for all FFI bridge calls.
-- Mitigates Hazard H-SYS-008
--
-- = Audit Events
-- All FFI returns trigger specific audit events defined in ffi_master_spec.md.
module Hardware.FFI.Bridge (
    MustHandle,
    executeBridgeCall,
    executeBridgeCallWith,
    auditHardwareEvent,
    bridgeHardwareCall,
    bridgeHardwareCallCustom,
    bridgeRingBufferCall,
    bridgeHardwareQuery,
    handleHardwareResponse
) where

import qualified Hardware.FFI.Common as Common
import Control.Monad (unless)
import Hardware.FFI.Common (HardwareResult, toHardwareResult, toRingBufferResult)
import Hardware.Types
import Data.Types (SystemState(..), AuditEvent(..), Severity(..))
import Control.Concurrent.STM
import Control.Concurrent (threadDelay)
import Foreign.C.Error (getErrno, Errno(..))
import Data.Time.HighRes (getMonotonicTimeNS)
import Foreign.C.Types (CInt)
import System.Posix.Types (CSsize)
import Safety.Audit (tryWriteAuditSTM, triggerShutdown)

-- | Opaque type to ensure the caller explicitly handles the result.
-- No Functor/Monad instances are provided to prevent `_ <-` ignoring.
newtype MustHandle a = MustHandle (Either HardwareError a)

-- | Explicit elimination function for MustHandle.
handleHardwareResponse :: (HardwareError -> IO b) -> (a -> IO b) -> MustHandle a -> IO b
handleHardwareResponse onErr _ (MustHandle (Left err)) = onErr err
handleHardwareResponse _ onSuccess (MustHandle (Right val)) = onSuccess val

-- | Pipes outcomes to the audit system. Shuts down if audit system is unreachable.
auditHardwareEvent :: TVar SystemState -> String -> HardwareResult -> IO ()
auditHardwareEvent stateVar comp res = do
    now <- getMonotonicTimeNS
    let (sev, msg) = case res of
            Common.Success -> (Info, "Hardware call succeeded")
            Common.SimulationMode -> (Warning, "Hardware call succeeded (Simulation Mode)")
            Common.PartialData n -> (Info, "Hardware call returned partial data: " ++ show n)
            Common.Busy -> (Warning, "Hardware resource busy")
            Common.EOF -> (Info, "Hardware EOF")
            Common.Failure s -> (Critical, "Hardware failure: " ++ s)
            Common.SystemError e -> (Critical, "Hardware POSIX System Error (errno): " ++ show e)
            Common.DriverError s -> (Critical, "Hardware driver error: " ++ s)
            Common.TransientError s -> (Warning, "Hardware transient error: " ++ s)
            Common.InvalidConfiguration -> (Critical, "Hardware invalid configuration")
    
    let evt = AuditEvent now sev comp msg
    
    -- "Failure to log an event to the audit system results in a controlled system shutdown."
    -- We can try to write to the TBQueue. If it's full, or we hit an exception, we shutdown.
    writeSuccess <- atomically $ do
        s <- readTVar stateVar
        tryWriteAuditSTM (auditQueue s) evt

    unless writeSuccess $ triggerShutdown stateVar "Audit logging failed"

-- | Automated retry logic for recoverable errors like transient serial port disconnects.
executeBridgeCall :: (HardwareResult -> IO ()) -> IO HardwareResult -> IO (MustHandle ())
executeBridgeCall auditFn action = executeBridgeCallWith auditFn (fmap (, ()) action)

-- | Automated retry logic that returns a value on success.
executeBridgeCallWith :: (HardwareResult -> IO ()) -> IO (HardwareResult, a) -> IO (MustHandle a)
executeBridgeCallWith auditFn action = go (3 :: Int)
  where
    go (0 :: Int) = do
        auditFn (Common.Failure "Max retries exceeded")
        return $ MustHandle (Left Timeout)
    go retries = do
        (res, val) <- action
        auditFn res
        case res of
            Common.Success -> return $ MustHandle (Right val)
            Common.SimulationMode -> return $ MustHandle (Left SimulationModeActive)
            Common.TransientError _ -> do
                threadDelay 10000 -- 10ms wait
                go (retries - 1)
            Common.SystemError err -> return $ MustHandle (Left (SystemError err))
            Common.DriverError err -> return $ MustHandle (Left (DriverError err))
            Common.Failure err -> return $ MustHandle (Left (UnknownError err))
            Common.EOF -> return $ MustHandle (Left ConnectionLost)
            _ -> return $ MustHandle (Left (UnknownError "Unexpected result"))

-- | Helper to call C functions that return CInt
bridgeHardwareCall :: TVar SystemState -> String -> IO CInt -> IO (MustHandle ())
bridgeHardwareCall stateVar comp c_call = executeBridgeCall (auditHardwareEvent stateVar comp) $ do
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
bridgeRingBufferCall stateVar comp c_call = do
    ret <- c_call
    (Errno err) <- getErrno
    let res = toRingBufferResult (fromIntegral err) ret
    auditHardwareEvent stateVar comp res
    return res

-- | High-speed fast path for status queries. No retry delay. Returns typed data.
bridgeHardwareQuery :: TVar SystemState -> String -> IO CInt -> (CInt -> (HardwareResult, Either HardwareError a)) -> IO (MustHandle a)
bridgeHardwareQuery stateVar comp c_call parser = do
    ret <- c_call
    let (res, parsed) = parser ret
    auditHardwareEvent stateVar comp res
    return $ MustHandle parsed
