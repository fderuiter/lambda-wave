{-# LANGUAGE OverloadedStrings #-}
module Hardware.Integrity (
    startupIntegrityTest,
    integrityMonitorLoop
) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.STM
import qualified Data.Map.Strict as Map
import Data.Time.HighRes (getMonotonicTimeNS)
import Control.Monad (forever)

import Hardware.Control (setBeamChannel, readBeamChannel, GpioChannel(..))
import Data.Types (SystemState(..), BeamState(..), AuditEvent(..), Severity(..))

-- | Run the startup hardware integrity test.
-- Toggles LogicChannel and verifies readback.
-- Returns True if successful, False if a short-circuit/failure is detected.
startupIntegrityTest :: IO Bool
startupIntegrityTest = do
    -- Ensure initial state is OFF
    setBeamChannel LogicChannel False
    threadDelay 2000 -- 2ms to settle
    state0 <- readBeamChannel LogicChannel
    
    -- Turn ON
    setBeamChannel LogicChannel True
    threadDelay 2000 -- 2ms to settle
    state1 <- readBeamChannel LogicChannel
    
    -- Turn OFF
    setBeamChannel LogicChannel False
    threadDelay 2000 -- 2ms to settle
    state2 <- readBeamChannel LogicChannel
    
    return (not state0 && state1 && not state2)

-- | Real-time integrity monitor loop.
-- Runs independently to ensure hardware matches intended software state.
integrityMonitorLoop :: TVar SystemState -> IO ()
integrityMonitorLoop stateVar = do
    -- Loop checks Pin 17 against beamState with 2ms debounce.
    let loop mismatchStart = do
            -- Sleep for 1ms
            threadDelay 1000
            
            systemState <- readTVarIO stateVar
            let intendedOn = beamState systemState == BeamOn
            
            -- Read actual physical echo (Pin 17)
            actualOn <- readBeamChannel LogicChannel
            
            now <- getMonotonicTimeNS
            
            if intendedOn == actualOn
                then do
                    -- Update heartbeat
                    atomically $ modifyTVar' stateVar $ \s -> 
                        s { threadHeartbeats = Map.insert "HardwareIntegrity" now (threadHeartbeats s) }
                    loop Nothing
                else do
                    case mismatchStart of
                        Nothing -> loop (Just now)
                        Just startT -> do
                            -- 2ms debounce (2,000,000 ns)
                            let elapsedNs = now - startT
                            if elapsedNs >= 2000000
                                then do
                                    -- Trip interlock (Pin 27 OFF)
                                    setBeamChannel WatchdogChannel False
                                    
                                    let evt = AuditEvent now Critical "HardwareIntegrity" "Interlock tripped: Signal mismatch detected"
                                    atomically $ do
                                        writeTBQueue (auditQueue systemState) evt
                                        modifyTVar' stateVar $ \s -> s { beamState = BeamOff }
                                    
                                    -- Keep reporting heartbeat but stay in tripped state
                                    tripLoop stateVar
                                else do
                                    -- Wait more
                                    loop mismatchStart
    
    loop Nothing

tripLoop :: TVar SystemState -> IO ()
tripLoop stateVar = forever $ do
    threadDelay 10000 -- 10ms
    now <- getMonotonicTimeNS
    atomically $ modifyTVar' stateVar $ \s -> 
        s { threadHeartbeats = Map.insert "HardwareIntegrity" now (threadHeartbeats s) }
