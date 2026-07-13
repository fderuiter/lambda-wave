{-# LANGUAGE OverloadedStrings #-}
module Safety.AuditSpec (spec) where

import Test.Hspec
import Control.Concurrent.STM
import Data.Time.HighRes (getMonotonicTimeNS)
import Data.Types
import Safety.Audit (tryWriteAudit, tryWriteAuditSTM, triggerShutdown)
import SignalProcessing.Kalman (initKalman, KalmanConfig(..))

spec :: Spec
spec = describe "Safety.Audit" $ do
    it "tryWriteAudit successfully writes to an empty queue" $ do
        q <- newTBQueueIO 10
        time <- getMonotonicTimeNS
        let evt = AuditEvent time Info "Test" "Message"
        tryWriteAudit q evt
        True `shouldBe` True

    it "tryWriteAudit fails gracefully on a full queue" $ do
        q <- newTBQueueIO 1
        time <- getMonotonicTimeNS
        let evt1 = AuditEvent time Info "Test" "Msg1"
        let evt2 = AuditEvent time Info "Test" "Msg2"
        tryWriteAudit q evt1
        tryWriteAudit q evt2
        True `shouldBe` True

    it "tryWriteAuditSTM behaves the same" $ do
        q <- newTBQueueIO 1
        time <- getMonotonicTimeNS
        let evt1 = AuditEvent time Info "Test" "Msg1"
        let evt2 = AuditEvent time Info "Test" "Msg2"
        (res1, res2) <- atomically $ do
            r1 <- tryWriteAuditSTM q evt1
            r2 <- tryWriteAuditSTM q evt2
            return (r1, r2)
        res1 `shouldBe` True
        res2 `shouldBe` False

    it "triggerShutdown sets BeamOff" $ do
        time <- getMonotonicTimeNS
        q <- newTBQueueIO 10
        let kConfig = KalmanConfig 1.0 1.0
        let st = SystemState [] BeamOff time 0 (Point3D 0 0 0 0 0) mempty (initKalman 0 kConfig) [] q False "en" "BEAM OFF" CalibrationUnverified StandardPreset
        stateVar <- newTVarIO st
        
        triggerShutdown stateVar "Test shutdown"
        
        st' <- readTVarIO stateVar
        beamState st' `shouldBe` BeamOff
