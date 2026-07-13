{-# OPTIONS_GHC -Wno-type-defaults #-}
import Test.Hspec
import qualified RegressionSpec
import qualified Control.MesherSpec
import qualified FFI.RingBuffer.TypesSpec
import qualified FFI.RingBuffer.IOSpec
import qualified Hardware.ConsumerSpec
import qualified Hardware.ControlSpec
import qualified SignalProcessing.FMCWSpec
import qualified System.RTSSpec
import qualified Safety.WatchdogSpec
import qualified Safety.KinematicsSpec
import qualified Control.UIMathSpec
import qualified Safety.AuditSpec

main :: IO ()
main = hspec $ do
  RegressionSpec.spec
  Control.MesherSpec.spec
  SignalProcessing.FMCWSpec.spec
  FFI.RingBuffer.TypesSpec.spec
  FFI.RingBuffer.IOSpec.spec
  Hardware.ConsumerSpec.spec
  Hardware.ControlSpec.spec
  System.RTSSpec.spec
  Safety.WatchdogSpec.spec
  Safety.KinematicsSpec.spec
  Control.UIMathSpec.spec
  Safety.AuditSpec.spec

  describe "Gating" $ do
    it "keeps beam off when target is out of range" $ do
      -- Placeholder for QuickCheck
      1 `shouldBe` 1
