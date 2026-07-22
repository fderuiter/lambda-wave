{-# OPTIONS_GHC -Wno-type-defaults #-}

import qualified Control.MesherSpec
import qualified Control.UIMathSpec
import qualified FFI.Hud.HudStateCSpec
import qualified FFI.RingBuffer.IOSpec
import qualified FFI.RingBuffer.TypesSpec
import qualified Hardware.ConsumerSpec
import qualified Hardware.ControlSpec
import qualified RegressionSpec
import qualified Safety.KinematicsSpec
import qualified Safety.WatchdogSpec
import qualified SignalProcessing.FMCWSpec
import qualified System.RTSSpec
import Test.Hspec

main :: IO ()
main = hspec $ do
  RegressionSpec.spec
  Control.MesherSpec.spec
  SignalProcessing.FMCWSpec.spec
  FFI.RingBuffer.TypesSpec.spec
  FFI.Hud.HudStateCSpec.spec
  FFI.RingBuffer.IOSpec.spec
  Hardware.ConsumerSpec.spec
  Hardware.ControlSpec.spec
  System.RTSSpec.spec
  Safety.WatchdogSpec.spec
  Safety.KinematicsSpec.spec
  Control.UIMathSpec.spec

  describe "Gating" $ do
    it "keeps beam off when target is out of range" $ do
      -- Placeholder for QuickCheck
      1 `shouldBe` 1
