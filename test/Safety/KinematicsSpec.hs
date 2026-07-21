module Safety.KinematicsSpec (spec) where

import Numeric.Kinematics
import Safety.Result (SafetyResult (..))
import Test.Hspec

spec :: Spec
spec = do
  describe "KinematicMath non-negative enforcement (Requirement 1)" $ do
    it "safeAddDistance clamps negative results to 0" $ do
      let r = Distance (-5.0) |+| Distance 3.0
      r `shouldBe` ClampedToMin (Distance 0)

    it "safeSubDistance clamps negative results to 0" $ do
      let r = Distance 5.0 |-| Distance 10.0
      r `shouldBe` ClampedToMin (Distance 0)

    it "safeSubTime clamps negative results to 0" $ do
      let r = Time 1.0 |-| Time 2.0
      r `shouldBe` ClampedToMin (Time 0)

    it "safeSubFrequency clamps negative results to 0" $ do
      let r = Frequency 10.0 |-| Frequency 15.0
      r `shouldBe` ClampedToMin (Frequency 0)

  describe "Kinematic division handles zero/near-zero (Requirement 2)" $ do
    it "Distance / Time handles zero denominator" $ do
      let r = Distance 5.0 |/| Time 0.0
      r `shouldBe` DivByZeroSafe (Velocity (maxVelocity defaultBounds))

    it "Distance / Time handles near-zero denominator" $ do
      let r = Distance 5.0 |/| Time 1e-13
      r `shouldBe` DivByZeroSafe (Velocity (maxVelocity defaultBounds))

    it "Velocity / Distance handles zero denominator" $ do
      let r = Velocity 5.0 |/| Distance 0.0
      r `shouldBe` DivByZeroSafe (Frequency 1000.0)

  describe "Clinical Clamping for Velocity and Acceleration (Requirement 3)" $ do
    it "Velocity is clamped to min bounds" $ do
      let r = Velocity 0.005 |+| Velocity 0.001
      r `shouldBe` ClampedToMin (Velocity (minVelocity defaultBounds))

    it "Velocity is clamped to max bounds" $ do
      let r = Velocity 0.08 |+| Velocity 0.05
      r `shouldBe` ClampedToMax (Velocity (maxVelocity defaultBounds))

    it "Acceleration is clamped to max bounds" $ do
      let r = Acceleration 0.09 |+| Acceleration 0.05
      r `shouldBe` ClampedToMax (Acceleration (maxAcceleration defaultBounds))

    it "Negative Velocity is clamped to max negative bounds" $ do
      let r = Velocity (-0.08) |-| Velocity 0.05
      r `shouldBe` ClampedToMax (Velocity (-(maxVelocity defaultBounds)))

    it "Negative Velocity is clamped to min negative bounds" $ do
      let r = Velocity (-0.005) |-| Velocity 0.001
      r `shouldBe` ClampedToMin (Velocity (-(minVelocity defaultBounds)))

  describe "SafetyResult propagation (Requirement 4)" $ do
    it "Multiplication returns safe results" $ do
      let r = Velocity 0.05 |*| Time 1.0
      r `shouldBe` Safe (Distance 0.05)

    it "Division returns safe results when valid" $ do
      let r = Distance 0.05 |/| Time 1.0
      r `shouldBe` Safe (Velocity 0.05)
