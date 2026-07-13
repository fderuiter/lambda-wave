module Safety.KinematicsSpec (spec) where

import Test.Hspec
import Numeric.Kinematics
import Safety.Result (SafetyResult(..))

spec :: Spec
spec = do
  describe "KinematicMath non-negative enforcement (Requirement 1)" $ do
    it "safeAddDistance clamps negative results to 0" $ do
      let r = Meters (-5.0) |+| Meters 3.0
      r `shouldBe` ClampedToMin (Meters 0)

    it "safeSubDistance clamps negative results to 0" $ do
      let r = Meters 5.0 |-| Meters 10.0
      r `shouldBe` ClampedToMin (Meters 0)
      
    it "safeSubTime clamps negative results to 0" $ do
      let r = Seconds 1.0 |-| Seconds 2.0
      r `shouldBe` ClampedToMin (Seconds 0)

    it "safeSubFrequency clamps negative results to 0" $ do
      let r = Hertz 10.0 |-| Hertz 15.0
      r `shouldBe` ClampedToMin (Hertz 0)

  describe "Kinematic division handles zero/near-zero (Requirement 2)" $ do
    it "Meters / Seconds handles zero denominator" $ do
      let r = Meters 5.0 |/| Seconds 0.0
      r `shouldBe` DivByZeroSafe (MetersPerSecond (maxVelocity defaultBounds))
      
    it "Meters / Seconds handles near-zero denominator" $ do
      let r = Meters 5.0 |/| Seconds 1e-13
      r `shouldBe` DivByZeroSafe (MetersPerSecond (maxVelocity defaultBounds))

    it "MetersPerSecond / Meters handles zero denominator" $ do
      let r = MetersPerSecond 5.0 |/| Meters 0.0
      r `shouldBe` DivByZeroSafe (Hertz 1000.0)

  describe "Clinical Clamping for Velocity and Acceleration (Requirement 3)" $ do
    it "Velocity is clamped to min bounds" $ do
      let r = MetersPerSecond 0.005 |+| MetersPerSecond 0.001
      r `shouldBe` ClampedToMin (MetersPerSecond (minVelocity defaultBounds))
      
    it "Velocity is clamped to max bounds" $ do
      let r = MetersPerSecond 0.08 |+| MetersPerSecond 0.05
      r `shouldBe` ClampedToMax (MetersPerSecond (maxVelocity defaultBounds))

    it "Acceleration is clamped to max bounds" $ do
      let r = MetersPerSecondSquared 0.09 |+| MetersPerSecondSquared 0.05
      r `shouldBe` ClampedToMax (MetersPerSecondSquared (maxAcceleration defaultBounds))
      
    it "Negative Velocity is clamped to max negative bounds" $ do
      let r = MetersPerSecond (-0.08) |-| MetersPerSecond 0.05
      r `shouldBe` ClampedToMax (MetersPerSecond (- (maxVelocity defaultBounds)))

    it "Negative Velocity is clamped to min negative bounds" $ do
      let r = MetersPerSecond (-0.005) |-| MetersPerSecond 0.001
      r `shouldBe` ClampedToMin (MetersPerSecond (- (minVelocity defaultBounds)))

  describe "SafetyResult propagation (Requirement 4)" $ do
    it "Multiplication returns safe results" $ do
      let r = MetersPerSecond 0.05 |*| Seconds 1.0
      r `shouldBe` Safe (Meters 0.05)
      
    it "Division returns safe results when valid" $ do
      let r = Meters 0.05 |/| Seconds 1.0
      r `shouldBe` Safe (MetersPerSecond 0.05)

