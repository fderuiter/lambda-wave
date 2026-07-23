{-# LANGUAGE ScopedTypeVariables #-}

module Safety.KinematicsSpec (spec) where

import Foreign.Marshal.Alloc (alloca)
import Foreign.Storable (alignment, peek, poke, sizeOf)
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

  describe "Unified Coordinate System (Type-Safe Units)" $ do
    it "instantiates coordinates tagged with physical unit dimensions" $ do
      let p1 :: Coordinate MillimetersUnit Double = Vector3D 1000 2000 3000
      coordX p1 `shouldBe` 1000.0
      coordY p1 `shouldBe` 2000.0
      coordZ p1 `shouldBe` 3000.0

    it "supports explicit unit conversion from Millimeters to Meters" $ do
      let p1 :: Coordinate MillimetersUnit Double = Vector3D 1500 2500 3500
          p2 :: Coordinate MetersUnit Double = convertUnit p1
      coordX p2 `shouldBe` 1.5
      coordY p2 `shouldBe` 2.5
      coordZ p2 `shouldBe` 3.5

    it "supports explicit unit conversion from Meters to Millimeters" $ do
      let p1 :: Coordinate MetersUnit Double = Vector3D 1.5 2.5 3.5
          p2 :: Coordinate MillimetersUnit Double = convertUnit p1
      coordX p2 `shouldBe` 1500.0
      coordY p2 `shouldBe` 2500.0
      coordZ p2 `shouldBe` 3500.0

    it "supports direct, loss-free coordinate projection and conversion between double-precision and single-precision formats" $ do
      let p1 :: Coordinate MillimetersUnit Double = Vector3D 12.34 56.78 90.12
          p2 :: Coordinate MillimetersUnit Float = convertPrecision p1
      coordX p2 `shouldBe` (12.34 :: Float)
      coordY p2 `shouldBe` (56.78 :: Float)
      coordZ p2 `shouldBe` (90.12 :: Float)

    it "supports standard vector math operations when unit tags match" $ do
      let p1 :: Coordinate MillimetersUnit Double = Vector3D 100 200 300
          p2 :: Coordinate MillimetersUnit Double = Vector3D 10 20 30
          pAdd = addCoords p1 p2
          pSub = subCoords p1 p2
          pScale = scaleCoord 2.0 p1
      pAdd `shouldBe` Vector3D 110.0 220.0 330.0
      pSub `shouldBe` Vector3D 90.0 180.0 270.0
      pScale `shouldBe` Vector3D 200.0 400.0 600.0

    it "supports Storable instance sizeOf, alignment, and round-trip peek/poke" $ do
      let p1 :: Coordinate MillimetersUnit Double = Vector3D 1.2 3.4 5.6
      sizeOf p1 `shouldBe` (3 * sizeOf (0.0 :: Double))
      alignment p1 `shouldBe` alignment (0.0 :: Double)
      alloca $ \ptr -> do
        poke ptr p1
        p2 <- peek ptr
        p2 `shouldBe` p1
