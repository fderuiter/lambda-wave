{-# LANGUAGE StrictData #-}
{-# LANGUAGE PatternSynonyms #-}
module Control.UIMathSpec (spec) where

import Test.Hspec
import GHC.Float (double2Float)
import UI.Presentation (shouldTriggerAudioAlert)
import Data.Types (BeamState(..))
import Numeric.Kinematics 
  ( Millimeters(..)
  , Meters(..)
  , mmToMeters
  , Coordinate(..)
  , pattern Vector3D
  , sub
  , angleBetween
  )

-- | Mock types for verification
data Vertex3 a = Vertex3 a a a deriving (Show, Eq)

-- | Pure transformation logic to verify
-- Transforms a radar point (mm) to OpenGL coordinates (meters)
transformPoint :: Coordinate -> Vertex3 Float
transformPoint p =
    let Meters mx = mmToMeters (Millimeters (coordX p))
        Meters my = mmToMeters (Millimeters (coordY p))
        Meters mz = mmToMeters (Millimeters (coordZ p))
        x = double2Float mx
        y = double2Float my
        z = double2Float mz
    in Vertex3 x y z

spec :: Spec
spec = describe "Control.UI.Math" $ do
    describe "Coordinate Transformation (mm to meters)" $ do
        it "correctly scales and converts Coordinate to Vertex3" $ do
            let p1 = Vector3D 1000 2000 3000
            let v1 = transformPoint p1
            let expected = Vertex3 1.0 2.0 3.0
            v1 `shouldBe` expected

        it "handles negative coordinates" $ do
            let p = Vector3D (-500) (-100) 0
            let vec = transformPoint p
            let expected = Vertex3 (-0.5) (-0.1) 0.0
            vec `shouldBe` expected

    describe "Camera Projection Logic (FOV Coverage)" $ do
        it "ensures a target at (0, 0, 2m) is centered in view from (0, 2, -2)" $ do
            let cameraPos = Vector3D 0.0 2.0 (-2.0)
            let lookAtPos = Vector3D 0.0 0.0 2.0
            let forward = sub lookAtPos cameraPos

            -- Target is at lookAtPos, so angle should be 0
            let targetVec = sub lookAtPos cameraPos
            let angle = angleBetween forward targetVec
            angle `shouldSatisfy` (< 1.0e-5)

        it "ensures edge point (2m lateral) is within horizontal FOV" $ do
            let cameraPos = Vector3D 0.0 2.0 (-2.0)
            let lookAtPos = Vector3D 0.0 0.0 2.0
            let forward = sub lookAtPos cameraPos

            let edgePoint = Vector3D 2.0 0.0 2.0
            let toEdge = sub edgePoint cameraPos

            let angle = angleBetween forward toEdge
            -- Angle should be ~24 degrees
            angle `shouldSatisfy` (\x -> x > 24.0 && x < 25.0)

            -- Assuming Horizontal FOV > 50 degrees (half > 25), it is visible.
            -- 24.1 < 25.
            angle `shouldSatisfy` (< 35.0) -- Safe margin

    describe "Audio Alert Logic (shouldTriggerAudioAlert)" $ do
        it "beeps when transitioning from BeamHold to BeamOff with alerts enabled" $ do
            shouldTriggerAudioAlert True BeamHold BeamOff `shouldBe` True

        it "beeps when transitioning from BeamOn to BeamOff with alerts enabled" $ do
            shouldTriggerAudioAlert True BeamOn BeamOff `shouldBe` True

        it "does not beep when remaining in BeamOff (startup/steady state)" $ do
            shouldTriggerAudioAlert True BeamOff BeamOff `shouldBe` False

        it "beeps when transitioning from BeamOff to BeamOn" $ do
            shouldTriggerAudioAlert True BeamOff BeamOn `shouldBe` True

        it "does not beep on any transition if alerts are disabled" $ do
            shouldTriggerAudioAlert False BeamHold BeamOff `shouldBe` False
            shouldTriggerAudioAlert False BeamOn BeamOff `shouldBe` False

-- Requirement FR-UI-001
-- Requirement FR-UI-002
