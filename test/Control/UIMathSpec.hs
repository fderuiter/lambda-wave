{-# LANGUAGE StrictData #-}
module Control.UIMathSpec (spec) where

import Test.Hspec
-- Removed unused Control.Monad (unless)
import GHC.Float (double2Float)
import Control.UI.Renderer (shouldBeep)
import Data.Types (BeamState(..))

-- | Mock types for verification
-- Removed unused fields 'v' and 'snr' to satisfy -Wunused-top-binds
data Point3D = Point3D { px :: Double, py :: Double, pz :: Double }
data Vertex3 a = Vertex3 a a a deriving (Show, Eq)

-- | Pure transformation logic to verify
-- Transforms a radar point (mm) to OpenGL coordinates (meters)
transformPoint :: Point3D -> Vertex3 Float
transformPoint p =
    let x = double2Float (px p) / 1000.0
        y = double2Float (py p) / 1000.0
        z = double2Float (pz p) / 1000.0
    in Vertex3 x y z

type Vector3 = (Double, Double, Double)

dot :: Vector3 -> Vector3 -> Double
dot (x1, y1, z1) (x2, y2, z2) = x1*x2 + y1*y2 + z1*z2

magnitude :: Vector3 -> Double
magnitude (x, y, z) = sqrt (x*x + y*y + z*z)

normalize :: Vector3 -> Vector3
normalize vec@(x, y, z) = -- Renamed 'v' to 'vec' to avoid shadowing
    let m = magnitude vec
    in if m == 0 then (0,0,0) else (x/m, y/m, z/m)

sub :: Vector3 -> Vector3 -> Vector3
sub (x1, y1, z1) (x2, y2, z2) = (x1-x2, y1-y2, z1-z2)

rad2deg :: Double -> Double
rad2deg r = r * 180.0 / pi

angleBetween :: Vector3 -> Vector3 -> Double
angleBetween v1 v2 =
    let n1 = normalize v1
        n2 = normalize v2
        d = dot n1 n2
        -- Clamp d to [-1, 1] to avoid NaN from acos
        d' = max (-1.0) (min 1.0 d)
    in rad2deg (acos d')

spec :: Spec
spec = describe "Control.UI.Math" $ do
    describe "Coordinate Transformation (mm to meters)" $ do
        it "correctly scales and converts Point3D to Vertex3" $ do
            let p1 = Point3D 1000 2000 3000
            let v1 = transformPoint p1
            let expected = Vertex3 1.0 2.0 3.0
            v1 `shouldBe` expected

        it "handles negative coordinates" $ do
            let p = Point3D (-500) (-100) 0
            let vec = transformPoint p -- Renamed 'v' to 'vec'
            let expected = Vertex3 (-0.5) (-0.1) 0.0
            vec `shouldBe` expected

    describe "Camera Projection Logic (FOV Coverage)" $ do
        it "ensures a target at (0, 0, 2m) is centered in view from (0, 2, -2)" $ do
            let cameraPos = (0.0, 2.0, -2.0)
            let lookAtPos = (0.0, 0.0, 2.0)
            let forward = sub lookAtPos cameraPos -- (0, -2, 4) -> (0, -0.447, 0.894)

            -- Target is at lookAtPos, so angle should be 0
            let targetVec = sub lookAtPos cameraPos
            let angle = angleBetween forward targetVec
            angle `shouldSatisfy` (< 1.0e-5)

        it "ensures edge point (2m lateral) is within horizontal FOV" $ do
            let cameraPos = (0.0, 2.0, -2.0)
            let lookAtPos = (0.0, 0.0, 2.0)
            let forward = sub lookAtPos cameraPos

            let edgePoint = (2.0, 0.0, 2.0)
            let toEdge = sub edgePoint cameraPos

            let angle = angleBetween forward toEdge
            -- Angle should be ~24 degrees
            angle `shouldSatisfy` (\x -> x > 24.0 && x < 25.0)

            -- Assuming Horizontal FOV > 50 degrees (half > 25), it is visible.
            -- 24.1 < 25.
            angle `shouldSatisfy` (< 35.0) -- Safe margin

    describe "Audio Alert Logic (shouldBeep)" $ do
        it "beeps when transitioning from BeamHold to BeamOff with alerts enabled" $ do
            shouldBeep True BeamHold BeamOff `shouldBe` True

        it "beeps when transitioning from BeamOn to BeamOff with alerts enabled" $ do
            shouldBeep True BeamOn BeamOff `shouldBe` True

        it "does not beep when remaining in BeamOff (startup/steady state)" $ do
            shouldBeep True BeamOff BeamOff `shouldBe` False

        it "does not beep when transitioning from BeamOff to BeamOn" $ do
            shouldBeep True BeamOff BeamOn `shouldBe` False

        it "does not beep on any transition if alerts are disabled" $ do
            shouldBeep False BeamHold BeamOff `shouldBe` False
            shouldBeep False BeamOn BeamOff `shouldBe` False

-- Requirement FR-UI-001
