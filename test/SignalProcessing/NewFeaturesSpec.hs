module SignalProcessing.NewFeaturesSpec (spec) where

import Test.Hspec
import SignalProcessing.Fitting
import SignalProcessing.Interference
import SignalProcessing.OpticalFlow
import Numeric.LinearAlgebra
import Data.Types (Point3D(..))
import Data.Complex

spec :: Spec
spec = do
    describe "Surface Fitting" $ do
        it "fits a flat plane correctly" $ do
            -- Plane z = 2.0 (c0=2, others=0)
            let pts = [ Point3D (fromIntegral x) (fromIntegral y) 2.0 0 0 | x <- [0..4], y <- [0..4] ]
            let coeffs = fitSurfaceBiQuadratic pts
            -- Coeffs: [c0, c1, c2, c3, c5, c4] ?
            -- Implementation order: [1, x, y, x^2, xy, y^2] -> [c0, c1, c2, c3, c5, c4]
            -- Expect c0 ~ 2.0, others ~ 0
            let c0_est = coeffs ! 0
            c0_est `shouldSatisfy` (\v -> abs (v - 2.0) < 1e-5)

        it "fits a quadratic bowl correctly" $ do
            -- z = x^2 + y^2
            let pts = [ Point3D x y (x*x + y*y) 0 0 | x <- [-2..2], y <- [-2..2] ]
            let coeffs = fitSurfaceBiQuadratic pts
            -- Order: [1, x, y, x^2, y^2, xy]
            -- Expect c3 (x^2) ~ 1, c4 (y^2) ~ 1, others ~ 0
            let c3_est = coeffs ! 3 -- x^2
            let c4_est = coeffs ! 4 -- y^2
            c3_est `shouldSatisfy` (\v -> abs (v - 1.0) < 1e-5)
            c4_est `shouldSatisfy` (\v -> abs (v - 1.0) < 1e-5)

    describe "Interference Mitigation" $ do
        it "reconstructs interference (basic check)" $ do
            let params = InterferenceParams 100.0 1 0.1
            -- Simple peak at index 10
            let specData = fromList [ if i == 10 then 10.0 :+ 0 else 0 :+ 0 | i <- [0..19] ] :: Vector (Complex Double)
            let recon = reconstructInterference params specData 10
            size recon `shouldBe` 20
            -- Should be non-zero
            norm_2 recon `shouldSatisfy` (> 0)

        it "runs L1 smoothing without error" $ do
            let params = InterferenceParams 100.0 1 0.1
            let signal = fromList [ 1.0 :+ 0.0 | _ <- [0..15] ] :: Vector (Complex Double)
            let mask = fromList [ 1.0 | _ <- [0..15] ]
            let result = l1RegularizedSmoothing params signal mask
            size result `shouldBe` 16

    describe "Optical Flow" $ do
        it "calculates velocity field zero for identical images" $ do
            let img = (3><3) [1..9] :: Matrix R
            let dIdV = (3><3) [0,0,0, 0,0,0, 0,0,0] -- No change
            let (vx, vy) = calculateVelocityField img dIdV
            norm_2 vx `shouldBe` 0
            norm_2 vy `shouldBe` 0

        it "interpolates volume (shift)" $ do
            let params = OpticalFlowParams 1.0 1
            let img = (3><3) [1,1,1, 1,1,1, 1,1,1] :: Matrix R
            let (vx, vy) = ((3><3) [1,1,1, 1,1,1, 1,1,1], (3><3) [0,0,0, 0,0,0, 0,0,0])
            let result = interpolateVolume params img (vx, vy) 0.5
            size result `shouldBe` (3,3)

        it "computes PCA motion model basics" $ do
            let f1 = ((2><2) [1,0,0,1], (2><2) [0,0,0,0])
            let f2 = ((2><2) [2,0,0,2], (2><2) [0,0,0,0])
            let (meanFlow, comps) = pcaMotionModel [f1, f2] 1
            length comps `shouldBe` 1
