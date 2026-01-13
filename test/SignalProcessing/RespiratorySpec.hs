{-# LANGUAGE OverloadedStrings #-}
module SignalProcessing.RespiratorySpec (spec) where

import Test.Hspec
import Test.QuickCheck
import Data.Complex
import Numeric.LinearAlgebra
import qualified Data.Set as Set
import qualified Data.Vector.Storable as V
import Data.Types (Point3D(..))

import SignalProcessing.Respiratory.Physics
import SignalProcessing.Respiratory.Methods
import SignalProcessing.Respiratory.Validation
import SignalProcessing.Respiratory.Calibration

spec :: Spec
spec = do
  describe "SignalProcessing.Respiratory.Physics" $ do
    it "calculates Lambertian order correctly" $ do
      -- If half-power angle is 60 deg (pi/3), cos(60) = 0.5.
      -- n = -ln(2) / ln(0.5) = -ln(2) / -ln(2) = 1.
      let phiHalf = pi / 3
      lambertianOrder phiHalf `shouldSatisfy` (\x -> abs (x - 1.0) < 1e-6)

    it "calculates photocurrent correctly" $ do
      photoCurrent 1e-9 0.5 1e-3 `shouldBe` (1e-9 + 0.5 * 1e-3)

  describe "SignalProcessing.Respiratory.Methods" $ do
    describe "Weighted Average Height" $ do
      it "computes average Z" $ do
        let pts = [ Point3D 0 0 10 0 0, Point3D 0 0 20 0 0 ]
        weightedAverageHeight pts `shouldBe` 15.0

    describe "SNR Improvement" $ do
      it "follows sqrt(N) law" $ do
        snrImprovement 100 `shouldBe` 10.0

    describe "radix2FFT" $ do
      it "computes FFT of a DC signal correctly" $ do
        let input = fromList [1 :+ 0, 1 :+ 0, 1 :+ 0, 1 :+ 0] :: Vector (Complex Double)
        let output = radix2FFT input
        -- FFT of [1,1,1,1] is [4, 0, 0, 0]
        let expected = fromList [4 :+ 0, 0 :+ 0, 0 :+ 0, 0 :+ 0]
        norm_2 (output - expected) `shouldSatisfy` (< 1e-6)

    describe "Time Delay Analysis" $ do
      it "detects simple shift with power-of-2 length" $ do
        let sigA = fromList [0, 1, 0, 0] :: Vector Double
        let sigB = fromList [0, 0, 1, 0] :: Vector Double
        -- B is delayed by 1 relative to A.
        timeDelayAnalysis sigA sigB `shouldBe` 1

      it "handles non-power-of-2 inputs by padding (length 5)" $ do
        -- Signal A: [0, 1, 0, 0, 0]
        -- Signal B: [0, 0, 1, 0, 0] (Shifted by 1)
        -- Padding to next power of 2 (8) should still preserve the peak at 1.
        let sigA = fromList [0, 1, 0, 0, 0] :: Vector Double
        let sigB = fromList [0, 0, 1, 0, 0] :: Vector Double
        timeDelayAnalysis sigA sigB `shouldBe` 1

  describe "SignalProcessing.Respiratory.Validation" $ do
    it "calculates RMSE correctly" $ do
      let v1 = fromList [1, 2, 3]
      let v2 = fromList [1, 2, 3]
      rootMeanSquareError v1 v2 `shouldBe` 0.0

      let v3 = fromList [2, 3, 4] -- Error is 1 everywhere. RMSE should be 1.
      rootMeanSquareError v1 v3 `shouldBe` 1.0

    it "calculates Pearson Correlation correctly" $ do
      let v1 = fromList [1, 2, 3]
      let v2 = fromList [2, 4, 6] -- Perfectly correlated
      pearsonCorrelation v1 v2 `shouldSatisfy` (\x -> abs (x - 1.0) < 1e-6)

    it "calculates DICE coefficient correctly" $ do
      let s1 = Set.fromList [1, 2, 3] :: Set.Set Int
      let s2 = Set.fromList [2, 3, 4] :: Set.Set Int
      -- Intersect: {2, 3} (size 2)
      -- Size A: 3, Size B: 3.
      -- DICE = 2 * 2 / (3 + 3) = 4 / 6 = 2/3
      diceSimilarityCoefficient s1 s2 `shouldBe` (2.0 / 3.0)

  describe "SignalProcessing.Respiratory.Calibration" $ do
    it "derives linear calibration parameters correctly" $ do
      -- y = 2x + 1
      let x = fromList [0, 1, 2, 3]
      let y = fromList [1, 3, 5, 7]
      let params = linearCalibration x y
      slope params `shouldSatisfy` (\v -> abs (v - 2.0) < 1e-6)
      intercept params `shouldSatisfy` (\v -> abs (v - 1.0) < 1e-6)

      calibrateValue params 4 `shouldSatisfy` (\v -> abs (v - 9.0) < 1e-6)
