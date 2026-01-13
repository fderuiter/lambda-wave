{-# LANGUAGE OverloadedStrings #-}
module SignalProcessing.AdvancedSpec (spec) where

import Test.Hspec
import Test.QuickCheck hiding ((><))
import SignalProcessing.Advanced
import Numeric.LinearAlgebra
import Data.Complex

approxEq :: Complex Double -> Complex Double -> Bool
approxEq (r1 :+ i1) (r2 :+ i2) = abs (r1 - r2) < 1e-6 && abs (i1 - i2) < 1e-6

spec :: Spec
spec = do
  describe "SignalProcessing.Advanced" $ do
    describe "chirpZTransformBluestein" $ do
      it "matches direct summation (DFT case) for small inputs" $ do
        -- When A=1, W=exp(-j 2pi/N), CZT is DFT.
        let n = 4
            k = 4
            a = 1.0 :+ 0.0
            w = cis (-2 * pi / fromIntegral n)
            params = CZTParams { cztA = a, cztW = w, cztN = n, cztK = k }

            input = fromList [1 :+ 0, 2 :+ 0, 3 :+ 0, 4 :+ 0]

            -- Expected DFT
            -- Naive DFT implementation since hmatrix fft is not available/reliable here
            naiveDFT :: Vector (Complex Double) -> Vector (Complex Double)
            naiveDFT v = fromList [ sum [ (v ! n_idx) * cis (-2 * pi * fromIntegral k_idx * fromIntegral n_idx / fromIntegral n)
                                        | n_idx <- [0 .. n - 1] ]
                                  | k_idx <- [0 .. k - 1] ]

            expected = naiveDFT input

            result = chirpZTransformBluestein params input

        -- Check each element
        mapM_ (\i -> (result ! i) `shouldSatisfy` (\v -> magnitude (v - (expected ! i)) < 1e-5)) [0 .. k - 1]

    describe "unwrapPhase" $ do
      it "handles small changes correctly" $ do
        unwrapPhase 0.1 0.0 `shouldBe` 0.1
        unwrapPhase (-0.1) 0.0 `shouldBe` -0.1

      it "unwraps positive wrap" $ do
        -- Jump from 3.0 to -3.0 (approx pi to -pi)
        -- Diff = -3.0 - 3.0 = -6.0. < -pi. Add 2pi.
        -- Result should be approx 0.28.
        let p1 = 3.0
            p2 = -3.0
            diff = p2 - p1 -- -6.0
            expected = diff + 2 * pi -- ~0.28
        unwrapPhase p2 p1 `shouldSatisfy` (\v -> abs (v - expected) < 1e-6)

      it "unwraps negative wrap" $ do
        -- Jump from -3.0 to 3.0
        -- Diff = 6.0. > pi. Subtract 2pi.
        let p1 = -3.0
            p2 = 3.0
            diff = p2 - p1
            expected = diff - 2 * pi
        unwrapPhase p2 p1 `shouldSatisfy` (\v -> abs (v - expected) < 1e-6)

    describe "beamform" $ do
      it "outputs correct dimensions" $ do
        let m = 4 -- antennas
            t = 10 -- time samples
            config = AntennaConfig { antennaCount = m, antennaSpacing = 0.5, signalWavelength = 1.0 }
            signals = (m><t) [ x :+ 0 | x <- [1..(fromIntegral (m*t))] ] :: Matrix (Complex Double)

            result = beamform config 0.0 Nothing signals

        size result `shouldBe` t

      it "sums broadside signals constructively" $ do
        -- 2 antennas, identical signal 1.0
        -- broadside (theta=0) -> steering vector [1, 1]
        -- sum should be 2.0
        let m = 2
            t = 1
            config = AntennaConfig { antennaCount = m, antennaSpacing = 0.5, signalWavelength = 1.0 }
            signals = (m><t) [1 :+ 0, 1 :+ 0] :: Matrix (Complex Double)

            result = beamform config 0.0 Nothing signals
            val = result ! 0

        magnitude (val - (2 :+ 0)) < 1e-6 `shouldBe` True
