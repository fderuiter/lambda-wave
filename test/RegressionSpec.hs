{-# OPTIONS_GHC -Wno-type-defaults #-}
module RegressionSpec (spec) where

import Test.Hspec
import SignalProcessing.Regression
import Control.Monad (zipWithM_)

-- | Helper to check if a BiQuadratic matches expected coefficients
shouldBeApproxBi :: Maybe BiQuadratic -> [Double] -> Expectation
shouldBeApproxBi actual expected = case actual of
    Nothing -> expectationFailure "Expected BiQuadratic, got Nothing"
    Just (BiQuadratic valB0 valB1 valB2 valB3 valB4) -> do
        length expected `shouldBe` 5
        let actualList = [valB0, valB1, valB2, valB3, valB4]
        zipWithM_ (\a e -> a `shouldSatisfy` (\x -> abs (x - e) < 1e-6)) actualList expected

-- | Helper to check if a StrictBiQuadratic matches expected coefficients
shouldBeApproxStrict :: Maybe StrictBiQuadratic -> [Double] -> Expectation
shouldBeApproxStrict actual expected = case actual of
    Nothing -> expectationFailure "Expected StrictBiQuadratic, got Nothing"
    Just (StrictBiQuadratic valC0 valC2 valC4) -> do
        length expected `shouldBe` 3
        let actualList = [valC0, valC2, valC4]
        zipWithM_ (\a e -> a `shouldSatisfy` (\x -> abs (x - e) < 1e-6)) actualList expected

spec :: Spec
spec = describe "SignalProcessing.Regression" $ do
    describe "solveBiQuadratic" $ do
        it "solves a perfect bi-quadratic polynomial (y = 1 + 2x + 3x^2 + 4x^3 + 5x^4)" $ do
            let xs = [0.0, 1.0, 2.0, 3.0, 4.0, 5.0, 6.0]
                ys = [1 + 2*x + 3*x^2 + 4*x^3 + 5*x^4 | x <- xs]
                expected = [1.0, 2.0, 3.0, 4.0, 5.0]
            solveBiQuadratic xs ys `shouldBeApproxBi` expected

        it "returns Nothing when input lengths mismatch" $ do
            solveBiQuadratic [1, 2, 3] [1, 2] `shouldBe` Nothing

        it "returns Nothing for singular systems (too few points)" $ do
            let xs = [0, 1, 2]
                ys = [1, 2, 3]
            -- 3 points is not enough for 5 params
            solveBiQuadratic xs ys `shouldBe` Nothing

    describe "solveStrictBiQuadratic" $ do
        it "solves a perfect strict bi-quadratic (y = 2 + 4x^2 + 6x^4)" $ do
            let xs = [0.0, 1.0, 2.0, 3.0, 4.0]
                ys = [2 + 4*x^2 + 6*x^4 | x <- xs]
                expected = [2.0, 4.0, 6.0]
            solveStrictBiQuadratic xs ys `shouldBeApproxStrict` expected

        it "returns Nothing when input lengths mismatch" $ do
            solveStrictBiQuadratic [1, 2] [1] `shouldBe` Nothing

    describe "predict" $ do
        it "predicts bi-quadratic correctly" $ do
            let bq = BiQuadratic 1 2 3 4 5
                x = 2.0
                expected = 1 + 2*x + 3*x^2 + 4*x^3 + 5*x^4
            predict bq x `shouldBe` expected

        it "predicts strict bi-quadratic correctly" $ do
            let sbq = StrictBiQuadratic 2 4 6
                x = 3.0
                expected = 2 + 4*x^2 + 6*x^4
            predict sbq x `shouldBe` expected
