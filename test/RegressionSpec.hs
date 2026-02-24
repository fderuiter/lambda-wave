{-# OPTIONS_GHC -Wno-type-defaults #-}
module RegressionSpec (spec) where

import Test.Hspec
import SignalProcessing.Regression
import Control.Monad (zipWithM_)

-- | Helper to check if two vectors are approximately equal
shouldBeApprox :: [Double] -> [Double] -> Expectation
shouldBeApprox actual expected = do
    length actual `shouldBe` length expected
    zipWithM_ (\a e -> a `shouldSatisfy` (\x -> abs (x - e) < 1e-6)) actual expected

spec :: Spec
spec = describe "SignalProcessing.Regression" $ do
    describe "solveBiQuadratic" $ do
        it "solves a perfect bi-quadratic polynomial (y = 1 + 2x + 3x^2 + 4x^3 + 5x^4)" $ do
            let xs = [0.0, 1.0, 2.0, 3.0, 4.0, 5.0, 6.0]
                ys = [1 + 2*x + 3*x^2 + 4*x^3 + 5*x^4 | x <- xs]
                expected = [1.0, 2.0, 3.0, 4.0, 5.0]
            solveBiQuadratic xs ys `shouldBeApprox` expected

        it "returns zeros when input lengths mismatch" $ do
            solveBiQuadratic [1, 2, 3] [1, 2] `shouldBe` replicate 5 0.0

        it "returns zeros for singular systems (too few points)" $ do
            let xs = [0, 1, 2]
                ys = [1, 2, 3]
            solveBiQuadratic xs ys `shouldBe` replicate 5 0.0

    describe "solveStrictBiQuadratic" $ do
        it "solves a perfect strict bi-quadratic (y = 2 + 4x^2 + 6x^4)" $ do
            let xs = [0.0, 1.0, 2.0, 3.0, 4.0]
                ys = [2 + 4*x^2 + 6*x^4 | x <- xs]
                expected = [2.0, 4.0, 6.0]
            solveStrictBiQuadratic xs ys `shouldBeApprox` expected

        it "returns zeros when input lengths mismatch" $ do
            solveStrictBiQuadratic [1, 2] [1] `shouldBe` replicate 3 0.0

    describe "predict" $ do
        it "predicts bi-quadratic correctly" $ do
            let coeffs = [1, 2, 3, 4, 5]
                x = 2.0
                expected = 1 + 2*x + 3*x^2 + 4*x^3 + 5*x^4
            predict coeffs x `shouldBe` expected

        it "predicts strict bi-quadratic correctly" $ do
            let coeffs = [2, 4, 6]
                x = 3.0
                expected = 2 + 4*x^2 + 6*x^4
            predict coeffs x `shouldBe` expected

        it "returns 0 for invalid coefficient list" $ do
            predict [1, 2] 5.0 `shouldBe` 0.0
