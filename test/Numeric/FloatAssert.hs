module Numeric.FloatAssert
  ( approxEq,
    shouldBeApprox,
    shouldBeApproxList,
  )
where

import Control.Monad (zipWithM_)
import Test.Hspec (Expectation, shouldBe, shouldSatisfy)

-- | Centralized absolute tolerance helper
approxEq :: Double -> Double -> Double -> Bool
approxEq a b epsilon = abs (a - b) < epsilon

-- | Centralized float expectation
shouldBeApprox :: Double -> Double -> Double -> Expectation
shouldBeApprox actual expected epsilon =
  actual `shouldSatisfy` (\x -> approxEq x expected epsilon)

-- | Centralized list expectation
shouldBeApproxList :: [Double] -> [Double] -> Double -> Expectation
shouldBeApproxList actual expected epsilon = do
  length actual `shouldBe` length expected
  zipWithM_ (\a e -> shouldBeApprox a e epsilon) actual expected
