module RegressionSpec (spec) where

import Test.Hspec
import Numeric.LinearAlgebra
import SignalProcessing.Regression

spec :: Spec
spec = do
  describe "Bi-Quadratic Regression" $ do
    it "solves a known quartic curve correctly" $ do
      let xData = vector [ -3.0, -2.0, -1.0, 0.0, 1.0, 2.0, 3.0 ]
          -- y = x^4 + 3 roughly, but using data from guide
          -- -3^4 = 81 + 3 = 84. Guide has 85.
          -- -2^4 = 16 + 3 = 19. Guide has 11.
          -- -1^4 = 1 + 3 = 4. Guide has 2.
          -- 0^4 = 0 + 3 = 3. Guide has 1.
          -- 1^4 = 1 + 3 = 4. Guide has 2.
          -- 2^4 = 16 + 3 = 19. Guide has 19.
          -- 3^4 = 81 + 3 = 84. Guide has 85.
          yData = vector [ 70.0, 11.0, 2.0, 1.0, 2.0, 19.0, 85.0 ]

          coeffs = solveBiQuadratic xData yData
          [b0, b1, b2, b3, b4] = toList coeffs

      -- Based on the guide description: "beta_4 approx 1.0 and beta_0 approx 1.0"
      -- Actually guide says "beta_4 approx 1.0 and beta_0 approx 1.0 (with other terms near zero)"
      -- Let's check if they are close.
      b4 `shouldSatisfy` (\v -> abs (v - 1.0) < 0.2)

      -- Let's verify prediction for x=2.5
      -- Guide says: "Prediction at x=2.5 is y=..." (It doesn't say the value, but we can compute)
      -- If y approx x^4, 2.5^4 = 39.0625.
      let predY = predict coeffs 2.5
      predY `shouldSatisfy` (> 30.0)
      predY `shouldSatisfy` (< 50.0)

    it "Strict Bi-Quadratic forces odd coefficients to zero (implicit)" $ do
       let xData = vector [ -3.0, -2.0, -1.0, 0.0, 1.0, 2.0, 3.0 ]
           yData = vector [ 82.0, 17.0, 2.0, 1.0, 2.0, 17.0, 82.0 ] -- Symmetric data y = x^4 + 1

           coeffs = solveStrictBiQuadratic xData yData
           -- coeffs should be [c0, c2, c4]
           [c0, c2, c4] = toList coeffs

           -- c4 should be approx 1.0
           c4 `shouldSatisfy` (\v -> abs (v - 1.0) < 0.1)
           -- c0 should be approx 1.0
           c0 `shouldSatisfy` (\v -> abs (v - 1.0) < 0.1)
           -- c2 should be approx 0.0
           c2 `shouldSatisfy` (\v -> abs v < 0.1)

  describe "Design Matrix" $ do
    it "creates correct dimensions" $ do
      let x = vector [1, 2, 3]
          m = createDesignMatrix x
      rows m `shouldBe` 3
      cols m `shouldBe` 5 -- 1, x, x^2, x^3, x^4
