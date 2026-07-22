module Control.MesherSpec (spec) where

import Control.Mesher
import Data.Types
import Numeric.FloatAssert (shouldBeApproxList)
import Test.Hspec

spec :: Spec
spec = describe "Control.Mesher" $ do
  describe "reconstructPolynomialSurface" $ do
    it "reconstructs a 20x20 mesh (400 points)" $ do
      let coeffs = [1.0, 0.0, 0.0, 0.0, 0.0, 0.0]
          pts = reconstructPolynomialSurface coeffs
      length pts `shouldBe` 400
      map pz pts `shouldBe` replicate 400 1.0

  describe "fitPolynomialSurface" $ do
    it "returns all zeros when there are fewer than 6 points" $ do
      let pts = [Point3D x y (1 + x + y) 0 0 | let y = 0, x <- [0 .. 4]]
      length pts `shouldBe` 5
      fitPolynomialSurface pts `shouldBe` replicate 6 0.0

    it "fits a perfect plane (z = 1 + 2x + 3y)" $ do
      let pts =
            [ Point3D 0 0 1 0 0,
              Point3D 1 0 3 0 0,
              Point3D 0 1 4 0 0,
              Point3D 1 1 6 0 0,
              Point3D 2 0 5 0 0,
              Point3D 0 2 7 0 0
            ]
          expected = [1.0, 2.0, 3.0, 0.0, 0.0, 0.0]
      shouldBeApproxList (fitPolynomialSurface pts) expected 1e-6

    it "fits a full quadratic surface (z = 1 + x + y + x^2 + xy + y^2)" $ do
      let f x y = 1 + x + y + x * x + x * y + y * y
          pts = [Point3D x y (f x y) 0 0 | x <- [0 .. 2], y <- [0 .. 2]]
          -- This generates 9 points, enough for 6 coefficients
          expected = [1.0, 1.0, 1.0, 1.0, 1.0, 1.0]
      shouldBeApproxList (fitPolynomialSurface pts) expected 1e-6

    it "handles a singular case (collinear points) by returning zeros" $ do
      -- If the matrix is singular, leastSquares returns Nothing,
      -- and fitPolynomialSurface returns replicate 6 0.0
      let pts = [Point3D x 0 1 0 0 | x <- [0 .. 10]] -- All points on y=0 line
      fitPolynomialSurface pts `shouldBe` replicate 6 0.0
