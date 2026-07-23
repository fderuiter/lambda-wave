module Control.MesherSpec (spec) where

import Control.Mesher
import Data.Types
import Numeric.FloatAssert (shouldBeApproxList)
import Test.Hspec

spec :: Spec
spec = describe "Control.Mesher" $ do
  describe "reconstructPolynomialSurface" $ do
    it "reconstructs a 20x20 mesh (400 points)" $ do
      let coeffs = PolynomialSurface 1.0 0.0 0.0 0.0 0.0 0.0
          mPts = reconstructPolynomialSurface coeffs
      case mPts of
        Just pts -> do
          length pts `shouldBe` 400
          map pz pts `shouldBe` replicate 400 1.0
        Nothing -> fail "reconstructPolynomialSurface returned Nothing"

  describe "fitPolynomialSurface" $ do
    it "returns Nothing when there are fewer than 6 points" $ do
      let pts = [Point3D x y (1 + x + y) 0 0 | let y = 0, x <- [0 .. 4]]
      length pts `shouldBe` 5
      fitPolynomialSurface pts `shouldBe` Nothing

    it "fits a perfect plane (z = 1 + 2x + 3y)" $ do
      let pts =
            [ Point3D 0 0 1 0 0,
              Point3D 1 0 3 0 0,
              Point3D 0 1 4 0 0,
              Point3D 1 1 6 0 0,
              Point3D 2 0 5 0 0,
              Point3D 0 2 7 0 0
            ]
      case fitPolynomialSurface pts of
        Just (PolynomialSurface c0 c1 c2 c3 c4 c5) ->
          shouldBeApproxList [c0, c1, c2, c3, c4, c5] [1.0, 2.0, 3.0, 0.0, 0.0, 0.0] 1e-6
        Nothing -> fail "fitPolynomialSurface returned Nothing"

    it "fits a full quadratic surface (z = 1 + x + y + x^2 + xy + y^2)" $ do
      let f x y = 1 + x + y + x * x + x * y + y * y
          pts = [Point3D x y (f x y) 0 0 | x <- [0 .. 2], y <- [0 .. 2]]
          -- This generates 9 points, enough for 6 coefficients
      case fitPolynomialSurface pts of
        Just (PolynomialSurface c0 c1 c2 c3 c4 c5) ->
          shouldBeApproxList [c0, c1, c2, c3, c4, c5] [1.0, 1.0, 1.0, 1.0, 1.0, 1.0] 1e-6
        Nothing -> fail "fitPolynomialSurface returned Nothing"

    it "handles a singular case (collinear points) by returning Nothing" $ do
      -- If the matrix is singular, leastSquares returns Nothing,
      -- and fitPolynomialSurface returns Nothing
      let pts = [Point3D x 0 1 0 0 | x <- [0 .. 10]] -- All points on y=0 line
      fitPolynomialSurface pts `shouldBe` Nothing
