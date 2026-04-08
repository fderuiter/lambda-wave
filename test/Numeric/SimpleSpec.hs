module Numeric.SimpleSpec (spec) where

import Test.Hspec
import Numeric.Simple

spec :: Spec
spec = do
  describe "dot" $ do
    it "calculates the dot product of two vectors" $ do
      dot [1, 2, 3] [4, 5, 6] `shouldBe` (1*4 + 2*5 + 3*6)

    it "returns 0 for empty vectors" $ do
      dot [] [] `shouldBe` 0
      dot [1, 2] [] `shouldBe` 0
      dot [] [1, 2] `shouldBe` 0

    it "handles vectors of different lengths by stopping at the shortest" $ do
      dot [1, 2, 3] [4, 5] `shouldBe` (1*4 + 2*5)
      dot [1, 2] [4, 5, 6] `shouldBe` (1*4 + 2*5)

  describe "multiply" $ do
    let a = [[1, 2], [3, 4]]
    let b = [[5, 6], [7, 8]]

    it "multiplies two square matrices correctly" $ do
      multiply a b `shouldBe` Just [[19, 22], [43, 50]]

    it "multiplies rectangular matrices correctly" $ do
      let r1 = [[1, 2, 3], [4, 5, 6]] -- 2x3
      let r2 = [[7, 8], [9, 10], [11, 12]] -- 3x2
      multiply r1 r2 `shouldBe` Just [[58, 64], [139, 154]]

    it "returns Nothing for dimension mismatch" $ do
      let mismatch = [[1, 2]] -- 1x2
      multiply a mismatch `shouldBe` Nothing

    it "returns Nothing for jagged matrices" $ do
      let jagged = [[1, 2], [3]]
      multiply jagged a `shouldBe` Nothing
      multiply a jagged `shouldBe` Nothing

    it "returns Just [] for empty matrix A" $ do
      multiply [] a `shouldBe` Just []

    it "multiplies by identity matrix correctly" $ do
      let id2 = identity 2
      multiply a id2 `shouldBe` Just a
      multiply id2 a `shouldBe` Just a

  describe "matVecMult" $ do
    it "multiplies a matrix by a vector" $ do
      let m = [[1, 2], [3, 4]]
      let v = [5, 6]
      matVecMult m v `shouldBe` [17, 39]

    it "handles dimension mismatch by dot product behavior" $ do
      let m = [[1, 2, 3], [4, 5, 6]]
      let v = [1, 1]
      -- [1*1 + 2*1, 4*1 + 5*1]
      matVecMult m v `shouldBe` [3, 9]

  describe "identity" $ do
    it "creates an identity matrix of size N" $ do
      identity 1 `shouldBe` [[1.0]]
      identity 2 `shouldBe` [[1.0, 0.0], [0.0, 1.0]]
      identity 3 `shouldBe` [[1.0, 0.0, 0.0], [0.0, 1.0, 0.0], [0.0, 0.0, 1.0]]

    it "returns an empty list for size 0" $ do
      identity 0 `shouldBe` []

  describe "transpose" $ do
    it "transposes a matrix" $ do
      transpose [[1, 2], [3, 4]] `shouldBe` [[1, 3], [2, 4]]
      transpose [[1, 2, 3]] `shouldBe` [[1], [2], [3]]
      transpose [[1], [2], [3]] `shouldBe` [[1, 2, 3]]
