{-|
Module      : Control.Mesher
Description : Surface Meshing using Pure Linear Algebra
Copyright   : (c) 2024
License     : AGPL-3.0-only
-}
module Control.Mesher (fitPolynomialSurface) where

import Data.Types
import Numeric.Simple
import Data.Maybe (fromMaybe)

-- | Fits a polynomial surface to the points
-- Model: z = c0 + c1*x + c2*y + c3*x^2 + c4*xy + c5*y^2
-- Uses 'Numeric.Simple.leastSquares' for dependency-free calculation.
--
-- Complexity: O(N * M^2) where N is points, M is 6 terms.
fitPolynomialSurface :: [Point3D] -> [Double]
fitPolynomialSurface pts
    | length pts < 6 = replicate 6 0.0 -- Not enough points
    | otherwise = fromMaybe (replicate 6 0.0) (leastSquares a b)
  where
    -- Design Matrix A
    -- Rows are points, Columns are terms [1, x, y, x^2, xy, y^2]
    a = map (\p -> [1, px p, py p, px p ^ (2::Int), px p * py p, py p ^ (2::Int)]) pts

    -- Vector b (z coordinates)
    b = map pz pts
