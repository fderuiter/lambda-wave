{-|
Module      : Control.Mesher
Description : Surface Meshing using Pure Linear Algebra
Copyright   : (c) 2024
License     : AGPL-3.0-only
-}
module Control.Mesher (fitPolynomialSurface, reconstructPolynomialSurface) where

import Data.Types
import Numeric.Simple
import Data.Maybe (fromMaybe)

-- | Reconstructs a 20x20 3D surface mesh from 6 polynomial coefficients
-- Model: z = c0 + c1*x + c2*y + c3*x^2 + c4*xy + c5*y^2
reconstructPolynomialSurface :: [Double] -> [Point3D]
reconstructPolynomialSurface coeffs = case coeffs of
    [c0, c1, c2, c3, c4, c5] ->
        [ Point3D x y z 0.0 0.0
        | xi <- [0..19 :: Int]
        , yi <- [0..19 :: Int]
        , let x = -100.0 + (fromIntegral xi * 200.0 / 19.0)
        , let y = -100.0 + (fromIntegral yi * 200.0 / 19.0)
        , let z = c0 + c1 * x + c2 * y + c3 * x * x + c4 * x * y + c5 * y * y
        ]
    _ -> []


-- | Fits a polynomial surface to the points
-- Model: z = c0 + c1*x + c2*y + c3*x^2 + c4*xy + c5*y^2
-- Uses 'Numeric.Simple.leastSquares' for dependency-free calculation.
--
-- Complexity: O(N * M^2) where N is points, M is 6 terms.
fitPolynomialSurface :: [Point3D] -> [Double]
fitPolynomialSurface pts
    | null (drop 5 pts) = replicate 6 0.0 -- Not enough points
    | otherwise = fromMaybe (replicate 6 0.0) (leastSquares a b)
  where
    -- Design Matrix A
    -- Rows are points, Columns are terms [1, x, y, x^2, xy, y^2]
    a = map (\p ->
        let x = px p
            y = py p
        -- ⚡ Bolt Optimization: Replace ^ with simple multiplication for performance
        in [1, x, y, x * x, x * y, y * y]) pts

    -- Vector b (z coordinates)
    b = map pz pts

-- Requirement FR-UI-001
