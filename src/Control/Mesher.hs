{-|
Module      : Control.Mesher
Description : Surface Meshing using Pure Linear Algebra
Copyright   : (c) 2024
License     : AGPL-3.0-only
-}
module Control.Mesher (fitPolynomialSurface, reconstructPolynomialSurface) where

import Data.Types
import SignalProcessing.Matrix
import qualified Data.Vector.Unboxed as U

-- | Reconstructs a 20x20 3D surface mesh from 6 polynomial coefficients
-- Model: z = c0 + c1*x + c2*y + c3*x^2 + c4*xy + c5*y^2
reconstructPolynomialSurface :: [Double] -> [Point3D]
reconstructPolynomialSurface coeffs = case coeffs of
    [c0, c1, c2, c3, c4, c5] ->
        let pts = [ Point3D x y z 0.0 0.0
                  | xi <- [0..19 :: Int]
                  , yi <- [0..19 :: Int]
                  , let x = -100.0 + (fromIntegral xi * 200.0 / 19.0)
                  , let y = -100.0 + (fromIntegral yi * 200.0 / 19.0)
                  , let z = c0 + c1 * x + c2 * y + c3 * x * x + c4 * x * y + c5 * y * y
                  ]
        in if any (\p -> isNaN (pz p) || isInfinite (pz p)) pts
           then [] -- Safety shutdown triggered by returning empty
           else pts
    _ -> []

-- | Fits a polynomial surface to the points
-- Model: z = c0 + c1*x + c2*y + c3*x^2 + c4*xy + c5*y^2
-- Uses 'SignalProcessing.Matrix.leastSquares' for dependency-free calculation.
--
-- Complexity: O(N * M^2) where N is points, M is 6 terms.
fitPolynomialSurface :: [Point3D] -> [Double]
fitPolynomialSurface pts
    | null (drop 5 pts) = replicate 6 0.0 -- Not enough points
    | otherwise = case leastSquares a b of
        Just v -> vToList v
        Nothing -> replicate 6 0.0
  where
    a = fromLists $ map (\p ->
        let x = px p
            y = py p
        in [1, x, y, x * x, x * y, y * y]) pts

    b = Vector $ U.fromList $ map pz pts

-- Requirement FR-UI-001
