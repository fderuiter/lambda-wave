module Control.Mesher (fitPolynomialSurface) where

import Data.Types
import Numeric.LinearAlgebra

-- | Fits a polynomial surface to the points
-- z = c0 + c1*x + c2*y + c3*x^2 + c4*xy + c5*y^2
fitPolynomialSurface :: [Point3D] -> [Double]
fitPolynomialSurface pts
    | length pts < 6 = replicate 6 0.0 -- Not enough points
    | otherwise = toList (flatten coeffs)
  where
    -- Construct Vandermonde Matrix A and vector b
    -- A * x = b

    n = length pts

    -- Extract coordinates
    zs = map pz pts

    -- Build Matrix A (Design Matrix)
    -- Rows are points, Columns are terms [1, x, y, x^2, xy, y^2]
    rows' = map (\p -> [1, px p, py p, (px p)^(2::Int), (px p)*(py p), (py p)^(2::Int)]) pts
    a = (n><6) (concat rows')

    -- Vector b (z coordinates)
    b = (n><1) zs

    -- Solve least squares: x = (A^T A)^-1 A^T b
    -- hmatrix provides leastSquares
    coeffs = a <\> b
