module Control.Mesher (fitPolynomialSurface, generateMesh) where

import Data.Types
import Data.Config
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

-- | Generates a virtual mesh (grid of points) from the coefficients
-- over the ROI defined in Data.Config.
generateMesh :: [Double] -> [Point3D]
generateMesh coeffs
    | length coeffs /= 6 = []
    | otherwise =
        let c0 = coeffs !! 0
            c1 = coeffs !! 1
            c2 = coeffs !! 2
            c3 = coeffs !! 3
            c4 = coeffs !! 4
            c5 = coeffs !! 5
        in [ evalSurface c0 c1 c2 c3 c4 c5 x y | x <- xRange, y <- yRange ]
  where
    -- Create grid steps
    xStep = (roiMaxX - roiMinX) / fromIntegral (meshGridSize - 1)
    yStep = (roiMaxY - roiMinY) / fromIntegral (meshGridSize - 1)

    xRange = [roiMinX, roiMinX + xStep .. roiMaxX]
    yRange = [roiMinY, roiMinY + yStep .. roiMaxY]

-- | Evaluates the polynomial at a specific (x,y)
evalSurface :: Double -> Double -> Double -> Double -> Double -> Double -> Double -> Double -> Point3D
evalSurface c0 c1 c2 c3 c4 c5 x y = Point3D
    { px = x
    , py = y
    , pz = z
    , v  = 0.0
    , snr = 0.0
    }
  where
    z = c0 + c1*x + c2*y + c3*(x^(2::Int)) + c4*x*y + c5*(y^(2::Int))
