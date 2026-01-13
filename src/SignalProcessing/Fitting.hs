module SignalProcessing.Fitting
    ( -- * 1D Regression (Legacy)
      solveBiQuadratic
    , solveStrictBiQuadratic
    , createDesignMatrix
    , createStrictBiQuadraticMatrix
    , predict
      -- * 2.5D Surface Fitting
    , fitSurfaceBiQuadratic
    , SurfaceCoefficients(..)
    ) where

import Numeric.LinearAlgebra
import Data.Types (Point3D(..))

-- | Coefficients for the Bi-Quadratic Surface:
-- Z(x,y) = c0 + c1*x + c2*y + c3*x^2 + c4*y^2 + c5*xy
-- Note: Order in vector is [c0, c1, c2, c3, c5, c4] based on typical implementation,
-- but we should standardize.
-- Standardizing to: [1, x, y, x^2, xy, y^2] -> [c0, c1, c2, c3, c5, c4]
data SurfaceCoefficients = SurfaceCoefficients
  { c0 :: Double -- ^ Intercept (Mean Height)
  , c1 :: Double -- ^ x slope
  , c2 :: Double -- ^ y slope
  , c3 :: Double -- ^ x^2 curvature
  , c4 :: Double -- ^ xy torsion
  , c5 :: Double -- ^ y^2 curvature
  } deriving (Show, Eq)

-- | Construct the Design Matrix
-- For Bi-Quadratic, we need powers: x^0, x^1, x^2, x^3, x^4
createDesignMatrix :: Vector R -> Matrix R
createDesignMatrix xVec = fromColumns [ ones
                                      , xVec
                                      , xVec ^ (2::Int)
                                      , xVec ^ (3::Int)
                                      , xVec ^ (4::Int)
                                      ]
  where
    n    = size xVec
    ones = n |> repeat 1.0 -- Creates a vector of 1s of length n

-- | Construct the Design Matrix for "Strict" Bi-Quadratic
-- This forces odd coefficients to be 0 (y = ax^4 + bx^2 + c)
createStrictBiQuadraticMatrix :: Vector R -> Matrix R
createStrictBiQuadraticMatrix xVec = fromColumns [ ones, xVec ^ (2::Int), xVec ^ (4::Int) ]
  where
    n    = size xVec
    ones = n |> repeat 1.0

-- | Perform the Regression
solveBiQuadratic :: Vector R -> Vector R -> Vector R
solveBiQuadratic x y = flatten result
  where
    designM = createDesignMatrix x
    -- linearSolveLS solves the overdetermined system A * x = B in a least-squares sense
    -- It returns the coefficients that minimize the squared error.
    result  = designM <\> asColumn y

-- | Perform the Regression for "Strict" Bi-Quadratic
solveStrictBiQuadratic :: Vector R -> Vector R -> Vector R
solveStrictBiQuadratic x y = flatten result
  where
    designM = createStrictBiQuadraticMatrix x
    result  = designM <\> asColumn y

-- | Prediction Function
predict :: Vector R -> Double -> Double
predict coeffs x =
    case toList coeffs of
        [b0, b1, b2, b3, b4] -> b0 + (b1 * x) + (b2 * x**2) + (b3 * x**3) + (b4 * x**4)
        [c0, c2, c4]         -> c0 + (c2 * x**2) + (c4 * x**4) -- Handle strict case
        _                    -> error "Invalid coefficient vector length"

-- | Fits a bi-quadratic polynomial surface to the points.
-- The patient's torso is treated as a 2.5D continuous manifold defined by:
-- Z(x,y) = c_0 + c_1x + c_2y + c_3x^2 + c_4y^2 + c_5xy
--
-- This solves the Normal Equations (A^T A) c = A^T b implicitly via QR/SVD (<\>)
-- which minimizes sum(|Ax - b|^2).
fitSurfaceBiQuadratic :: [Point3D] -> Vector R
fitSurfaceBiQuadratic pts
    | length pts < 6 = fromList (replicate 6 0.0) -- Not enough points
    | otherwise = flatten coeffs
  where
    n = length pts
    zs = map pz pts

    -- Build Design Matrix A
    -- Rows are points, Columns are terms [1, x, y, x^2, xy, y^2]
    -- Note: This aligns with c0, c1, c2, c3, c4 (xy), c5 (y^2) in the prompt's notation?
    -- Prompt says: Z(x,y) = c0 + c1x + c2y + c3x^2 + c4y^2 + c5xy
    -- But typically we order by degree. Let's stick to the prompt's implied indexing if possible,
    -- but usually it's [1, x, y, x^2, xy, y^2].
    -- Let's map columns to: [1, x, y, x^2, y^2, xy]
    -- Then output vector is [c0, c1, c2, c3, c4, c5] matching the prompt's formula:
    -- c0, c1(x), c2(y), c3(x^2), c4(y^2), c5(xy)
    rows' = map (\p -> [1, px p, py p, (px p)^(2::Int), (py p)^(2::Int), (px p)*(py p)]) pts
    a = (n><6) (concat rows')

    -- Vector b (z coordinates)
    b = (n><1) zs

    -- Solve least squares: c = (A^T A)^-1 A^T b
    coeffs = a <\> b
