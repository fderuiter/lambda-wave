module SignalProcessing.Regression
    ( solveBiQuadratic
    , solveStrictBiQuadratic
    , createDesignMatrix
    , createStrictBiQuadraticMatrix
    , predict
    ) where

import SignalProcessing.SimpleLA (leastSquares, Matrix, Vector)
import Data.List (transpose)

-- | Construct the Design Matrix
-- For Bi-Quadratic, we need powers: x^0, x^1, x^2, x^3, x^4
createDesignMatrix :: Vector -> Matrix
createDesignMatrix xVec = transpose [ ones
                                    , xVec
                                    , map (^ (2::Int)) xVec
                                    , map (^ (3::Int)) xVec
                                    , map (^ (4::Int)) xVec
                                    ]
  where
    n    = length xVec
    ones = replicate n 1.0

-- | Construct the Design Matrix for "Strict" Bi-Quadratic
-- This forces odd coefficients to be 0 (y = ax^4 + bx^2 + c)
createStrictBiQuadraticMatrix :: Vector -> Matrix
createStrictBiQuadraticMatrix xVec = transpose [ ones
                                               , map (^ (2::Int)) xVec
                                               , map (^ (4::Int)) xVec
                                               ]
  where
    n    = length xVec
    ones = replicate n 1.0

-- | Perform the Regression
-- Checks for dimension mismatch to prevent runtime exceptions.
solveBiQuadratic :: Vector -> Vector -> Vector
solveBiQuadratic x y
    | length x /= length y = replicate 5 0.0 -- Return zero coefficients on mismatch
    | otherwise = leastSquares (createDesignMatrix x) y

-- | Perform the Regression for "Strict" Bi-Quadratic
solveStrictBiQuadratic :: Vector -> Vector -> Vector
solveStrictBiQuadratic x y
    | length x /= length y = replicate 3 0.0 -- Return zero coefficients on mismatch
    | otherwise = leastSquares (createStrictBiQuadraticMatrix x) y

-- | Prediction Function
-- Returns 0 if coefficients are invalid (Safe Fallback)
predict :: Vector -> Double -> Double
predict coeffs x =
    case coeffs of
        [b0, b1, b2, b3, b4] -> b0 + (b1 * x) + (b2 * x**2) + (b3 * x**3) + (b4 * x**4)
        [c0, c2, c4]         -> c0 + (c2 * x**2) + (c4 * x**4) -- Handle strict case
        _                    -> 0.0 -- Fallback to 0 instead of crashing
