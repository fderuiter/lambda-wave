{-# LANGUAGE FlexibleContexts #-}
module SignalProcessing.Regression
    ( solveBiQuadratic
    , solveStrictBiQuadratic
    , createDesignMatrix
    , createStrictBiQuadraticMatrix
    , predict
    ) where

import Numeric.Simple

type R = Double

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
    ones = n |> 1.0

-- | Construct the Design Matrix for "Strict" Bi-Quadratic
-- This forces odd coefficients to be 0 (y = ax^4 + bx^2 + c)
createStrictBiQuadraticMatrix :: Vector R -> Matrix R
createStrictBiQuadraticMatrix xVec = fromColumns [ ones, xVec ^ (2::Int), xVec ^ (4::Int) ]
  where
    n    = size xVec
    ones = n |> 1.0

-- | Perform the Regression
-- Checks for dimension mismatch to prevent runtime exceptions.
solveBiQuadratic :: Vector R -> Vector R -> Vector R
solveBiQuadratic x y
    | size x /= size y = 5 |> 0.0 -- Return zero coefficients on mismatch
    | otherwise = flatten result
  where
    designM = createDesignMatrix x
    -- linearSolveLS solves the overdetermined system A * x = B in a least-squares sense
    result  = designM <\> asColumn y

-- | Perform the Regression for "Strict" Bi-Quadratic
solveStrictBiQuadratic :: Vector R -> Vector R -> Vector R
solveStrictBiQuadratic x y
    | size x /= size y = 3 |> 0.0 -- Return zero coefficients on mismatch
    | otherwise = flatten result
  where
    designM = createStrictBiQuadraticMatrix x
    result  = designM <\> asColumn y

-- | Prediction Function
-- Returns 0 if coefficients are invalid (Safe Fallback)
predict :: Vector R -> Double -> Double
predict coeffs x =
    case toList coeffs of
        [b0, b1, b2, b3, b4] -> b0 + (b1 * x) + (b2 * x**2) + (b3 * x**3) + (b4 * x**4)
        [c0, c2, c4]         -> c0 + (c2 * x**2) + (c4 * x**4) -- Handle strict case
        _                    -> 0.0 -- Fallback to 0 instead of crashing
