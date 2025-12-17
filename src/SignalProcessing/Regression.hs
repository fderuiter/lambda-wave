module SignalProcessing.Regression
    ( solveBiQuadratic
    , solveStrictBiQuadratic
    , createDesignMatrix
    , createStrictBiQuadraticMatrix
    , predict
    ) where

import Numeric.LinearAlgebra

-- | Construct the Design Matrix
-- For Bi-Quadratic, we need powers: x^0, x^1, x^2, x^3, x^4
createDesignMatrix :: Vector R -> Matrix R
createDesignMatrix xVec = fromColumns [ ones
                                      , xVec
                                      , xVec ^ 2
                                      , xVec ^ 3
                                      , xVec ^ 4
                                      ]
  where
    n    = size xVec
    ones = n |> repeat 1.0 -- Creates a vector of 1s of length n

-- | Construct the Design Matrix for "Strict" Bi-Quadratic
-- This forces odd coefficients to be 0 (y = ax^4 + bx^2 + c)
createStrictBiQuadraticMatrix :: Vector R -> Matrix R
createStrictBiQuadraticMatrix xVec = fromColumns [ ones, xVec ^ 2, xVec ^ 4 ]
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
