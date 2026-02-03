module SignalProcessing.Regression
    ( solveBiQuadratic
    , solveStrictBiQuadratic
    , predict
    ) where

import Numeric.Simple

-- | Perform the Regression
solveBiQuadratic :: [Double] -> [Double] -> [Double]
solveBiQuadratic x y
    | length x /= length y = replicate 5 0.0
    | otherwise = leastSquares designM y
  where
    designM = map (\val -> [1, val, val^2, val^3, val^4]) x

-- | Perform the Regression for "Strict" Bi-Quadratic
-- y = a*x^4 + b*x^2 + c
-- Cols: [1, x^2, x^4]
solveStrictBiQuadratic :: [Double] -> [Double] -> [Double]
solveStrictBiQuadratic x y
    | length x /= length y = replicate 3 0.0
    | otherwise = leastSquares designM y
  where
    designM = map (\val -> [1, val^2, val^4]) x

-- | Prediction Function
predict :: [Double] -> Double -> Double
predict coeffs x =
    case coeffs of
        [b0, b1, b2, b3, b4] -> b0 + (b1 * x) + (b2 * x**2) + (b3 * x**3) + (b4 * x**4)
        [c0, c2, c4]         -> c0 + (c2 * x**2) + (c4 * x**4) -- Handle strict case
        _                    -> 0.0
