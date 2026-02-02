module SignalProcessing.Regression
    ( solveBiQuadratic
    , solveStrictBiQuadratic
    , predict
    ) where

import Numeric.Simple (solveLS)
import Data.Maybe (fromMaybe)

-- | Perform the Regression
-- Checks for dimension mismatch to prevent runtime exceptions.
solveBiQuadratic :: [Double] -> [Double] -> [Double]
solveBiQuadratic x y
    | length x /= length y = replicate 5 0.0 -- Return zero coefficients on mismatch
    | otherwise = fromMaybe (replicate 5 0.0) $ do
        -- Design Matrix: [1, x, x^2, x^3, x^4]
        let designM = [ [1, val, val^(2::Int), val^(3::Int), val^(4::Int)] | val <- x ]
        solveLS designM y

-- | Perform the Regression for "Strict" Bi-Quadratic
solveStrictBiQuadratic :: [Double] -> [Double] -> [Double]
solveStrictBiQuadratic x y
    | length x /= length y = replicate 3 0.0 -- Return zero coefficients on mismatch
    | otherwise = fromMaybe (replicate 3 0.0) $ do
        -- Design Matrix: [1, x^2, x^4]
        let designM = [ [1, val^(2::Int), val^(4::Int)] | val <- x ]
        solveLS designM y

-- | Prediction Function
-- Returns 0 if coefficients are invalid (Safe Fallback)
predict :: [Double] -> Double -> Double
predict coeffs x =
    case coeffs of
        [b0, b1, b2, b3, b4] -> b0 + (b1 * x) + (b2 * x**(2::Double)) + (b3 * x**(3::Double)) + (b4 * x**(4::Double))
        [c0, c2, c4]         -> c0 + (c2 * x**(2::Double)) + (c4 * x**(4::Double)) -- Handle strict case
        _                    -> 0.0 -- Fallback to 0 instead of crashing
