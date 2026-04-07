{-# LANGUAGE StrictData #-}
module SignalProcessing.Regression
    ( solveBiQuadratic
    , solveStrictBiQuadratic
    , predict
    , BiQuadratic(..)
    , StrictBiQuadratic(..)
    ) where

import Numeric.Simple

-- | Standard Bi-Quadratic Polynomial
-- y = b0 + b1*x + b2*x^2 + b3*x^3 + b4*x^4
data BiQuadratic = BiQuadratic
    { b0 :: Double
    , b1 :: Double
    , b2 :: Double
    , b3 :: Double
    , b4 :: Double
    } deriving (Show, Eq)

-- | Strict Bi-Quadratic Polynomial (Even powers only)
-- y = c0 + c2*x^2 + c4*x^4
data StrictBiQuadratic = StrictBiQuadratic
    { c0 :: Double
    , c2 :: Double
    , c4 :: Double
    } deriving (Show, Eq)

-- | Perform the Regression
solveBiQuadratic :: [Double] -> [Double] -> Maybe BiQuadratic
solveBiQuadratic x y
    | null x = Nothing
    -- ⚡ Bolt Optimization: Removed redundant O(N) `length x /= length y` check
    -- `leastSquares` inherently validates dimension equality.
    | otherwise = do
        coeffs <- leastSquares designM y
        case coeffs of
            [p0, p1, p2, p3, p4] -> Just $ BiQuadratic p0 p1 p2 p3 p4
            _ -> Nothing
  where
    -- ⚡ Bolt: Using explicit multiplication (v2 * v2) instead of (val^4)
    -- avoids the function overhead of `^` for small integer powers.
    designM = map (\val -> let v2 = val * val in [1, val, v2, v2 * val, v2 * v2]) x

-- | Perform the Regression for "Strict" Bi-Quadratic
-- y = a*x^4 + b*x^2 + c
-- Cols: [1, x^2, x^4]
solveStrictBiQuadratic :: [Double] -> [Double] -> Maybe StrictBiQuadratic
solveStrictBiQuadratic x y
    | null x = Nothing
    -- ⚡ Bolt Optimization: Removed redundant O(N) `length x /= length y` check
    -- `leastSquares` inherently validates dimension equality.
    | otherwise = do
        coeffs <- leastSquares designM y
        case coeffs of
            [k0, k2, k4] -> Just $ StrictBiQuadratic k0 k2 k4
            _ -> Nothing
  where
    -- ⚡ Bolt: Using explicit multiplication instead of ^
    designM = map (\val -> let v2 = val * val in [1, v2, v2 * v2]) x

-- | Prediction Function via TypeClass or Overloading would be nice,
-- but for simplicity/safety we can just have specific functions or a Sum Type.
class Predictable a where
    predict :: a -> Double -> Double

instance Predictable BiQuadratic where
    predict (BiQuadratic p0 p1 p2 p3 p4) x =
        -- ⚡ Bolt: Caching x^2 and using explicit multiplication for x^3 and x^4
        let x2 = x * x
        in p0 + (p1 * x) + (p2 * x2) + (p3 * x2 * x) + (p4 * x2 * x2)

instance Predictable StrictBiQuadratic where
    predict (StrictBiQuadratic k0 k2 k4) x =
        -- ⚡ Bolt: Caching x^2 and using explicit multiplication
        let x2 = x * x
        in k0 + (k2 * x2) + (k4 * x2 * x2)
