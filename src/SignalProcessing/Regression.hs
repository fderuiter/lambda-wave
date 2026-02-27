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
    | length x /= length y || null x = Nothing
    | otherwise = do
        coeffs <- leastSquares designM y
        case coeffs of
            [p0, p1, p2, p3, p4] -> Just $ BiQuadratic p0 p1 p2 p3 p4
            _ -> Nothing
  where
    designM = map (\val -> [1, val, val^(2::Int), val^(3::Int), val^(4::Int)]) x

-- | Perform the Regression for "Strict" Bi-Quadratic
-- y = a*x^4 + b*x^2 + c
-- Cols: [1, x^2, x^4]
solveStrictBiQuadratic :: [Double] -> [Double] -> Maybe StrictBiQuadratic
solveStrictBiQuadratic x y
    | length x /= length y || null x = Nothing
    | otherwise = do
        coeffs <- leastSquares designM y
        case coeffs of
            [k0, k2, k4] -> Just $ StrictBiQuadratic k0 k2 k4
            _ -> Nothing
  where
    designM = map (\val -> [1, val^(2::Int), val^(4::Int)]) x

-- | Prediction Function via TypeClass or Overloading would be nice,
-- but for simplicity/safety we can just have specific functions or a Sum Type.
class Predictable a where
    predict :: a -> Double -> Double

instance Predictable BiQuadratic where
    predict (BiQuadratic p0 p1 p2 p3 p4) x =
        p0 + (p1 * x) + (p2 * x^(2::Int)) + (p3 * x^(3::Int)) + (p4 * x^(4::Int))

instance Predictable StrictBiQuadratic where
    predict (StrictBiQuadratic k0 k2 k4) x =
        k0 + (k2 * x^(2::Int)) + (k4 * x^(4::Int))
