{-# LANGUAGE StrictData #-}
module SignalProcessing.Respiratory.Calibration
  ( -- * 4.0 System Calibration
    linearCalibration
  , calibrateValue
  , CalibrationParams(..)
  ) where

import Numeric.LinearAlgebra hiding (find, i)

-- | Calibration Parameters
data CalibrationParams = CalibrationParams
  { slope :: Double     -- ^ m
  , intercept :: Double -- ^ c
  } deriving (Show, Eq)

-- | 4.0 Mathematical Approach to System Calibration
-- Derives linear equation y = mx + c from datasets using Least Squares Regression.
--
-- Input:
-- x: Raw sensor values (Voltages)
-- y: True physical values (Distances)
linearCalibration :: Vector Double -- ^ X: Raw values
                  -> Vector Double -- ^ Y: True values
                  -> CalibrationParams
linearCalibration x y =
  let
    n = fromIntegral (size x)
    sumX = sumElements x
    sumY = sumElements y
    sumXY = x <.> y
    sumX2 = x <.> x

    -- Linear Least Squares formulas for slope m and intercept c:
    -- m = (N*Sum(XY) - Sum(X)*Sum(Y)) / (N*Sum(X^2) - (Sum(X))^2)
    -- c = (Sum(Y) - m*Sum(X)) / N

    numerator = n * sumXY - sumX * sumY
    denominator = n * sumX2 - sumX * sumX

    m = numerator / denominator
    c = (sumY - m * sumX) / n
  in CalibrationParams { slope = m, intercept = c }

-- | Applies the calibration equation.
-- y = mx + c
calibrateValue :: CalibrationParams -> Double -> Double
calibrateValue params rawValue =
  (slope params * rawValue) + intercept params
