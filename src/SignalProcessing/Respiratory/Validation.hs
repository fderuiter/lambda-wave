{-# LANGUAGE StrictData #-}
module SignalProcessing.Respiratory.Validation
  ( -- * 3.0 Formulas for System Validation
    percentageError
  , rootMeanSquareError
  , pearsonCorrelation
  , diceSimilarityCoefficient
  , timeShiftError
  ) where

import Numeric.LinearAlgebra hiding (find, i)
import qualified Data.Set as Set
import Data.Set (Set)

-- | Percentage Error
-- Evaluates relative deviation of a measured value from a known reference.
-- Note: Formula uses measured value as denominator per prompt.
--
-- Formula: % Error = (X_measured - X_reference) / X_measured * 100%
percentageError :: Double -- ^ X_measured
                -> Double -- ^ X_reference
                -> Double -- ^ Returns Percentage Error
percentageError measured reference
  | measured == 0 = 0 -- Avoid division by zero
  | otherwise = (measured - reference) / measured * 100.0

-- | Root-Mean-Square Error (RMSE)
-- Quantifies the average magnitude of the error between two datasets.
--
-- Formula: RMSE = sqrt( (1/n) * Sum( (X_i - Y_i)^2 ) )
rootMeanSquareError :: Vector Double -- ^ X: First dataset (Prediction)
                    -> Vector Double -- ^ Y: Second dataset (Actual)
                    -> Double        -- ^ Returns RMSE
rootMeanSquareError x y =
  let
    diff = x - y
    squaredDiff = diff * diff
    meanSquared = sumElements squaredDiff / fromIntegral (size x)
  in sqrt meanSquared

-- | Pearson Correlation Coefficient (r)
-- Measures strength and direction of linear relationship.
--
-- Formula: r = ( n*Sum(XY) - Sum(X)*Sum(Y) ) / sqrt( [n*Sum(X^2) - (Sum(X))^2] * [n*Sum(Y^2) - (Sum(Y))^2] )
-- Using simplified vector operations:
-- r = Cov(X,Y) / (StdDev(X) * StdDev(Y))
pearsonCorrelation :: Vector Double -> Vector Double -> Double
pearsonCorrelation x y =
  let
    n = fromIntegral (size x)
    sumX = sumElements x
    sumY = sumElements y
    sumXY = x <.> y
    sumX2 = x <.> x
    sumY2 = y <.> y

    numerator = n * sumXY - sumX * sumY
    denomX = n * sumX2 - sumX * sumX
    denomY = n * sumY2 - sumY * sumY
  in numerator / sqrt (denomX * denomY)

-- | DICE Similarity Coefficient
-- Gauges spatial overlap between two segmented volumes (sets of indices or voxels).
--
-- Formula: DICE = 2 * |A intersect B| / (|A| + |B|)
-- Implementation uses Set for generic items (e.g., voxel coordinates).
diceSimilarityCoefficient :: (Ord a) => Set a -> Set a -> Double
diceSimilarityCoefficient setA setB =
  let
    intersection = Set.intersection setA setB
    sizeInt = fromIntegral (Set.size intersection)
    sizeA = fromIntegral (Set.size setA)
    sizeB = fromIntegral (Set.size setB)
  in if (sizeA + sizeB) == 0
     then 1.0 -- Empty sets are identical
     else (2.0 * sizeInt) / (sizeA + sizeB)

-- | Time Shift Error Calculation
-- Calculates sum of squared differences to optimize time shift.
--
-- Formula: Error = Sum( (Y_measured[i] - Y_reference[i])^2 )
timeShiftError :: Vector Double -> Vector Double -> Double
timeShiftError measured reference =
  let
    diff = measured - reference
  in diff <.> diff -- Dot product with self is sum of squares
