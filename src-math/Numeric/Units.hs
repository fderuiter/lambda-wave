{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE PatternSynonyms #-}

module Numeric.Units
  ( ConvertUnits (..),
    Point3DM (..),
    KalmanStateM (..),
  )
where

import Numeric.Kinematics (Millimeters (..), MillimetersPerSecond (..), MillimetersPerSecondSquared (..))
import SignalProcessing.Kalman (KalmanState (..), pattern V3)

class ConvertUnits a b | a -> b where
  convertUnits :: a -> b

instance ConvertUnits Millimeters Double where
  convertUnits (Millimeters mm) = mm / 1000.0

instance ConvertUnits MillimetersPerSecond Double where
  convertUnits (MillimetersPerSecond mms) = mms / 1000.0

instance ConvertUnits MillimetersPerSecondSquared Double where
  convertUnits (MillimetersPerSecondSquared mms2) = mms2 / 1000.0

data Point3DM = Point3DM {pxM :: Double, pyM :: Double, pzM :: Double} deriving (Show, Eq)

data KalmanStateM = KalmanStateM {posX :: Double, velX :: Double, accX :: Double} deriving (Show, Eq)

instance ConvertUnits KalmanState KalmanStateM where
  convertUnits ks =
    let (pos, vel, acc) = case x ks of
          V3 pVal vVal aVal -> (pVal, vVal, aVal)
          _ -> (0, 0, 0)
     in KalmanStateM
          (convertUnits (Millimeters pos))
          (convertUnits (MillimetersPerSecond vel))
          (convertUnits (MillimetersPerSecondSquared acc))
