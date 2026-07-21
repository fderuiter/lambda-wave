{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PatternSynonyms #-}

module Numeric.Units
    ( ConvertUnits(..)
    ) where

import Data.Types (Point3D(..))
import SignalProcessing.Kalman (KalmanState(..), pattern V3)
import Numeric.Kinematics (Millimeters(..), MillimetersPerSecond(..), MillimetersPerSecondSquared(..))

class ConvertUnits a b | a -> b where
    convertUnits :: a -> b

instance ConvertUnits Millimeters Double where
    convertUnits (Millimeters mm) = mm / 1000.0

instance ConvertUnits MillimetersPerSecond Double where
    convertUnits (MillimetersPerSecond mms) = mms / 1000.0

instance ConvertUnits MillimetersPerSecondSquared Double where
    convertUnits (MillimetersPerSecondSquared mms2) = mms2 / 1000.0

instance ConvertUnits Point3D Point3D where
    convertUnits pt = pt
        { px = convertUnits (Millimeters (px pt))
        , py = convertUnits (Millimeters (py pt))
        , pz = convertUnits (Millimeters (pz pt))
        }

instance ConvertUnits KalmanState KalmanState where
    convertUnits ks =
        let (pos, vel, acc) = case x ks of
                V3 pVal vVal aVal -> (pVal, vVal, aVal)
                _ -> (0, 0, 0)
        in ks { x = V3 (convertUnits (Millimeters pos))
                       (convertUnits (MillimetersPerSecond vel))
                       (convertUnits (MillimetersPerSecondSquared acc)) }
