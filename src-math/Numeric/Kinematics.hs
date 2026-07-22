{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE PatternSynonyms #-}

-- | Numeric.Kinematics
-- Failure Modes: Incorrect dimension conversion.
-- Mitigations: Core types strictly define domain bounds.
-- Traceability: REQ-SYS-003
module Numeric.Kinematics
    ( -- * Core Types
      Distance(..)
    , Velocity(..)
    , Acceleration(..)
    , Time(..)
    , Frequency(..)
    , Coordinate(..)
      -- * Unit Specific Types
    , Millimeters(..)
    , Meters(..)
    , Nanoseconds(..)
    , Seconds(..)
    , Milliseconds(..)
    , Hertz(..)
    , Gigahertz(..)
    , MillimetersPerSecond(..)
    , MetersPerSecond(..)
    , MillimetersPerSecondSquared(..)
    , MetersPerSecondSquared(..)
      -- * Conversions
    , distanceToMeters
    , metersToDistance
    , timeToSeconds
    , secondsToTime
    , mmToMeters
    , metersToMm
    , mmPerSToMetersPerS
    , mmPerS2ToMetersPerS2
    , nsToSeconds
    , secondsToNs
    , nsToMs
    , msToNs
    , msToSeconds
    , secondsToMs
    , hzToFrequency
    , frequencyToHz
    , ghzToHz
      -- * Classes
    , KinematicMath(..)
    , KinematicMultiply(..)
    , KinematicDivide(..)
    , ScalarMultiply(..)
      -- * Safe Bounds
    , ClinicalBounds(..)
    , defaultBounds
      -- * Spatial Functions
    , pattern Vector3D
    , translate
    , distance
    , magnitude
    , normalize
    , dot
    , sub
    , angleBetween
      -- * Re-exports
    , Proxy(..)
    ) where

import Data.Proxy
import Safety.Result (SafetyResult(..))

data Coordinate = Coordinate
  { coordX :: Double
  , coordY :: Double
  , coordZ :: Double
  , coordIntensity :: Double
  , coordConfidence :: Double
  } deriving (Show, Eq)

pattern Vector3D :: Double -> Double -> Double -> Coordinate
pattern Vector3D x y z <- Coordinate x y z _ _
  where Vector3D x y z = Coordinate x y z 0.0 0.0
{-# COMPLETE Vector3D :: Coordinate #-}

translate :: Coordinate -> Coordinate -> Coordinate
translate (Coordinate x1 y1 z1 i1 c1) (Coordinate x2 y2 z2 _ _) =
    Coordinate (x1 + x2) (y1 + y2) (z1 + z2) i1 c1

distance :: Coordinate -> Coordinate -> Double
distance (Coordinate x1 y1 z1 _ _) (Coordinate x2 y2 z2 _ _) =
    sqrt ((x1 - x2)**2 + (y1 - y2)**2 + (z1 - z2)**2)

magnitude :: Coordinate -> Double
magnitude (Coordinate x y z _ _) = sqrt (x*x + y*y + z*z)

normalize :: Coordinate -> Coordinate
normalize v@(Coordinate x y z _ _) =
    let m = magnitude v
    in if m == 0 then Coordinate 0 0 0 0.0 0.0 else Coordinate (x/m) (y/m) (z/m) 0.0 0.0

dot :: Coordinate -> Coordinate -> Double
dot (Coordinate x1 y1 z1 _ _) (Coordinate x2 y2 z2 _ _) = x1*x2 + y1*y2 + z1*z2

sub :: Coordinate -> Coordinate -> Coordinate
sub (Coordinate x1 y1 z1 _ _) (Coordinate x2 y2 z2 _ _) = Coordinate (x1-x2) (y1-y2) (z1-z2) 0.0 0.0

angleBetween :: Coordinate -> Coordinate -> Double
angleBetween v1 v2 =
    let (Coordinate n1x n1y n1z _ _) = normalize v1
        (Coordinate n2x n2y n2z _ _) = normalize v2
        d = n1x*n2x + n1y*n2y + n1z*n2z
        d' = max (-1.0) (min 1.0 d)
    in (acos d') * 180.0 / pi

newtype Distance = Distance Double deriving (Show, Eq, Ord)
newtype Velocity = Velocity Double deriving (Show, Eq, Ord)
newtype Acceleration = Acceleration Double deriving (Show, Eq, Ord)
newtype Time = Time Double deriving (Show, Eq, Ord)
newtype Frequency = Frequency Double deriving (Show, Eq, Ord)

newtype Millimeters = Millimeters Double deriving (Show, Eq, Ord)
newtype Meters = Meters Double deriving (Show, Eq, Ord)
newtype Nanoseconds = Nanoseconds Double deriving (Show, Eq, Ord)
newtype Seconds = Seconds Double deriving (Show, Eq, Ord)
newtype Milliseconds = Milliseconds Double deriving (Show, Eq, Ord)
newtype Hertz = Hertz Double deriving (Show, Eq, Ord)
newtype Gigahertz = Gigahertz Double deriving (Show, Eq, Ord)
newtype MillimetersPerSecond = MillimetersPerSecond Double deriving (Show, Eq, Ord)
newtype MetersPerSecond = MetersPerSecond Double deriving (Show, Eq, Ord)
newtype MillimetersPerSecondSquared = MillimetersPerSecondSquared Double deriving (Show, Eq, Ord)
newtype MetersPerSecondSquared = MetersPerSecondSquared Double deriving (Show, Eq, Ord)

distanceToMeters :: Distance -> Meters
distanceToMeters (Distance d) = Meters d
metersToDistance :: Meters -> Distance
metersToDistance (Meters m) = Distance m
timeToSeconds :: Time -> Seconds
timeToSeconds (Time t) = Seconds t
secondsToTime :: Seconds -> Time
secondsToTime (Seconds s) = Time s
mmToMeters :: Millimeters -> Meters
mmToMeters (Millimeters mm) = Meters (mm / 1000.0)
metersToMm :: Meters -> Millimeters
metersToMm (Meters m) = Millimeters (m * 1000.0)
mmPerSToMetersPerS :: MillimetersPerSecond -> MetersPerSecond
mmPerSToMetersPerS (MillimetersPerSecond mm) = MetersPerSecond (mm / 1000.0)
mmPerS2ToMetersPerS2 :: MillimetersPerSecondSquared -> MetersPerSecondSquared
mmPerS2ToMetersPerS2 (MillimetersPerSecondSquared mm) = MetersPerSecondSquared (mm / 1000.0)
nsToSeconds :: Nanoseconds -> Seconds
nsToSeconds (Nanoseconds ns) = Seconds (ns / 1_000_000_000.0)
secondsToNs :: Seconds -> Nanoseconds
secondsToNs (Seconds s) = Nanoseconds (s * 1_000_000_000.0)
nsToMs :: Nanoseconds -> Milliseconds
nsToMs (Nanoseconds ns) = Milliseconds (ns / 1_000_000.0)
msToNs :: Milliseconds -> Nanoseconds
msToNs (Milliseconds ms) = Nanoseconds (ms * 1_000_000.0)
msToSeconds :: Milliseconds -> Seconds
msToSeconds (Milliseconds ms) = Seconds (ms / 1000.0)
secondsToMs :: Seconds -> Milliseconds
secondsToMs (Seconds s) = Milliseconds (s * 1000.0)
hzToFrequency :: Hertz -> Frequency
hzToFrequency (Hertz hz) = Frequency hz
frequencyToHz :: Frequency -> Hertz
frequencyToHz (Frequency f) = Hertz f
ghzToHz :: Gigahertz -> Hertz
ghzToHz (Gigahertz ghz) = Hertz (ghz * 1_000_000_000.0)

data ClinicalBounds = ClinicalBounds
  { minVelocity     :: !Double
  , maxVelocity     :: !Double
  , minAcceleration :: !Double
  , maxAcceleration :: !Double
  } deriving (Show, Eq)

defaultBounds :: ClinicalBounds
defaultBounds = ClinicalBounds
  { minVelocity     = 0.01
  , maxVelocity     = 0.1
  , minAcceleration = 0.01
  , maxAcceleration = 0.1
  }

clampNonNegative :: (Double -> a) -> Double -> SafetyResult a
clampNonNegative con r = if r < 0 then ClampedToMin (con 0) else Safe (con r)

clampV :: Double -> SafetyResult Velocity
clampV v
    | abs v > maxVelocity defaultBounds = ClampedToMax (Velocity (signum v * maxVelocity defaultBounds))
    | abs v < minVelocity defaultBounds = ClampedToMin (Velocity (signum v * minVelocity defaultBounds))
    | otherwise = Safe (Velocity v)

clampA :: Double -> SafetyResult Acceleration
clampA a
    | abs a > maxAcceleration defaultBounds = ClampedToMax (Acceleration (signum a * maxAcceleration defaultBounds))
    | abs a < minAcceleration defaultBounds = ClampedToMin (Acceleration (signum a * minAcceleration defaultBounds))
    | otherwise = Safe (Acceleration a)


class KinematicMath a where
    (|+|) :: a -> a -> SafetyResult a
    (|-|) :: a -> a -> SafetyResult a
    kabs  :: a -> a

instance KinematicMath Distance where
    (Distance a) |+| (Distance b) = clampNonNegative Distance (a + b)
    (Distance a) |-| (Distance b) = clampNonNegative Distance (a - b)
    kabs (Distance a) = Distance (abs a)

instance KinematicMath Velocity where
    (Velocity a) |+| (Velocity b) = clampV (a + b)
    (Velocity a) |-| (Velocity b) = clampV (a - b)
    kabs (Velocity a) = Velocity (abs a)

instance KinematicMath Acceleration where
    (Acceleration a) |+| (Acceleration b) = clampA (a + b)
    (Acceleration a) |-| (Acceleration b) = clampA (a - b)
    kabs (Acceleration a) = Acceleration (abs a)

instance KinematicMath Time where
    (Time a) |+| (Time b) = clampNonNegative Time (a + b)
    (Time a) |-| (Time b) = clampNonNegative Time (a - b)
    kabs (Time a) = Time (abs a)

instance KinematicMath Frequency where
    (Frequency a) |+| (Frequency b) = clampNonNegative Frequency (a + b)
    (Frequency a) |-| (Frequency b) = clampNonNegative Frequency (a - b)
    kabs (Frequency a) = Frequency (abs a)

class KinematicMultiply a b c where
    (|*|) :: a -> b -> SafetyResult c

class KinematicDivide a b c where
    (|/|) :: a -> b -> SafetyResult c

-- Invalid kinematics operations
instance KinematicMultiply Distance Distance Distance where
    _ |*| _ = Unsafe "Cannot multiply Distance by Distance"
instance KinematicMultiply Velocity Velocity Velocity where
    _ |*| _ = Unsafe "Cannot multiply Velocity by Velocity"
instance KinematicMultiply Acceleration Acceleration Acceleration where
    _ |*| _ = Unsafe "Cannot multiply Acceleration by Acceleration"
instance KinematicMultiply Time Time Time where
    _ |*| _ = Unsafe "Cannot multiply Time by Time"
instance KinematicMultiply Frequency Frequency Frequency where
    _ |*| _ = Unsafe "Cannot multiply Frequency by Frequency"
instance KinematicDivide Distance Distance Distance where
    _ |/| _ = Unsafe "Cannot divide Distance by Distance"

instance KinematicMultiply Velocity Time Distance where
    (Velocity v) |*| (Time t) = clampNonNegative Distance (v * t)

instance KinematicMultiply Time Velocity Distance where
    (Time t) |*| (Velocity v) = clampNonNegative Distance (v * t)

instance KinematicMultiply Acceleration Time Velocity where
    (Acceleration a) |*| (Time t) = clampV (a * t)

instance KinematicMultiply Time Acceleration Velocity where
    (Time t) |*| (Acceleration a) = clampV (a * t)

instance KinematicDivide Distance Time Velocity where
    (Distance d) |/| (Time t) = 
        if abs t < 1e-12 
        then DivByZeroSafe (Velocity (maxVelocity defaultBounds)) 
        else clampV (d / t)

instance KinematicDivide Velocity Time Acceleration where
    (Velocity v) |/| (Time t) = 
        if abs t < 1e-12 
        then DivByZeroSafe (Acceleration (maxAcceleration defaultBounds)) 
        else clampA (v / t)

instance KinematicMultiply Frequency Distance Velocity where
    (Frequency f) |*| (Distance d) = clampV (f * d)

instance KinematicMultiply Distance Frequency Velocity where
    (Distance d) |*| (Frequency f) = clampV (d * f)

instance KinematicDivide Velocity Distance Frequency where
    (Velocity v) |/| (Distance d) = 
        if abs d < 1e-12 
        then DivByZeroSafe (Frequency 1000.0)
        else clampNonNegative Frequency (v / d)

instance KinematicDivide Velocity Frequency Distance where
    (Velocity v) |/| (Frequency f) = 
        if abs f < 1e-12 
        then DivByZeroSafe (Distance 1000.0)
        else clampNonNegative Distance (v / f)

class ScalarMultiply a where
    (|*) :: Double -> a -> SafetyResult a
    (*|) :: a -> Double -> SafetyResult a

instance ScalarMultiply Distance where
    s |* (Distance d) = clampNonNegative Distance (s * d)
    (Distance d) *| s = clampNonNegative Distance (s * d)

instance ScalarMultiply Velocity where
    s |* (Velocity v) = clampV (s * v)
    (Velocity v) *| s = clampV (s * v)

instance ScalarMultiply Acceleration where
    s |* (Acceleration a) = clampA (s * a)
    (Acceleration a) *| s = clampA (s * a)

instance ScalarMultiply Time where
    s |* (Time t) = clampNonNegative Time (s * t)
    (Time t) *| s = clampNonNegative Time (s * t)

instance ScalarMultiply Frequency where
    s |* (Frequency f) = clampNonNegative Frequency (s * f)
    (Frequency f) *| s = clampNonNegative Frequency (s * f)

