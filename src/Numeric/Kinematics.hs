{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ConstraintKinds #-}

module Numeric.Kinematics
    ( -- * Core Types
      Distance(..)
    , Velocity(..)
    , Acceleration(..)
    , Time(..)
    , Frequency(..)
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
      -- * Re-exports
    , Proxy(..)
    ) where

import Data.Proxy
import Safety.Result (SafetyResult(..))

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
    (Distance a) |+| (Distance b) = let r = a + b in if r < 0 then ClampedToMin (Distance 0) else Safe (Distance r)
    (Distance a) |-| (Distance b) = let r = a - b in if r < 0 then ClampedToMin (Distance 0) else Safe (Distance r)
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
    (Time a) |+| (Time b) = let r = a + b in if r < 0 then ClampedToMin (Time 0) else Safe (Time r)
    (Time a) |-| (Time b) = let r = a - b in if r < 0 then ClampedToMin (Time 0) else Safe (Time r)
    kabs (Time a) = Time (abs a)

instance KinematicMath Frequency where
    (Frequency a) |+| (Frequency b) = let r = a + b in if r < 0 then ClampedToMin (Frequency 0) else Safe (Frequency r)
    (Frequency a) |-| (Frequency b) = let r = a - b in if r < 0 then ClampedToMin (Frequency 0) else Safe (Frequency r)
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
    (Velocity v) |*| (Time t) = let r = v * t in if r < 0 then ClampedToMin (Distance 0) else Safe (Distance r)

instance KinematicMultiply Time Velocity Distance where
    (Time t) |*| (Velocity v) = let r = v * t in if r < 0 then ClampedToMin (Distance 0) else Safe (Distance r)

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
        else let r = v / d in if r < 0 then ClampedToMin (Frequency 0) else Safe (Frequency r)

instance KinematicDivide Velocity Frequency Distance where
    (Velocity v) |/| (Frequency f) = 
        if abs f < 1e-12 
        then DivByZeroSafe (Distance 1000.0)
        else let r = v / f in if r < 0 then ClampedToMin (Distance 0) else Safe (Distance r)

class ScalarMultiply a where
    (|*) :: Double -> a -> SafetyResult a
    (*|) :: a -> Double -> SafetyResult a

instance ScalarMultiply Distance where
    s |* (Distance d) = let r = s * d in if r < 0 then ClampedToMin (Distance 0) else Safe (Distance r)
    (Distance d) *| s = let r = s * d in if r < 0 then ClampedToMin (Distance 0) else Safe (Distance r)

instance ScalarMultiply Velocity where
    s |* (Velocity v) = clampV (s * v)
    (Velocity v) *| s = clampV (s * v)

instance ScalarMultiply Acceleration where
    s |* (Acceleration a) = clampA (s * a)
    (Acceleration a) *| s = clampA (s * a)

instance ScalarMultiply Time where
    s |* (Time t) = let r = s * t in if r < 0 then ClampedToMin (Time 0) else Safe (Time r)
    (Time t) *| s = let r = s * t in if r < 0 then ClampedToMin (Time 0) else Safe (Time r)

instance ScalarMultiply Frequency where
    s |* (Frequency f) = let r = s * f in if r < 0 then ClampedToMin (Frequency 0) else Safe (Frequency r)
    (Frequency f) *| s = let r = s * f in if r < 0 then ClampedToMin (Frequency 0) else Safe (Frequency r)

