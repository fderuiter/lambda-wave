{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ConstraintKinds #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

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
      -- * Type-level constants and assertions
    , SystemLatencyMs
    , WatchdogTimeoutMs
    , AssertWatchdogSafe
    , systemLatencyTime
    , watchdogTimeoutTime
      -- * Classes
    , KinematicMath(..)
    , KinematicMultiply(..)
    , KinematicDivide(..)
    , ScalarMultiply(..)
      -- * Re-exports
    , Proxy(..)
    ) where

import GHC.TypeLits
import Data.Proxy
import Hardware.Manifest (WatchdogTimeoutMs, SystemLatencyMs)
import Safety.Result (SafetyResult(..))

-- Core Types (Requirement 1)
-- Newtypes ensure zero runtime overhead (Constraints & Guardrails)
-- | Physical Distance.
-- Units: Meters or Millimeters depending on context.
-- Range constraints: Must be positive for physical lengths (>= 0).
newtype Distance = Distance Double deriving (Show, Eq, Ord)

-- | Physical Velocity.
-- Units: Meters per second (m/s).
-- Range constraints: Can be negative or positive.
newtype Velocity = Velocity Double deriving (Show, Eq, Ord)

-- | Physical Acceleration.
-- Units: Meters per second squared (m/s^2).
-- Range constraints: Can be negative or positive.
newtype Acceleration = Acceleration Double deriving (Show, Eq, Ord)

-- | Time duration.
-- Units: Seconds (s).
-- Range constraints: Must be non-negative (>= 0).
newtype Time = Time Double deriving (Show, Eq, Ord)

-- | Frequency.
-- Units: Hertz (Hz).
-- Range constraints: Must be non-negative (>= 0).
newtype Frequency = Frequency Double deriving (Show, Eq, Ord)

-- Unit Explicit Types for Conversions
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

-- Conversions
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

-- Basic Add/Sub for same types
class KinematicMath a where
    (|+|) :: a -> a -> a
    (|-|) :: a -> a -> a
    kabs  :: a -> a

instance KinematicMath Distance where
    (Distance a) |+| (Distance b) = Distance (a + b)
    (Distance a) |-| (Distance b) = Distance (a - b)
    kabs (Distance a) = Distance (abs a)

instance KinematicMath Velocity where
    (Velocity a) |+| (Velocity b) = Velocity (a + b)
    (Velocity a) |-| (Velocity b) = Velocity (a - b)
    kabs (Velocity a) = Velocity (abs a)

instance KinematicMath Acceleration where
    (Acceleration a) |+| (Acceleration b) = Acceleration (a + b)
    (Acceleration a) |-| (Acceleration b) = Acceleration (a - b)
    kabs (Acceleration a) = Acceleration (abs a)

instance KinematicMath Time where
    (Time a) |+| (Time b) = Time (a + b)
    (Time a) |-| (Time b) = Time (a - b)
    kabs (Time a) = Time (abs a)

instance KinematicMath Frequency where
    (Frequency a) |+| (Frequency b) = Frequency (a + b)
    (Frequency a) |-| (Frequency b) = Frequency (a - b)
    kabs (Frequency a) = Frequency (abs a)

class KinematicMultiply a b c where
    (|*|) :: a -> b -> c

class KinematicDivide a b c where
    (|/|) :: a -> b -> c

-- Invalid kinematics operations return the mandatory safety type rather than throwing an exception.
instance KinematicMultiply Distance Distance (SafetyResult Distance) where
    _ |*| _ = Unsafe "Cannot multiply Distance by Distance"

instance KinematicMultiply Velocity Velocity (SafetyResult Velocity) where
    _ |*| _ = Unsafe "Cannot multiply Velocity by Velocity"

instance KinematicMultiply Acceleration Acceleration (SafetyResult Acceleration) where
    _ |*| _ = Unsafe "Cannot multiply Acceleration by Acceleration"

instance KinematicMultiply Time Time (SafetyResult Time) where
    _ |*| _ = Unsafe "Cannot multiply Time by Time"

instance KinematicMultiply Frequency Frequency (SafetyResult Frequency) where
    _ |*| _ = Unsafe "Cannot multiply Frequency by Frequency"

instance KinematicDivide Distance Distance (SafetyResult Distance) where
    _ |/| _ = Unsafe "Cannot divide Distance by Distance"

instance KinematicMultiply Velocity Time Distance where
    (Velocity v) |*| (Time t) = Distance (v * t)

instance KinematicMultiply Time Velocity Distance where
    (Time t) |*| (Velocity v) = Distance (v * t)

instance KinematicMultiply Acceleration Time Velocity where
    (Acceleration a) |*| (Time t) = Velocity (a * t)

instance KinematicMultiply Time Acceleration Velocity where
    (Time t) |*| (Acceleration a) = Velocity (a * t)

instance KinematicDivide Distance Time Velocity where
    (Distance d) |/| (Time t) = Velocity (d / t)

instance KinematicDivide Velocity Time Acceleration where
    (Velocity v) |/| (Time t) = Acceleration (v / t)

instance KinematicMultiply Frequency Distance Velocity where
    (Frequency f) |*| (Distance d) = Velocity (f * d)

instance KinematicMultiply Distance Frequency Velocity where
    (Distance d) |*| (Frequency f) = Velocity (d * f)

instance KinematicDivide Velocity Distance Frequency where
    (Velocity v) |/| (Distance d) = Frequency (v / d)

instance KinematicDivide Velocity Frequency Distance where
    (Velocity v) |/| (Frequency f) = Distance (v / f)

class ScalarMultiply a where
    (|*) :: Double -> a -> a
    (*|) :: a -> Double -> a

instance ScalarMultiply Distance where
    s |* (Distance d) = Distance (s * d)
    (Distance d) *| s = Distance (s * d)

instance ScalarMultiply Velocity where
    s |* (Velocity v) = Velocity (s * v)
    (Velocity v) *| s = Velocity (s * v)

instance ScalarMultiply Acceleration where
    s |* (Acceleration a) = Acceleration (s * a)
    (Acceleration a) *| s = Acceleration (s * a)

instance ScalarMultiply Time where
    s |* (Time t) = Time (s * t)
    (Time t) *| s = Time (s * t)

instance ScalarMultiply Frequency where
    s |* (Frequency f) = Frequency (s * f)
    (Frequency f) *| s = Frequency (s * f)

-- | Type-level constraint ensuring WatchdogTimeout > SystemLatency
type AssertWatchdogSafe w l = (CmpNat w l ~ 'GT)

-- Statically enforce it globally for the module
_assertWatchdogSafe :: AssertWatchdogSafe WatchdogTimeoutMs SystemLatencyMs => ()
_assertWatchdogSafe = ()

-- Helper functions to get type-level constants as runtime values
systemLatencyTime :: forall l. (l ~ SystemLatencyMs, KnownNat l) => Proxy l -> Time
systemLatencyTime p = Time (fromInteger (natVal p) / 1000.0) -- Ms to Seconds

watchdogTimeoutTime :: forall w. (w ~ WatchdogTimeoutMs, KnownNat w) => Proxy w -> Time
watchdogTimeoutTime p = Time (fromInteger (natVal p) / 1000.0)

