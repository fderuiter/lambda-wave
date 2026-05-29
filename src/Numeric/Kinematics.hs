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
      -- * Conversions
    , distanceToMeters
    , metersToDistance
    , timeToSeconds
    , secondsToTime
    , mmToMeters
    , metersToMm
    , nsToSeconds
    , secondsToNs
      -- * Type-level constants and assertions
    , SystemLatencyMs
    , WatchdogTimeoutMs
    , AssertWatchdogSafe
    , systemLatencyTime
    , watchdogTimeoutTime
      -- * Classes
    , KinematicMultiply(..)
    , KinematicDivide(..)
    , ScalarMultiply(..)
      -- * Re-exports
    , Proxy(..)
    ) where

import GHC.TypeLits
import Data.Proxy

-- Core Types (Requirement 1)
-- Newtypes ensure zero runtime overhead (Constraints & Guardrails)
newtype Distance = Distance Double deriving (Show, Eq, Ord)
newtype Velocity = Velocity Double deriving (Show, Eq, Ord)
newtype Acceleration = Acceleration Double deriving (Show, Eq, Ord)
newtype Time = Time Double deriving (Show, Eq, Ord)
newtype Frequency = Frequency Double deriving (Show, Eq, Ord)

-- Unit Explicit Types for Conversions
newtype Millimeters = Millimeters Double deriving (Show, Eq, Ord)
newtype Meters = Meters Double deriving (Show, Eq, Ord)
newtype Nanoseconds = Nanoseconds Double deriving (Show, Eq, Ord)
newtype Seconds = Seconds Double deriving (Show, Eq, Ord)

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

nsToSeconds :: Nanoseconds -> Seconds
nsToSeconds (Nanoseconds ns) = Seconds (ns / 1_000_000_000.0)

secondsToNs :: Seconds -> Nanoseconds
secondsToNs (Seconds s) = Nanoseconds (s * 1_000_000_000.0)

-- Basic Add/Sub for same types
instance Num Distance where
    (Distance a) + (Distance b) = Distance (a + b)
    (Distance a) - (Distance b) = Distance (a - b)
    (Distance _) * (Distance _) = error "Cannot multiply Distance by Distance"
    abs (Distance a) = Distance (abs a)
    signum (Distance a) = Distance (signum a)
    fromInteger i = Distance (fromInteger i)

instance Num Velocity where
    (Velocity a) + (Velocity b) = Velocity (a + b)
    (Velocity a) - (Velocity b) = Velocity (a - b)
    (Velocity _) * (Velocity _) = error "Cannot multiply Velocity by Velocity"
    abs (Velocity a) = Velocity (abs a)
    signum (Velocity a) = Velocity (signum a)
    fromInteger i = Velocity (fromInteger i)

instance Num Acceleration where
    (Acceleration a) + (Acceleration b) = Acceleration (a + b)
    (Acceleration a) - (Acceleration b) = Acceleration (a - b)
    (Acceleration _) * (Acceleration _) = error "Cannot multiply Acceleration by Acceleration"
    abs (Acceleration a) = Acceleration (abs a)
    signum (Acceleration a) = Acceleration (signum a)
    fromInteger i = Acceleration (fromInteger i)

instance Num Time where
    (Time a) + (Time b) = Time (a + b)
    (Time a) - (Time b) = Time (a - b)
    (Time _) * (Time _) = error "Cannot multiply Time by Time"
    abs (Time a) = Time (abs a)
    signum (Time a) = Time (signum a)
    fromInteger i = Time (fromInteger i)

instance Fractional Distance where
    (Distance _) / (Distance _) = error "Cannot divide Distance by Distance"
    fromRational r = Distance (fromRational r)

class KinematicMultiply a b c where
    (|*|) :: a -> b -> c

class KinematicDivide a b c where
    (|/|) :: a -> b -> c

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

-- Type-level assertions
type SystemLatencyMs = 50
type WatchdogTimeoutMs = 100

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

