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

-- Core Types
newtype Distance = Distance Double deriving (Show, Eq, Ord)
newtype Velocity = Velocity Double deriving (Show, Eq, Ord)
newtype Acceleration = Acceleration Double deriving (Show, Eq, Ord)
newtype Time = Time Double deriving (Show, Eq, Ord)
newtype Frequency = Frequency Double deriving (Show, Eq, Ord)

newtype Millimeters = Millimeters Double deriving (Show, Eq, Ord)
newtype Meters = Meters Double deriving (Show, Eq, Ord)
newtype Nanoseconds = Nanoseconds Double deriving (Show, Eq, Ord)
newtype Seconds = Seconds Double deriving (Show, Eq, Ord)

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

class KinematicMath a where
    (|+|) :: a -> a -> a
    (|-|) :: a -> a -> a
    kAbs :: a -> a

instance KinematicMath Distance where
    (Distance a) |+| (Distance b) = Distance (a + b)
    (Distance a) |-| (Distance b) = Distance (a - b)
    kAbs (Distance a) = Distance (abs a)

instance KinematicMath Velocity where
    (Velocity a) |+| (Velocity b) = Velocity (a + b)
    (Velocity a) |-| (Velocity b) = Velocity (a - b)
    kAbs (Velocity a) = Velocity (abs a)

instance KinematicMath Acceleration where
    (Acceleration a) |+| (Acceleration b) = Acceleration (a + b)
    (Acceleration a) |-| (Acceleration b) = Acceleration (a - b)
    kAbs (Acceleration a) = Acceleration (abs a)

instance KinematicMath Time where
    (Time a) |+| (Time b) = Time (a + b)
    (Time a) |-| (Time b) = Time (a - b)
    kAbs (Time a) = Time (abs a)

class KinematicMultiply a b c where
    (|*|) :: a -> b -> SafetyResult c

class KinematicDivide a b c where
    (|/|) :: a -> b -> SafetyResult c

instance KinematicMultiply Velocity Time Distance where
    (Velocity v) |*| (Time t) = Safe (Distance (v * t))
instance KinematicMultiply Time Velocity Distance where
    (Time t) |*| (Velocity v) = Safe (Distance (v * t))
instance KinematicMultiply Acceleration Time Velocity where
    (Acceleration a) |*| (Time t) = Safe (Velocity (a * t))
instance KinematicMultiply Time Acceleration Velocity where
    (Time t) |*| (Acceleration a) = Safe (Velocity (a * t))

instance KinematicDivide Distance Time Velocity where
    (Distance d) |/| (Time t) = Safe (Velocity (d / t))
instance KinematicDivide Velocity Time Acceleration where
    (Velocity v) |/| (Time t) = Safe (Acceleration (v / t))

instance KinematicMultiply Distance Distance Distance where
    _ |*| _ = Fault "Cannot multiply Distance by Distance"
instance KinematicMultiply Velocity Velocity Velocity where
    _ |*| _ = Fault "Cannot multiply Velocity by Velocity"
instance KinematicMultiply Acceleration Acceleration Acceleration where
    _ |*| _ = Fault "Cannot multiply Acceleration by Acceleration"
instance KinematicMultiply Time Time Time where
    _ |*| _ = Fault "Cannot multiply Time by Time"

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

type AssertWatchdogSafe w l = (CmpNat w l ~ 'GT)

_assertWatchdogSafe :: AssertWatchdogSafe WatchdogTimeoutMs SystemLatencyMs => ()
_assertWatchdogSafe = ()

systemLatencyTime :: forall l. (l ~ SystemLatencyMs, KnownNat l) => Proxy l -> Time
systemLatencyTime p = Time (fromInteger (natVal p) / 1000.0)

watchdogTimeoutTime :: forall w. (w ~ WatchdogTimeoutMs, KnownNat w) => Proxy w -> Time
watchdogTimeoutTime p = Time (fromInteger (natVal p) / 1000.0)
