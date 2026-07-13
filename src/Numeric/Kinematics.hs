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
    ( -- * Core Types (Deprecated/Removed, replaced by Unit Specific)
      -- * Unit Specific Types
      Millimeters(..)
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
    , mmToMeters
    , metersToMm
    , mmPerSToMetersPerS
    , mmPerS2ToMetersPerS2
    , metersPerSToMmPerS
    , metersPerS2ToMmPerS2
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
      -- * Safe Bounds
    , ClinicalBounds(..)
    , defaultBounds
      -- * Re-exports
    , Proxy(..)
    ) where

import GHC.TypeLits
import Data.Proxy
import Hardware.Manifest (WatchdogTimeoutMs, SystemLatencyMs, minVelocityMs, maxVelocityMs, minAccelerationMs2, maxAccelerationMs2)
import Safety.Result (SafetyResult(..))

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

mmToMeters :: Millimeters -> Meters
mmToMeters (Millimeters mm) = Meters (mm / 1000.0)
metersToMm :: Meters -> Millimeters
metersToMm (Meters m) = Millimeters (m * 1000.0)
mmPerSToMetersPerS :: MillimetersPerSecond -> MetersPerSecond
mmPerSToMetersPerS (MillimetersPerSecond mm) = MetersPerSecond (mm / 1000.0)
mmPerS2ToMetersPerS2 :: MillimetersPerSecondSquared -> MetersPerSecondSquared
mmPerS2ToMetersPerS2 (MillimetersPerSecondSquared mm) = MetersPerSecondSquared (mm / 1000.0)

metersPerSToMmPerS :: MetersPerSecond -> MillimetersPerSecond
metersPerSToMmPerS (MetersPerSecond m) = MillimetersPerSecond (m * 1000.0)
metersPerS2ToMmPerS2 :: MetersPerSecondSquared -> MillimetersPerSecondSquared
metersPerS2ToMmPerS2 (MetersPerSecondSquared m) = MillimetersPerSecondSquared (m * 1000.0)


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

hzToFrequency :: Hertz -> Hertz
hzToFrequency = id
frequencyToHz :: Hertz -> Hertz
frequencyToHz = id
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
  { minVelocity     = minVelocityMs
  , maxVelocity     = maxVelocityMs
  , minAcceleration = minAccelerationMs2
  , maxAcceleration = maxAccelerationMs2
  }

clampV :: Double -> SafetyResult MetersPerSecond
clampV v
    | abs v > maxVelocity defaultBounds = ClampedToMax (MetersPerSecond (signum v * maxVelocity defaultBounds))
    | v < minVelocity defaultBounds && v >= 0 = ClampedToMin (MetersPerSecond (minVelocity defaultBounds))
    | v > -minVelocity defaultBounds && v < 0 = ClampedToMin (MetersPerSecond (-minVelocity defaultBounds))
    | v < -maxVelocity defaultBounds = ClampedToMax (MetersPerSecond (-maxVelocity defaultBounds))
    | otherwise = Safe (MetersPerSecond v)

clampA :: Double -> SafetyResult MetersPerSecondSquared
clampA a
    | abs a > maxAcceleration defaultBounds = ClampedToMax (MetersPerSecondSquared (signum a * maxAcceleration defaultBounds))
    | a < minAcceleration defaultBounds && a >= 0 = ClampedToMin (MetersPerSecondSquared (minAcceleration defaultBounds))
    | a > -minAcceleration defaultBounds && a < 0 = ClampedToMin (MetersPerSecondSquared (-minAcceleration defaultBounds))
    | a < -maxAcceleration defaultBounds = ClampedToMax (MetersPerSecondSquared (-maxAcceleration defaultBounds))
    | otherwise = Safe (MetersPerSecondSquared a)

clampVMm :: Double -> SafetyResult MillimetersPerSecond
clampVMm v = 
    let resMeters = clampV (v / 1000.0)
    in case resMeters of
        Safe (MetersPerSecond m) -> Safe (MillimetersPerSecond (m * 1000.0))
        ClampedToMin (MetersPerSecond m) -> ClampedToMin (MillimetersPerSecond (m * 1000.0))
        ClampedToMax (MetersPerSecond m) -> ClampedToMax (MillimetersPerSecond (m * 1000.0))
        _ -> Unsafe "Invalid clamping"

clampAMm :: Double -> SafetyResult MillimetersPerSecondSquared
clampAMm a = 
    let resMeters = clampA (a / 1000.0)
    in case resMeters of
        Safe (MetersPerSecondSquared m) -> Safe (MillimetersPerSecondSquared (m * 1000.0))
        ClampedToMin (MetersPerSecondSquared m) -> ClampedToMin (MillimetersPerSecondSquared (m * 1000.0))
        ClampedToMax (MetersPerSecondSquared m) -> ClampedToMax (MillimetersPerSecondSquared (m * 1000.0))
        _ -> Unsafe "Invalid clamping"

class KinematicMath a where
    (|+|) :: a -> a -> SafetyResult a
    (|-|) :: a -> a -> SafetyResult a
    kabs  :: a -> a

instance KinematicMath Meters where
    (Meters a) |+| (Meters b) = let r = a + b in if r < 0 then ClampedToMin (Meters 0) else Safe (Meters r)
    (Meters a) |-| (Meters b) = let r = a - b in if r < 0 then ClampedToMin (Meters 0) else Safe (Meters r)
    kabs (Meters a) = Meters (abs a)

instance KinematicMath Millimeters where
    (Millimeters a) |+| (Millimeters b) = let r = a + b in if r < 0 then ClampedToMin (Millimeters 0) else Safe (Millimeters r)
    (Millimeters a) |-| (Millimeters b) = let r = a - b in if r < 0 then ClampedToMin (Millimeters 0) else Safe (Millimeters r)
    kabs (Millimeters a) = Millimeters (abs a)

instance KinematicMath MetersPerSecond where
    (MetersPerSecond a) |+| (MetersPerSecond b) = clampV (a + b)
    (MetersPerSecond a) |-| (MetersPerSecond b) = clampV (a - b)
    kabs (MetersPerSecond a) = MetersPerSecond (abs a)

instance KinematicMath MillimetersPerSecond where
    (MillimetersPerSecond a) |+| (MillimetersPerSecond b) = clampVMm (a + b)
    (MillimetersPerSecond a) |-| (MillimetersPerSecond b) = clampVMm (a - b)
    kabs (MillimetersPerSecond a) = MillimetersPerSecond (abs a)

instance KinematicMath MetersPerSecondSquared where
    (MetersPerSecondSquared a) |+| (MetersPerSecondSquared b) = clampA (a + b)
    (MetersPerSecondSquared a) |-| (MetersPerSecondSquared b) = clampA (a - b)
    kabs (MetersPerSecondSquared a) = MetersPerSecondSquared (abs a)

instance KinematicMath MillimetersPerSecondSquared where
    (MillimetersPerSecondSquared a) |+| (MillimetersPerSecondSquared b) = clampAMm (a + b)
    (MillimetersPerSecondSquared a) |-| (MillimetersPerSecondSquared b) = clampAMm (a - b)
    kabs (MillimetersPerSecondSquared a) = MillimetersPerSecondSquared (abs a)

instance KinematicMath Seconds where
    (Seconds a) |+| (Seconds b) = let r = a + b in if r < 0 then ClampedToMin (Seconds 0) else Safe (Seconds r)
    (Seconds a) |-| (Seconds b) = let r = a - b in if r < 0 then ClampedToMin (Seconds 0) else Safe (Seconds r)
    kabs (Seconds a) = Seconds (abs a)

instance KinematicMath Hertz where
    (Hertz a) |+| (Hertz b) = let r = a + b in if r < 0 then ClampedToMin (Hertz 0) else Safe (Hertz r)
    (Hertz a) |-| (Hertz b) = let r = a - b in if r < 0 then ClampedToMin (Hertz 0) else Safe (Hertz r)
    kabs (Hertz a) = Hertz (abs a)

class KinematicMultiply a b c where
    (|*|) :: a -> b -> SafetyResult c

class KinematicDivide a b c where
    (|/|) :: a -> b -> SafetyResult c


instance KinematicMultiply MetersPerSecond Seconds Meters where
    (MetersPerSecond v) |*| (Seconds t) = Safe (Meters (v * t))

instance KinematicMultiply Seconds MetersPerSecond Meters where
    (Seconds t) |*| (MetersPerSecond v) = Safe (Meters (v * t))

instance KinematicMultiply MillimetersPerSecond Seconds Millimeters where
    (MillimetersPerSecond v) |*| (Seconds t) = Safe (Millimeters (v * t))

instance KinematicMultiply Seconds MillimetersPerSecond Millimeters where
    (Seconds t) |*| (MillimetersPerSecond v) = Safe (Millimeters (v * t))

instance KinematicMultiply MetersPerSecondSquared Seconds MetersPerSecond where
    (MetersPerSecondSquared a) |*| (Seconds t) = clampV (a * t)

instance KinematicMultiply Seconds MetersPerSecondSquared MetersPerSecond where
    (Seconds t) |*| (MetersPerSecondSquared a) = clampV (a * t)

instance KinematicMultiply MillimetersPerSecondSquared Seconds MillimetersPerSecond where
    (MillimetersPerSecondSquared a) |*| (Seconds t) = clampVMm (a * t)

instance KinematicMultiply Seconds MillimetersPerSecondSquared MillimetersPerSecond where
    (Seconds t) |*| (MillimetersPerSecondSquared a) = clampVMm (a * t)


instance KinematicDivide Meters Seconds MetersPerSecond where
    (Meters d) |/| (Seconds t) = 
        if abs t < 1e-12 
        then DivByZeroSafe (MetersPerSecond (maxVelocity defaultBounds)) 
        else clampV (d / t)
        
instance KinematicDivide Millimeters Seconds MillimetersPerSecond where
    (Millimeters d) |/| (Seconds t) = 
        if abs t < 1e-12 
        then DivByZeroSafe (MillimetersPerSecond (maxVelocity defaultBounds * 1000.0)) 
        else clampVMm (d / t)

instance KinematicDivide MetersPerSecond Seconds MetersPerSecondSquared where
    (MetersPerSecond v) |/| (Seconds t) = 
        if abs t < 1e-12 
        then DivByZeroSafe (MetersPerSecondSquared (maxAcceleration defaultBounds)) 
        else clampA (v / t)
        
instance KinematicDivide MillimetersPerSecond Seconds MillimetersPerSecondSquared where
    (MillimetersPerSecond v) |/| (Seconds t) = 
        if abs t < 1e-12 
        then DivByZeroSafe (MillimetersPerSecondSquared (maxAcceleration defaultBounds * 1000.0)) 
        else clampAMm (v / t)


instance KinematicMultiply Hertz Meters MetersPerSecond where
    (Hertz f) |*| (Meters d) = clampV (f * d)

instance KinematicMultiply Meters Hertz MetersPerSecond where
    (Meters d) |*| (Hertz f) = clampV (d * f)

instance KinematicDivide MetersPerSecond Meters Hertz where
    (MetersPerSecond v) |/| (Meters d) = 
        if abs d < 1e-12 
        then DivByZeroSafe (Hertz 1000.0)
        else let r = v / d in if r < 0 then ClampedToMin (Hertz 0) else Safe (Hertz r)

instance KinematicDivide MetersPerSecond Hertz Meters where
    (MetersPerSecond v) |/| (Hertz f) = 
        if abs f < 1e-12 
        then DivByZeroSafe (Meters 1000.0)
        else let r = v / f in if r < 0 then ClampedToMin (Meters 0) else Safe (Meters r)

class ScalarMultiply a where
    (|*) :: Double -> a -> SafetyResult a
    (*|) :: a -> Double -> SafetyResult a

instance ScalarMultiply Meters where
    s |* (Meters d) = let r = s * d in if r < 0 then ClampedToMin (Meters 0) else Safe (Meters r)
    (Meters d) *| s = let r = s * d in if r < 0 then ClampedToMin (Meters 0) else Safe (Meters r)

instance ScalarMultiply Millimeters where
    s |* (Millimeters d) = let r = s * d in if r < 0 then ClampedToMin (Millimeters 0) else Safe (Millimeters r)
    (Millimeters d) *| s = let r = s * d in if r < 0 then ClampedToMin (Millimeters 0) else Safe (Millimeters r)

instance ScalarMultiply MetersPerSecond where
    s |* (MetersPerSecond v) = clampV (s * v)
    (MetersPerSecond v) *| s = clampV (s * v)

instance ScalarMultiply MillimetersPerSecond where
    s |* (MillimetersPerSecond v) = clampVMm (s * v)
    (MillimetersPerSecond v) *| s = clampVMm (s * v)

instance ScalarMultiply MetersPerSecondSquared where
    s |* (MetersPerSecondSquared a) = clampA (s * a)
    (MetersPerSecondSquared a) *| s = clampA (s * a)
    
instance ScalarMultiply MillimetersPerSecondSquared where
    s |* (MillimetersPerSecondSquared a) = clampAMm (s * a)
    (MillimetersPerSecondSquared a) *| s = clampAMm (s * a)

instance ScalarMultiply Seconds where
    s |* (Seconds t) = let r = s * t in if r < 0 then ClampedToMin (Seconds 0) else Safe (Seconds r)
    (Seconds t) *| s = let r = s * t in if r < 0 then ClampedToMin (Seconds 0) else Safe (Seconds r)

instance ScalarMultiply Hertz where
    s |* (Hertz f) = let r = s * f in if r < 0 then ClampedToMin (Hertz 0) else Safe (Hertz r)
    (Hertz f) *| s = let r = s * f in if r < 0 then ClampedToMin (Hertz 0) else Safe (Hertz r)


type family AssertWatchdogSafe w l where
    AssertWatchdogSafe w l = IfSafe (CmpNat w l)

type family IfSafe cmp where
    IfSafe 'GT = ()
    IfSafe _ = TypeError ('Text "Safety Invariant Violated: WatchdogTimeout must be greater than SystemLatency")

_assertWatchdogSafe :: AssertWatchdogSafe WatchdogTimeoutMs SystemLatencyMs
_assertWatchdogSafe = ()

systemLatencyTime :: forall l. (l ~ SystemLatencyMs, KnownNat l) => Proxy l -> Seconds
systemLatencyTime p = Seconds (fromInteger (natVal p) / 1000.0)

watchdogTimeoutTime :: forall w. (w ~ WatchdogTimeoutMs, KnownNat w) => Proxy w -> Seconds
watchdogTimeoutTime p = Seconds (fromInteger (natVal p) / 1000.0)
