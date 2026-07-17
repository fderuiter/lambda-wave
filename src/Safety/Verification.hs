{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Safety.Verification
    ( type ActiveSafetyChecks
    , assertSafetyChecks
    , systemLatencyTime
    , watchdogTimeoutTime
    ) where

import GHC.TypeLits
import Data.Proxy
import Hardware.Manifest (type WatchdogTimeoutMs, type SystemLatencyMs)
import Numeric.Kinematics (Time(..))

type family AssertWatchdogSafe w l where
    AssertWatchdogSafe w l = IfSafe (CmpNat w l)

type family IfSafe cmp where
    IfSafe 'GT = ()
    IfSafe _ = TypeError ('Text "Safety Invariant Violated: WatchdogTimeout must be greater than SystemLatency")

-- Composite validation symbol
type ActiveSafetyChecks = AssertWatchdogSafe WatchdogTimeoutMs SystemLatencyMs

assertSafetyChecks :: ActiveSafetyChecks
assertSafetyChecks = ()

systemLatencyTime :: forall l. (l ~ SystemLatencyMs, KnownNat l) => Proxy l -> Time
systemLatencyTime p = Time (fromInteger (natVal p) / 1000.0)

watchdogTimeoutTime :: forall w. (w ~ WatchdogTimeoutMs, KnownNat w) => Proxy w -> Time
watchdogTimeoutTime p = Time (fromInteger (natVal p) / 1000.0)


