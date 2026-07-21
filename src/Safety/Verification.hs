{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Safety.Verification
  ( type ActiveSafetyChecks,
    assertSafetyChecks,
    systemLatencyTime,
    watchdogTimeoutTime,
  )
where

import Data.Proxy
import GHC.TypeLits
import Hardware.Manifest (type SystemLatencyMs, type WatchdogTimeoutMs)
import Numeric.Kinematics (Time (..))

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
