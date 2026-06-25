-- |
-- Module      : Data.Config
-- Description : System configuration constants
--
-- This module defines the static configuration parameters for the Lambda-Wave system.
module Data.Config (
    radarMountOffset,
    gantryAngle,
    gatingTolerance,
    hysteresisMargin,
    targetHeight,
    systemLatencyNS,
    watchdogTimeoutNS,
    uartBaudRate
) where

import Hardware.Manifest (systemLatencyMs, framePeriodicityMs, dataBaudRate)

-- | System Configuration Constants

-- | Hardware settings
radarMountOffset :: Double
radarMountOffset = 0.0 -- mm

-- | The angle of the gantry
-- Fixed angle for setup (degrees)
gantryAngle :: Double
gantryAngle = 0.0 -- degrees

-- | Gating logic
gatingTolerance :: Double
gatingTolerance = 3.0 -- mm

-- | The tolerance threshold for gating
-- Hysteresis margin allows smooth transition and prevents flickering.
hysteresisMargin :: Double
hysteresisMargin = 0.5 -- mm

-- | The target height for gating decision
-- Defines the ideal height in mm for gating triggers.
targetHeight :: Double
targetHeight = 10.0 -- mm (Example target)

-- | The estimated latency of the system in nanoseconds
systemLatencyNS :: Double
systemLatencyNS = fromIntegral systemLatencyMs * 1_000_000

-- | Safety
watchdogTimeoutNS :: Integer
watchdogTimeoutNS = fromIntegral framePeriodicityMs * 1_000_000

-- | Serial Port
uartBaudRate :: Int
uartBaudRate = dataBaudRate

-- Requirement FR-DAQ-002
-- Hazard H-SYS-005: Config file error
