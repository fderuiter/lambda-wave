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
    uartBaudRate,
    quantizationEnabled,
    quantizationScale
) where

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
-- System latency compensation value (50ms).
systemLatencyNS :: Double
systemLatencyNS = 50_000_000 -- 50ms in nanoseconds

-- | Safety
watchdogTimeoutNS :: Integer
watchdogTimeoutNS = 100 * 1000 * 1000 -- 100ms in nanoseconds

-- | Serial Port
uartBaudRate :: Int
uartBaudRate = 921600

-- Requirement FR-DAQ-002
-- | Quantization for High-Frequency Telemetry

quantizationEnabled :: Bool

quantizationEnabled = True



quantizationScale :: Float

quantizationScale = 2000.0 / 32767.0 -- Covers +/- 2 meters


