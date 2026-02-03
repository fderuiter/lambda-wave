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

-- | System Configuration Constants

-- | Hardware settings
radarMountOffset :: Double
radarMountOffset = 0.0 -- mm

gantryAngle :: Double
gantryAngle = 0.0 -- degrees

-- | Gating logic
gatingTolerance :: Double
gatingTolerance = 3.0 -- mm

hysteresisMargin :: Double
hysteresisMargin = 0.5 -- mm

targetHeight :: Double
targetHeight = 10.0 -- mm (Example target)

systemLatencyNS :: Double
systemLatencyNS = 50_000_000 -- 50ms in nanoseconds

-- | Safety
watchdogTimeoutNS :: Integer
watchdogTimeoutNS = 100 * 1000 * 1000 -- 100ms in nanoseconds

-- | Serial Port
uartBaudRate :: Int
uartBaudRate = 921600
