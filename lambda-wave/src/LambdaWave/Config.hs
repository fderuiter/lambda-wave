module LambdaWave.Config where

-- | System Configuration Constants

-- | Hardware settings
radarMountOffset :: Double
radarMountOffset = 0.0 -- mm

gantryAngle :: Double
gantryAngle = 0.0 -- degrees

-- | Gating logic
gatingTolerance :: Double
gatingTolerance = 3.0 -- mm

targetHeight :: Double
targetHeight = 10.0 -- mm (Example target)

-- | Safety
watchdogTimeoutNS :: Integer
watchdogTimeoutNS = 100 * 1000 * 1000 -- 100ms in nanoseconds

-- | Serial Port
uartBaudRate :: Int
uartBaudRate = 921600
