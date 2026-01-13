module Data.Config where

-- | System Configuration Constants

-- | Hardware settings
radarMountOffset :: Double
radarMountOffset = 0.0 -- mm

gantryAngle :: Double
gantryAngle = 0.0 -- degrees

-- | ROI Configuration (Meters)
-- Based on typical patient torso dimensions
-- Coordinate System: Sensor Coordinates (Z is Range)
roiMinX :: Double
roiMinX = -0.3

roiMaxX :: Double
roiMaxX = 0.3

roiMinY :: Double
roiMinY = 0.0

roiMaxY :: Double
roiMaxY = 0.5

roiMinZ :: Double
roiMinZ = 0.5 -- 50cm min range

roiMaxZ :: Double
roiMaxZ = 1.5 -- 1.5m max range

-- | Meshing Configuration
meshGridSize :: Int
meshGridSize = 20 -- 20x20 grid

-- | Gating logic
gatingToleranceMeters :: Double
gatingToleranceMeters = 0.005 -- 5mm tolerance

targetHeightMeters :: Double
targetHeightMeters = 1.0 -- 1.0m (Patient chest distance from radar)

-- | Safety
watchdogTimeoutNS :: Integer
watchdogTimeoutNS = 100 * 1000 * 1000 -- 100ms in nanoseconds

-- | Serial Port
uartBaudRate :: Int
uartBaudRate = 921600
