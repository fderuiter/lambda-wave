{-# LANGUAGE StrictData #-}
module LambdaWave.Types where

import Control.Concurrent.STM
import System.Clock (TimeSpec)
import Data.Word (Word8)
import qualified Data.ByteString as B

-- | 3D Point in Room Coordinates (mm)
data Point3D = Point3D
  { px :: Double
  , py :: Double
  , pz :: Double
  , v  :: Double -- Velocity from Doppler
  , snr :: Double
  } deriving (Show, Eq)

-- | The critical decision state
data BeamState = BeamOn | BeamOff | BeamHold -- Hold is manual override
  deriving (Show, Eq)

-- | The Global State shared across threads via STM
data SystemState = SystemState
  { currentPoints :: [Point3D]
  , beamState :: BeamState
  , lastFrameTime :: TimeSpec -- For Watchdog
  , isocenter :: Point3D      -- Calibration zero
  }

-- | Raw parsed structure from the sensor
data RadarFrame = RadarFrame
  { header :: B.ByteString
  , points :: [Point3D]
  } deriving (Show, Eq)
