{-# LANGUAGE StrictData #-}
{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Data.Types (
    Point3D(..),
    Point(..),
    BeamState(..),
    SystemState(..),
    RadarFrame(..)
) where

import qualified Data.ByteString as B
import Foreign.Storable
import Control.DeepSeq (NFData(..))
import Data.Word (Word64)
import GHC.Generics (Generic)

-- | 3D Point in Room Coordinates (mm)
data Point3D = Point3D
  { px :: Double
  , py :: Double
  , pz :: Double
  , v  :: Double -- Velocity from Doppler
  , snr :: Double
  } deriving (Show, Eq, Generic)

instance NFData Point3D

-- | Raw Point structure from "Type 1" TLV (4 floats)
data Point = Point
  { px' :: Float
  , py' :: Float
  , pz' :: Float
  , v'  :: Float
  } deriving (Show, Eq)

instance Storable Point where
  sizeOf _ = 16
  alignment _ = 4
  peek ptr = do
      x <- peekByteOff ptr 0
      y <- peekByteOff ptr 4
      z <- peekByteOff ptr 8
      vel <- peekByteOff ptr 12
      return $ Point x y z vel
  poke ptr (Point x y z vel) = do
      pokeByteOff ptr 0 x
      pokeByteOff ptr 4 y
      pokeByteOff ptr 8 z
      pokeByteOff ptr 12 vel

-- | The critical decision state
data BeamState = BeamOn | BeamOff | BeamHold -- Hold is manual override
  deriving (Show, Eq, Generic)

instance NFData BeamState

-- | The Global State shared across threads via STM
data SystemState = SystemState
  { currentPoints :: [Point3D]
  , beamState :: BeamState
  , lastFrameTime :: Word64 -- Nanoseconds since boot (Monotonic)
  , isocenter :: Point3D    -- Calibration zero
  } deriving (Generic)

instance NFData SystemState

-- | Raw parsed structure from the sensor
data RadarFrame = RadarFrame
  { header :: B.ByteString
  , points :: [Point3D]
  } deriving (Show, Eq, Generic)

instance NFData RadarFrame
