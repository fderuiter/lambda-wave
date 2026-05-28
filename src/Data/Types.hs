{-# LANGUAGE StrictData #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# OPTIONS_GHC -Wno-orphans #-}
-- |
-- Module      : Data.Types
-- Description : Core system types
--
-- Defines the primary data structures used throughout the Lambda-Wave
-- system, including points, frames, state, and audit events.
module Data.Types (
    Point3D(..),
    Point(..),
    BeamState(..),
    SystemState(..),
    RadarFrame(..),
    Severity(..),
    AuditEvent(..),
    TelemetryPacket(..)
) where

import Data.Word (Word64)
import Data.Map.Strict (Map)
import qualified Data.ByteString as B
import Foreign.Storable
import Control.DeepSeq (NFData(..))
import Control.Concurrent.STM (TBQueue)
import GHC.Generics (Generic)
import Data.Binary (Binary)

import SignalProcessing.Kalman (KalmanState(..))

-- | Severity Levels for Audit Logs
data Severity = Info | Warning | Critical
    deriving (Show, Eq)

instance NFData Severity where
    rnf s = s `seq` ()

-- | Immutable Audit Event
data AuditEvent = AuditEvent
    { eventTime :: Word64  -- ^ Timestamp (ns)
    , severity  :: Severity
    , component :: String  -- ^ Source Component (e.g. "Gating", "Watchdog")
    , message   :: String
    } deriving (Show, Eq)

instance NFData AuditEvent where
    rnf (AuditEvent t s c m) = rnf t `seq` rnf s `seq` rnf c `seq` rnf m

-- | 3D Point in Room Coordinates (mm)
data Point3D = Point3D
  { px :: Double
  , py :: Double
  , pz :: Double
  , v  :: Double -- Velocity from Doppler
  , snr :: Double
  } deriving (Show, Eq, Generic, Binary)

instance NFData Point3D where
  rnf (Point3D xVal yVal zVal vel sVal) = rnf xVal `seq` rnf yVal `seq` rnf zVal `seq` rnf vel `seq` rnf sVal

-- | Raw Point structure from "Type 1" TLV (4 floats)
data Point = Point
  { px' :: Float
  , py' :: Float
  , pz' :: Float
  , v'  :: Float
  } deriving (Show, Eq, Generic, Binary)

instance Storable Point where
  sizeOf _ = 16
  alignment _ = 4
  peek ptr = do
      xVal <- peekByteOff ptr 0
      yVal <- peekByteOff ptr 4
      zVal <- peekByteOff ptr 8
      vel <- peekByteOff ptr 12
      return $ Point xVal yVal zVal vel
  poke ptr (Point xVal yVal zVal vel) = do
      pokeByteOff ptr 0 xVal
      pokeByteOff ptr 4 yVal
      pokeByteOff ptr 8 zVal
      pokeByteOff ptr 12 vel

-- | The critical decision state
data BeamState = BeamOn | BeamOff | BeamHold -- Hold is manual override
  deriving (Show, Eq, Generic, Binary)

instance NFData BeamState where
  rnf bs = bs `seq` ()

-- | The Global State shared across threads via STM
data SystemState = SystemState
  { currentPoints :: [Point3D]
  , beamState :: BeamState
  , lastFrameTime :: Word64   -- For Watchdog (Nanoseconds)
  , isocenter :: Point3D      -- Calibration zero
  , threadHeartbeats :: Map String Word64 -- Heartbeats for Watchdog
  , kalmanState :: KalmanState -- ^ Filtered state (Position, Velocity, Accel)
  , auditQueue :: TBQueue AuditEvent -- ^ High-performance event queue
  , audioAlertEnabled :: Bool -- ^ Feature toggle for Audio Alerts (P2-002)
  }

instance NFData SystemState where
  rnf (SystemState pts bs t iso hb ks aq ae) = rnf pts `seq` rnf bs `seq` rnf t `seq` rnf iso `seq` rnf hb `seq` rnf ks `seq` aq `seq` rnf ae

-- | Raw parsed structure from the sensor
data RadarFrame = RadarFrame
  { header :: B.ByteString
  , points :: [Point3D]
  } deriving (Show, Eq, Generic, Binary)

instance NFData RadarFrame where
  rnf (RadarFrame h pts) = rnf h `seq` rnf pts

-- | Packet for sending telemetry over IPC
data TelemetryPacket = TelemetryPacket
  { tpPoints :: [Point3D]
  , tpBeamState :: BeamState
  , tpLastFrameTime :: Word64
  , tpIsocenter :: Point3D
  , tpThreadHeartbeats :: Map String Word64
  , tpKalmanState :: KalmanState
  , tpAudioAlertEnabled :: Bool
  } deriving (Show, Generic, Binary)
