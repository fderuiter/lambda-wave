{-# LANGUAGE StrictData #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE MultiParamTypeClasses #-}
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
    PolynomialSurface(..),
    BeamState(..),
    CalibrationStatus(..),
    DisplayPreset(..),
    SystemState(..),
    RadarFrame(..),
    Severity(..),
    AuditEvent(..),
    AudioCommand(..),
    TelemetryPacket(..)
) where

import Data.Word (Word64, Word32)
import Data.Map.Strict (Map)
import qualified Data.ByteString as B
import Foreign.Storable
import Control.DeepSeq (NFData(..))
import Control.Concurrent.STM (TBQueue)
import Foreign.Ptr (Ptr, castPtr)
import GHC.Generics (Generic)
import Data.Binary (Binary)

import SignalProcessing.Kalman (KalmanState(..))
import Numeric.Units (ConvertUnits(..), Point3DM(..))
import Numeric.Kinematics (Millimeters(..))
import Data.Complex (Complex)

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

data AudioCommand = PlayTone Double Double -- Volume, Frequency
  deriving (Show, Eq)

instance NFData AudioCommand where
    rnf (PlayTone vol freq) = rnf vol `seq` rnf freq

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

-- | 3D Polynomial Surface Coefficients
-- Model: z = s0 + s1*x + s2*y + s3*x^2 + s4*xy + s5*y^2
data PolynomialSurface = PolynomialSurface
  { s0 :: Double
  , s1 :: Double
  , s2 :: Double
  , s3 :: Double
  , s4 :: Double
  , s5 :: Double
  } deriving (Show, Eq, Generic, Binary)

instance NFData PolynomialSurface where
  rnf (PolynomialSurface c0 c1 c2 c3 c4 c5) =
    rnf c0 `seq` rnf c1 `seq` rnf c2 `seq` rnf c3 `seq` rnf c4 `seq` rnf c5

instance ConvertUnits Point3D Point3DM where
    convertUnits pt = Point3DM
        { pxM = convertUnits (Millimeters (px pt))
        , pyM = convertUnits (Millimeters (py pt))
        , pzM = convertUnits (Millimeters (pz pt))
        }

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
      let fptr = castPtr ptr :: Ptr Float
      xVal <- peekElemOff fptr 0
      yVal <- peekElemOff fptr 1
      zVal <- peekElemOff fptr 2
      vel <- peekElemOff fptr 3
      return $ Point xVal yVal zVal vel
  poke ptr (Point xVal yVal zVal vel) = do
      let fptr = castPtr ptr :: Ptr Float
      pokeElemOff fptr 0 xVal
      pokeElemOff fptr 1 yVal
      pokeElemOff fptr 2 zVal
      pokeElemOff fptr 3 vel

-- | The critical decision state
data BeamState = BeamOn | BeamOff | BeamHold -- Hold is manual override
  deriving (Show, Eq, Generic, Binary)

instance NFData BeamState where
  rnf bs = bs `seq` ()

-- | Hardware Calibration Status
data CalibrationStatus = CalibrationUnverified | CalibrationValid | CalibrationInvalid
  deriving (Show, Eq, Generic, Binary)

instance NFData CalibrationStatus where
  rnf cs = cs `seq` ()

data DisplayPreset = StandardPreset | HighGlarePreset
  deriving (Show, Eq, Generic, Binary)

instance NFData DisplayPreset where
  rnf dp = dp `seq` ()

-- | The Global State shared across threads via STM
data SystemState = SystemState
  { currentPoints :: [Point3D]
  , beamState :: BeamState
  , lastFrameTime :: Word64   -- For Watchdog (Nanoseconds)
  , sequenceNumber :: Word32  -- ^ Monotonic sequence counter for visual safety (cite:source6)
  , isocenter :: Point3D      -- Calibration zero
  , threadHeartbeats :: Map String Word64 -- Heartbeats for Watchdog
  , kalmanState :: KalmanState            -- ^ Current Kalman filter state (position, velocity, acceleration; metres/s)
  , mtiState :: [Complex Double]
      -- ^ MTI\/EMA clutter filter state (one complex value per range bin).
      -- Empty list before the first radar frame is processed; after the first
      -- frame its length equals the number of range bins in that frame.
  , auditQueue :: TBQueue AuditEvent -- ^ High-performance event queue
  , audioQueue :: TBQueue AudioCommand -- ^ Async audio player queue
  , audioAlertEnabled :: Bool -- ^ Feature toggle for Audio Alerts (P2-002)
  , audioVolume :: Double
  , audioFrequency :: Double
  , activeLanguage :: String
  , localizedBeamState :: String
  , calibrationStatus :: CalibrationStatus -- ^ Real-time safety monitoring of hardware calibration health
  , displayPreset :: DisplayPreset
  }

instance NFData SystemState where
  rnf (SystemState pts bs t sn iso hb ks mti aq audioQ ae av af lang locbs cs dp) = 
      rnf pts `seq` rnf bs `seq` rnf t `seq` rnf sn `seq` rnf iso `seq` 
      rnf hb `seq` rnf ks `seq` rnf mti `seq` aq `seq` audioQ `seq` rnf ae `seq` rnf av `seq` rnf af `seq` rnf lang `seq` rnf locbs `seq` rnf cs `seq` rnf dp

-- | Raw parsed structure from the sensor
data RadarFrame = RadarFrame
  { header :: B.ByteString
  , seqNum :: Word32
  , points :: [Point3D]
  } deriving (Show, Eq, Generic, Binary)

instance NFData RadarFrame where
  rnf (RadarFrame h sn pts) = rnf h `seq` rnf sn `seq` rnf pts

-- | Packet for sending telemetry over IPC
data TelemetryPacket = TelemetryPacket
  { tpBeamState :: BeamState
  , tpLastFrameTime :: Word64
  , tpSequenceNumber :: Word32
  , tpIsocenter :: Point3D
  , tpThreadHeartbeats :: Map String Word64
  , tpKalmanState :: KalmanState
  , tpAudioAlertEnabled :: Bool
  , tpAudioVolume :: Double
  , tpAudioFrequency :: Double
  , tpActiveLanguage :: String
  , tpLocalizedBeamState :: String
  , tpCalibrationStatus :: CalibrationStatus
  } deriving (Show, Generic, Binary)
