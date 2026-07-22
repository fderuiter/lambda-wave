{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}

-- |
-- Module      : Hardware.Types
-- Description : Error Types and Data Structures for Hardware Interaction
-- Copyright   : (c) 2024
-- License     : AGPL-3.0-only
--
-- This module defines the typed errors and data structures used for communication with the
-- TI IWR6843ISK Radar Sensor. It ensures type safety for error handling in 'Consumer'
-- and 'Control' modules, adhering to Class C safety standards (no runtime exceptions).
module Hardware.Types
  ( HardwareError (..),
  )
where

import Control.DeepSeq (NFData (..))
import GHC.Generics (Generic)

-- | Represents errors that can occur during hardware interaction.
-- These cover configuration, communication, and data parsing failures.
data HardwareError
  = -- | Serial port connection lost or unreadable
    ConnectionLost
  | -- | Failed to apply sensor configuration
    ConfigurationFailed String
  | -- | General parsing failure (e.g. invalid header)
    ParseError String
  | -- | Operation timed out (e.g. no response to command)
    Timeout
  | -- | Catch-all for unexpected IO errors
    UnknownError String
  | -- | Failed to find Magic Word in stream
    MagicWordMissing
  | -- | Packet length outside valid range (< 36 or > 1MB)
    InvalidLength
  | -- | TLV parsing error (e.g. invalid type or length)
    TlvError String
  | -- | Potential DoS (e.g. max TLV size exceeded)
    DoSAttackDetected
  | -- | POSIX System Error (errno)
    SystemError Int
  | -- | Driver-specific failure
    DriverError String
  | -- | Transient resource availability
    TransientError String
  | -- | Hardware is in simulation mode
    SimulationModeActive
  deriving (Show, Eq, Generic)

instance NFData HardwareError
