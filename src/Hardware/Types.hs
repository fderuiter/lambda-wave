{-# LANGUAGE StrictData #-}
{-# LANGUAGE DeriveGeneric #-}

{-|
Module      : Hardware.Types
Description : Error Types and Data Structures for Hardware Interaction
Copyright   : (c) 2024
License     : AGPL-3.0-only

This module defines the typed errors and data structures used for communication with the
TI IWR6843ISK Radar Sensor. It ensures type safety for error handling in 'Consumer'
and 'Control' modules, adhering to Class C safety standards (no runtime exceptions).
-}
module Hardware.Types (
    HardwareError(..)
) where

import Control.DeepSeq (NFData(..))
import GHC.Generics (Generic)

-- | Represents errors that can occur during hardware interaction.
-- These cover configuration, communication, and data parsing failures.
data HardwareError
    = ConnectionLost          -- ^ Serial port connection lost or unreadable
    | ConfigurationFailed String -- ^ Failed to apply sensor configuration
    | ParseError String       -- ^ General parsing failure (e.g. invalid header)
    | Timeout                 -- ^ Operation timed out (e.g. no response to command)
    | UnknownError String     -- ^ Catch-all for unexpected IO errors
    | MagicWordMissing        -- ^ Failed to find Magic Word in stream
    | InvalidLength           -- ^ Packet length outside valid range (< 36 or > 1MB)
    | TlvError String         -- ^ TLV parsing error (e.g. invalid type or length)
    | DoSAttackDetected       -- ^ Potential DoS (e.g. max TLV size exceeded)
    | SystemError Int         -- ^ POSIX System Error (errno)
    | DriverError String      -- ^ Driver-specific failure
    | TransientError String   -- ^ Transient resource availability
    deriving (Show, Eq, Generic)

instance NFData HardwareError
