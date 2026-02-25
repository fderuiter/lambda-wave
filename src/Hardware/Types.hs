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
    HardwareError(..),
    isTransient,
    toSeverity,
    logMessage
) where

import Control.DeepSeq (NFData(..))
import GHC.Generics (Generic)
import Data.Types (Severity(..))

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
    | FileError String        -- ^ File access error (e.g. config file missing)
    | DeviceBusy              -- ^ Device is busy or locked
    deriving (Show, Eq, Generic)

instance NFData HardwareError

-- | Determines if an error is transient and likely to succeed on retry.
--
-- Complexity: O(1)
isTransient :: HardwareError -> Bool
isTransient Timeout = True
isTransient DeviceBusy = True
isTransient ConnectionLost = True -- Often transient on startup
isTransient (UnknownError _) = True -- Pessimistically assume transient
isTransient _ = False

-- | Maps a hardware error to a severity level for audit logging.
--
-- Complexity: O(1)
toSeverity :: HardwareError -> Severity
toSeverity DoSAttackDetected = Critical
toSeverity (FileError _) = Critical -- Missing config is critical
toSeverity (ConfigurationFailed _) = Critical
toSeverity ConnectionLost = Warning -- Can recover
toSeverity _ = Warning

-- | Generates a human-readable log message for the error.
--
-- Complexity: O(1) (amortized, string concatenation depends on error length)
logMessage :: HardwareError -> String
logMessage ConnectionLost = "Serial connection lost or unreadable"
logMessage (ConfigurationFailed msg) = "Sensor configuration failed: " ++ msg
logMessage (ParseError msg) = "Stream parsing error: " ++ msg
logMessage Timeout = "Operation timed out"
logMessage (UnknownError msg) = "Unexpected hardware error: " ++ msg
logMessage MagicWordMissing = "Sync Lost: Magic Word Missing"
logMessage InvalidLength = "Corrupt Packet: Invalid Length"
logMessage (TlvError msg) = "TLV Error: " ++ msg
logMessage DoSAttackDetected = "Potential DoS: TLV Too Large"
logMessage (FileError msg) = "File access error: " ++ msg
logMessage DeviceBusy = "Device resource is busy"
