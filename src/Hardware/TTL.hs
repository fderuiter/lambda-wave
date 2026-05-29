{-# LANGUAGE StrictData #-}
module Hardware.TTL (
    TTLController(..),
    initTTL,
    readTTLTrigger
) where

import Data.Time.HighRes (getMonotonicTimeNS)

-- | Controller for external TTL signals
data TTLController = TTLController {
    ttlPort :: FilePath
}

-- | Initialize TTL ingestion interface
initTTL :: FilePath -> IO TTLController
initTTL port = do
    putStrLn $ "[TTL] Initializing TTL trigger monitor on port: " ++ port
    return (TTLController port)

-- | Read current TTL state and provide microsecond-precision timestamp
-- Returns (State, Timestamp in ns). True = High/Triggered, False = Low
readTTLTrigger :: TTLController -> IO (Bool, Double)
readTTLTrigger _ = do
    t <- getMonotonicTimeNS
    return (False, fromIntegral t) -- Mocked for CI, replaced by physical read in HIL lab
