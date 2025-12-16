module LambdaWave.Safety.Audit (logDecision, auditLoop) where

import LambdaWave.Types
import Control.Concurrent.STM
import System.IO
import Control.Monad (forever)
import System.Clock

-- | Logs decisions to a file
auditLoop :: TVar SystemState -> FilePath -> IO ()
auditLoop stateVar logPath = do
    withFile logPath AppendMode $ \h -> do
        hSetBuffering h LineBuffering
        forever $ do
            state <- readTVarIO stateVar
            -- In a real app, we'd wait for a change or tick
            -- For now, just log periodically?
            -- Better: use a TChan for audit events.
            -- But since we only have stateVar, let's just log every 100ms

            now <- getTime Realtime
            let entry = show (toNanoSecs now) ++ "," ++ show (beamState state)
            hPutStrLn h entry

            Control.Concurrent.threadDelay 100000

logDecision :: String -> IO ()
logDecision msg = appendFile "audit.log" (msg ++ "\n")
