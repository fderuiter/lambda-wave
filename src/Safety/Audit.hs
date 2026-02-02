module Safety.Audit (logDecision, auditLoop) where

import Data.Types
import Control.Concurrent.STM
import qualified Control.Concurrent
import System.IO
import Control.Monad (forever)
import Hardware.Control (getRealTimeNS)

-- | Logs decisions to a file
auditLoop :: TVar SystemState -> FilePath -> IO ()
auditLoop stateVar logPath = do
    withFile logPath AppendMode $ \h -> do
        hSetBuffering h LineBuffering
        forever $ do
            state <- readTVarIO stateVar

            now <- getRealTimeNS
            let entry = show now ++ "," ++ show (beamState state)
            hPutStrLn h entry

            Control.Concurrent.threadDelay 100000

logDecision :: String -> IO ()
logDecision msg = appendFile "audit.log" (msg ++ "\n")
