{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}
module Safety.Audit (auditLoop) where

import Data.Types
import Control.Concurrent.STM
import Control.Concurrent (threadDelay)
import System.IO
import Control.Monad (when)
import Data.Functor ((<&>))
import Data.Time.HighRes (getMonotonicTimeNS)
import qualified Data.Map.Strict as Map
import Control.Exception (try, IOException)
import System.Posix.Files (rename)
import Text.Printf (printf)
import Data.Char (isControl)
import qualified Data.ByteString as B
import Safety.Crypto (encryptLog)
import Safety.Result (SafetyResult(..))

data LoopResult = RotationNeeded

auditLoop :: TVar SystemState -> FilePath -> IO ()
auditLoop stateVar logPath = do
    state <- readTVarIO stateVar
    let queue = auditQueue state
    runAuditLoop stateVar queue logPath

runAuditLoop :: TVar SystemState -> TBQueue AuditEvent -> FilePath -> IO ()
runAuditLoop stateVar queue logPath = do
    result <- try $ withFile logPath AppendMode $ \h -> do
        hSetBuffering h LineBuffering
        initialSize <- hFileSize h
        processEvents stateVar queue h initialSize

    case result of
        Left (e :: IOException) -> do
            hPutStrLn stderr $ "AUDIT SUBSYSTEM FAILURE: " ++ show e
            threadDelay 1_000_000
            runAuditLoop stateVar queue logPath
        Right RotationNeeded -> do
            _ <- try $ rename logPath (logPath ++ ".bak") :: IO (Either IOException ())
            runAuditLoop stateVar queue logPath

processEvents :: TVar SystemState -> TBQueue AuditEvent -> Handle -> Integer -> IO LoopResult
processEvents stateVar queue h = go
  where
    go currentSize = do
        now <- getMonotonicTimeNS
        atomically $ modifyTVar' stateVar $ \s ->
            s { threadHeartbeats = Map.insert "Audit" now (threadHeartbeats s) }

        delayVar <- registerDelay 100_000

        mEvt <- atomically $
            (readTBQueue queue <&> Just)
            `orElse`
            do
                expired <- readTVar delayVar
                if expired then return Nothing else retry

        case mEvt of
            Nothing -> go currentSize
            Just evt -> do
                let sanitize = map (\c -> if isControl c then ' ' else c)
                let entry = printf "[%d] [%s] [%s] %s"
                                (eventTime evt)
                                (show (severity evt))
                                (sanitize $ component evt)
                                (sanitize $ message evt)
                encRes <- encryptLog (entry ++ "\n")
                case encRes of
                    Safe enc -> do
                        B.hPut h enc
                        when (severity evt == Critical || severity evt == Warning) $ hFlush h
                        let !newSize = currentSize + fromIntegral (B.length enc)
                        if newSize > 10 * 1024 * 1024
                            then return RotationNeeded
                            else go newSize
                    Fault e -> do
                        -- Trigger software shutdown directly
                        -- since we can't write to the log, we can't use triggerShutdown which tries to write to the queue
                        -- Actually, we can update BeamOff directly here.
                        atomically $ modifyTVar' stateVar $ \s -> s { beamState = BeamOff }
                        hPutStrLn stderr $ "CRITICAL: Crypto fault in Audit: " ++ e
                        go currentSize

