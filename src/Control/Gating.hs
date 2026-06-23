{-# LANGUAGE BangPatterns #-}
-- |
-- Module      : Control.Gating
-- Description : Beam gating logic
--
-- Evaluates real-time motion states and determines whether the therapeutic
-- beam should be active, utilizing hysteresis and latency compensation.
module Control.Gating (processFrame, evaluateGating) where

import Data.Types
import Data.Config
import Control.Concurrent.STM
import Data.Time.HighRes (getMonotonicTimeNS)
import Data.List (foldl')
import qualified Data.Map.Strict as Map
import Control.Monad (when)
import SignalProcessing.Kalman (KalmanState(..), KalmanConfig(..), V3(..), predict, update)
import Hardware.Control (setBeam)
import Data.I18n (Translations, translateAudit, translateBeamState)
import qualified Data.Text as T
import Numeric.Kinematics
    ( Distance(..)
    , Velocity(..)
    , Acceleration(..)
    , Time(..)
    , SystemLatencyMs
    , KinematicMultiply(..)
    , ScalarMultiply(..)
    , systemLatencyTime
    , Proxy(..)
    )

-- | Kalman Configuration
-- Process Noise (Q): System agility (how fast we expect breathing to change)
-- Measurement Noise (R): Sensor noise
kConfig :: KalmanConfig
kConfig = KalmanConfig
    { procNoise = 10.0 -- High agility for breathing
    , measNoise = 2.0  -- 2mm noise estimate
    }


-- | The main logic function called every frame
processFrame :: Translations -> TVar SystemState -> RadarFrame -> IO ()
processFrame translations stateVar frame = do
    currTime <- getMonotonicTimeNS

    let pts = points frame

    -- 1. Read Previous State
    oldSystemState <- readTVarIO stateVar
    let lastTime = lastFrameTime oldSystemState
        oldKState = kalmanState oldSystemState

    -- 2. Calculate DT (Seconds)
    let dtNS = if currTime > lastTime then currTime - lastTime else 0
        dtSec = fromIntegral dtNS / 1_000_000_000.0

    -- 3. Measurement (Average Height)
    -- Optimize: Strict fold
    let (!totalHeight, !count) = foldl' (\(!sumH, !cnt) pt -> (sumH + pz pt, cnt + 1)) (0.0, 0 :: Int) pts

    -- 4. Kalman Filter Step
    -- Predict
    let predState = predict dtSec kConfig oldKState

    -- Update (only if we have measurements)
    let newKState = if count > 0
            then let meas = totalHeight / fromIntegral count
                 in update meas kConfig predState
            else predState -- Coasting (Dead Reckoning) if signal lost

    -- 6. Update System State & Resolve Final Beam State
    finalBeamState <- atomically $ do
        s <- readTVar stateVar
        let currentBeam = beamState s

        -- 5. Gating Logic
        -- Note: We calculate based on 'currentBeam' inside the transaction.
        -- This ensures that if the UI thread released BeamHold or modified the state concurrently,
        -- we use the fresh state as the basis for hysteresis and transition logic.
        let latencyT = systemLatencyTime (Proxy :: Proxy SystemLatencyMs)
            targetD  = Distance targetHeight
            tolD     = Distance gatingTolerance
            hystD    = Distance hysteresisMargin
            proposedBeamState = evaluateGating targetD tolD hystD latencyT newKState currentBeam

        -- Safety: If current state is BeamHold, we MUST respect it.
        -- Otherwise, we transition to the proposed state.
        let resolvedBeamState = if currentBeam == BeamHold
                                then BeamHold
                                else proposedBeamState

        -- Log Beam Change (only if resolved state is different from what we read initially or updated)
        -- We compare against 'currentBeam' to log transitions that happen NOW.
        when (resolvedBeamState /= currentBeam) $ do
             let msg = translateAudit translations (T.pack $ activeLanguage s) currentBeam resolvedBeamState
                 sev = if resolvedBeamState == BeamHold || currentBeam == BeamHold then Warning else Info
             writeTBQueue (auditQueue s) (AuditEvent currTime sev "Gating" msg)

        let locStr = T.unpack $ translateBeamState translations (T.pack $ activeLanguage s) resolvedBeamState

        -- Update State
        writeTVar stateVar $! s
            { currentPoints = pts
            , beamState = resolvedBeamState
            , lastFrameTime = currTime
            , sequenceNumber = seqNum frame
            , threadHeartbeats = Map.insert "Gating" currTime (threadHeartbeats s)
            , kalmanState = newKState
            , localizedBeamState = locStr
            }

        return resolvedBeamState

    -- 7. Hardware Actuation
    -- Only set beam if state changed to avoid UART spam (optimization)
    -- But safety says "Refresh always"?
    -- Let's set it always for now to ensure fail-safe (if hardware resets).
    let beamBool = case finalBeamState of
            BeamOn -> True
            _      -> False
    res <- setBeam beamBool
    case res of
        Left err -> do
            let msg = "Hardware actuation failed: " ++ show err
            let evt = AuditEvent currTime Critical "Hardware" msg
            atomically $ do
                s <- readTVar stateVar
                writeTBQueue (auditQueue s) evt
                writeTVar stateVar (s { beamState = BeamOff })
        Right () -> return ()

-- | Evaluate Gating Decision with Hysteresis and Latency Compensation
-- Pure function for testability.
evaluateGating :: Distance    -- ^ Target Height (Distance)
               -> Distance    -- ^ Tolerance (Distance)
               -> Distance    -- ^ Hysteresis Margin (Distance)
               -> Time        -- ^ System Latency
               -> KalmanState -- ^ Current Filter State
               -> BeamState   -- ^ Previous Beam State
               -> BeamState   -- ^ New Beam State
evaluateGating target tol hyst latencyTime kState oldBeam =
    let -- Latency Compensation
        -- Predict position at (Now + Latency)
        -- x(t+dt) = x(t) + v(t)*dt + 0.5*a(t)*dt^2
        (V3 pos vel acc) = x kState
        
        posD = Distance pos
        velV = Velocity vel
        accA = Acceleration acc

        -- Check for NaN/Inf
        invalid = isNaN pos || isNaN vel || isInfinite pos || isInfinite vel

        term1 = velV |*| latencyTime
        term2 = 0.5 |* (((accA |*| latencyTime) :: Velocity) |*| latencyTime)
        predPos = posD + term1 + term2

        err = abs (predPos - target)

        -- Thresholds
        -- ON Threshold: Tolerance
        -- OFF Threshold: Tolerance + Hysteresis
        onLimit = tol
        offLimit = tol + hyst

    in if invalid
       then BeamOff
       else case oldBeam of
            BeamOff -> if err < onLimit then BeamOn else BeamOff
            BeamOn  -> if err < offLimit then BeamOn else BeamOff
            BeamHold -> BeamHold -- Manual override persists

-- Requirement FR-GAT-001

-- Requirement FR-GAT-002
