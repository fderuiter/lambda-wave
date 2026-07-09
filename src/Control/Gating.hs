{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE BangPatterns #-}
-- |
-- Module      : Control.Gating
-- Description : Beam gating logic
--
-- Evaluates real-time motion states and determines whether the therapeutic
-- beam should be active, utilizing hysteresis and latency compensation.
--
-- ⚠️ SAFETY-CRITICAL
--
-- = Failure Mode
-- Beam remains ON during patient motion, delivering incorrect radiation dose.
--
-- = Mitigation
-- Hysteresis with conservative thresholds and latency compensation are applied.
module Control.Gating (processFrame, evaluateGating) where

import Safety.Result (SafetyResult(..))
unwrapSafety :: SafetyResult a -> a
unwrapSafety (Safe a) = a
unwrapSafety (ClampedToMin a) = a
unwrapSafety (ClampedToMax a) = a
unwrapSafety (DivByZeroSafe a) = a
unwrapSafety (Unsafe _) = error "Unsafe math evaluation"


import Data.Types
import Data.Config
import Control.Concurrent.STM
import Data.Time.HighRes (getMonotonicTimeNS)
import Data.List (foldl')
import qualified Data.Map.Strict as Map
import Control.Monad (when)
import SignalProcessing.Kalman (KalmanState(..), KalmanConfig(..), pattern V3, predict, update)
import SignalProcessing.FMCW (applyStaticClutterRemoval, mkMTIConfig)
import Data.Complex (Complex(..))
import Hardware.Control (setBeam)
import Hardware.FFI.Bridge (handleHardwareResponse)
import Data.I18n (Translations, translateAudit, translateBeamState)
import qualified Data.Text as T
import Numeric.Kinematics
    ( Distance(..)
    , Velocity(..)
    , Acceleration(..)
    , Time(..)
    , SystemLatencyMs
    , KinematicMultiply(..)
    , KinematicMath(..)
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

-- | MTI filter base learning rate (α_base): used during active motion.
-- Must satisfy 0 ≤ mtiAlphaBase ≤ mtiAlphaMax ≤ 1.
-- A lower value gives more inertia to the clutter mean estimate. (FR-DSP-004)
mtiAlphaBaseVal :: Double
mtiAlphaBaseVal = 0.1

-- | MTI filter maximum learning rate (α_max): used when the scene is static.
-- Higher value allows faster adaptation of the clutter baseline. (FR-DSP-004)
mtiAlphaMaxVal :: Double
mtiAlphaMaxVal = 0.9

-- | MTI motion variance threshold: separates static from dynamic scenes.
-- Scene variance below this value triggers the faster α_max adaptation path.
mtiThresholdVal :: Double
mtiThresholdVal = 1.0


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
    -- Runs in IO so that MTI config errors can be surfaced via the audit queue.
    (newMtiState, newKState) <- if count > 0
            then do
                let meas = totalHeight / fromIntegral count
                case mkMTIConfig mtiAlphaBaseVal mtiAlphaMaxVal mtiThresholdVal of
                    Left err -> do
                        -- Config validation failed: log a warning and fall back to
                        -- the unfiltered measurement so gating is not silently degraded.
                        let evt = AuditEvent currTime Warning "Gating" ("MTI config error: " ++ err)
                        atomically $ writeTBQueue (auditQueue oldSystemState) evt
                        return (mtiState oldSystemState, update meas kConfig predState)
                    Right mtiConfig ->
                        -- Run the MTI filter to advance state, but feed the raw average
                        -- height (not the high-pass output) into the Kalman update so the
                        -- absolute position is preserved and the first-frame zero issue is avoided.
                        let (mtiState', _) = applyStaticClutterRemoval mtiConfig (mtiState oldSystemState) [meas :+ 0.0] -- second tuple element is MTI high-pass output; intentionally ignored, using raw scalar meas for Kalman update
                        in  return (mtiState', update meas kConfig predState)
            else return (mtiState oldSystemState, predState) -- Coasting (Dead Reckoning) if signal lost

    -- 6. Update System State & Resolve Final Beam State
    (finalBeamState, hardwareUpdateNeeded) <- atomically $ do
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
        -- If calibration is not valid, we MUST block the beam (BeamOff).
        -- Otherwise, we transition to the proposed state.
        let resolvedBeamState
              | currentBeam == BeamHold = BeamHold
              | calibrationStatus s /= CalibrationValid = BeamOff
              | otherwise = proposedBeamState

        -- Log Beam Change (only if resolved state is different from what we read initially or updated)
        -- We compare against 'currentBeam' to log transitions that happen NOW.
        let changed = resolvedBeamState /= currentBeam
        when changed $ do
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
            , mtiState = newMtiState
            , localizedBeamState = locStr
            }

        return (resolvedBeamState, changed)

    -- 7. Hardware Actuation
    -- Only set beam if state changed to avoid UART spam and excessive logging.
    let beamBool = case finalBeamState of
            BeamOn -> True
            _      -> False
    
    when hardwareUpdateNeeded $ do
        res <- setBeam stateVar beamBool
        
        -- Explicitly handle the hardware response
        handleHardwareResponse 
            (\err -> do
                let msg = "Hardware actuation failed: " ++ show err
                let evt = AuditEvent currTime Critical "Hardware" msg
                atomically $ do
                    s <- readTVar stateVar
                    writeTBQueue (auditQueue s) evt
                    writeTVar stateVar (s { beamState = BeamOff })
            )
            (\() -> return ())
            res

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
        (pos, vel, acc) = case x kState of
            V3 pVal vVal aVal -> (pVal, vVal, aVal)
            _ -> (0, 0, 0)
        
        posD = Distance pos
        velV = Velocity vel
        accA = Acceleration acc

        -- Check for NaN/Inf
        invalid = isNaN pos || isNaN vel || isNaN acc || isInfinite pos || isInfinite vel || isInfinite acc

        term1 = unwrapSafety (velV |*| latencyTime)
        term2 = unwrapSafety (0.5 |* unwrapSafety (unwrapSafety ((accA |*| latencyTime) :: SafetyResult Velocity) |*| latencyTime))
        predPos = unwrapSafety (unwrapSafety (posD |+| term1) |+| term2)

        err = kabs (unwrapSafety (predPos |-| target))

        -- Thresholds
        -- ON Threshold: Tolerance
        -- OFF Threshold: Tolerance + Hysteresis
        onLimit = tol
        offLimit = unwrapSafety (tol |+| hyst)

    in if invalid
       then BeamOff
       else case oldBeam of
            BeamOff -> if err < onLimit then BeamOn else BeamOff
            BeamOn  -> if err < offLimit then BeamOn else BeamOff
            BeamHold -> BeamHold -- Manual override persists

-- Requirement FR-GAT-001

-- Requirement FR-GAT-002
