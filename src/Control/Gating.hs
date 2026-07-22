{-# LANGUAGE ExplicitNamespaces #-}
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
import Safety.Audit (tryWriteAudit)
import Safety.Result (SafetyResult(..))
import Data.I18n (Translations, translateAudit, translateBeamState)
import qualified Data.Text as T
import Numeric.Kinematics
    ( Distance(..)
    , Velocity(..)
    , Acceleration(..)
    , Time(..)
    , Proxy(..)
    , KinematicMath(..)
    , KinematicMultiply(..)
    , ScalarMultiply(..)
    )
import Hardware.Manifest (type SystemLatencyMs)
import Safety.Verification (systemLatencyTime)


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
                        tryWriteAudit (auditQueue oldSystemState) evt
                        return (mtiState oldSystemState, update meas kConfig predState)
                    Right mtiConfig ->
                        -- Run the MTI filter to advance state, but feed the raw average
                        -- height (not the high-pass output) into the Kalman update so the
                        -- absolute position is preserved and the first-frame zero issue is avoided.
                        let (mtiState', _) = applyStaticClutterRemoval mtiConfig (mtiState oldSystemState) [meas :+ 0.0] -- second tuple element is MTI high-pass output; intentionally ignored, using raw scalar meas for Kalman update
                        in  return (mtiState', update meas kConfig predState)
            else return (mtiState oldSystemState, predState) -- Coasting (Dead Reckoning) if signal lost

    -- 6. Update System State & Resolve Final Beam State
    (finalBeamState, hardwareUpdateNeeded, mEvtToLog, mAudioCmd) <- atomically $ do
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
        let mEvt = if changed
                     then Just $ AuditEvent currTime (if resolvedBeamState == BeamHold || currentBeam == BeamHold then Warning else Info) "Gating" (translateAudit translations (T.pack $ activeLanguage s) currentBeam resolvedBeamState)
                     else Nothing

        let mAudio = if changed && audioAlertEnabled s
                       then Just $ PlayTone (audioVolume s) (audioFrequency s)
                       else Nothing

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

        return (resolvedBeamState, changed, mEvt, mAudio)

    -- Handle Audio Trigger (Non-blocking enqueue)
    case mAudioCmd of
        Just cmd -> do
            st <- readTVarIO stateVar
            atomically $ do
                isFull <- isFullTBQueue (audioQueue st)
                if isFull
                    then return () -- Drop if falling behind (prevents blocking)
                    else writeTBQueue (audioQueue st) cmd
        Nothing -> return ()

    -- Log if needed
    case mEvtToLog of
        Just evt -> do
            st <- readTVarIO stateVar
            tryWriteAudit (auditQueue st) evt
        Nothing -> return ()

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
                st <- readTVarIO stateVar
                tryWriteAudit (auditQueue st) evt
                atomically $ modifyTVar' stateVar (\s -> s { beamState = BeamOff })
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
evaluateGating target tol hyst lat kState oldBeam =
    let (pD, vV, aA) = case x kState of
            V3 pVal' vVal' aVal' -> (Distance pVal', Velocity vVal', Acceleration aVal')
            _                 -> (Distance 0, Velocity 0, Acceleration 0)

        Distance pVal = pD
        Velocity vVal = vV
        Acceleration aVal = aA
        invalid = isNaN pVal || isNaN vVal || isNaN aVal || isInfinite pVal || isInfinite vVal || isInfinite aVal

    in if invalid
       then BeamOff
       else
            let v_lat = case vV |*| lat of
                    Safe d -> d
                    ClampedToMin d -> d
                    ClampedToMax d -> d
                    DivByZeroSafe d -> d
                    Unsafe _ -> Distance 0

                a_lat = case aA |*| lat of
                    Safe velOut -> velOut
                    ClampedToMin velOut -> velOut
                    ClampedToMax velOut -> velOut
                    DivByZeroSafe velOut -> velOut
                    Unsafe _ -> Velocity 0

                a_lat2 = case a_lat |*| lat of
                    Safe d -> d
                    ClampedToMin d -> d
                    ClampedToMax d -> d
                    DivByZeroSafe d -> d
                    Unsafe _ -> Distance 0

                half_a_lat2 = case 0.5 |* a_lat2 of
                    Safe d -> d
                    ClampedToMin d -> d
                    ClampedToMax d -> d
                    DivByZeroSafe d -> d
                    Unsafe _ -> Distance 0

                p_v = case pD |+| v_lat of
                    Safe d -> d
                    ClampedToMin d -> d
                    ClampedToMax d -> d
                    DivByZeroSafe d -> d
                    Unsafe _ -> pD

                predPos = case p_v |+| half_a_lat2 of
                    Safe d -> d
                    ClampedToMin d -> d
                    ClampedToMax d -> d
                    DivByZeroSafe d -> d
                    Unsafe _ -> p_v

                diff1 = case predPos |-| target of
                    Safe d -> d
                    ClampedToMin d -> d
                    ClampedToMax d -> d
                    DivByZeroSafe d -> d
                    Unsafe _ -> Distance 0

                diff2 = case target |-| predPos of
                    Safe d -> d
                    ClampedToMin d -> d
                    ClampedToMax d -> d
                    DivByZeroSafe d -> d
                    Unsafe _ -> Distance 0

                errD = case diff1 |+| diff2 of
                    Safe d -> d
                    ClampedToMin d -> d
                    ClampedToMax d -> d
                    DivByZeroSafe d -> d
                    Unsafe _ -> Distance 0

                offLimit = case tol |+| hyst of
                    Safe d -> d
                    ClampedToMin d -> d
                    ClampedToMax d -> d
                    DivByZeroSafe d -> d
                    Unsafe _ -> tol

            in case oldBeam of
                 BeamOff -> if errD < tol then BeamOn else BeamOff
                 BeamOn  -> if errD < offLimit then BeamOn else BeamOff
                 BeamHold -> BeamHold

-- Requirement FR-GAT-001

-- Requirement FR-GAT-002
