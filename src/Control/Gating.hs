{-# LANGUAGE BangPatterns #-}
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

-- | Kalman Configuration
-- Process Noise (Q): System agility (how fast we expect breathing to change)
-- Measurement Noise (R): Sensor noise
kConfig :: KalmanConfig
kConfig = KalmanConfig
    { procNoise = 10.0 -- High agility for breathing
    , measNoise = 2.0  -- 2mm noise estimate
    }

-- | The main logic function called every frame
processFrame :: TVar SystemState -> [Point3D] -> IO ()
processFrame stateVar pts = do
    currTime <- getMonotonicTimeNS

    -- 1. Read Previous State
    oldSystemState <- readTVarIO stateVar
    let lastTime = lastFrameTime oldSystemState
        oldKState = kalmanState oldSystemState
        oldBeamState = beamState oldSystemState

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

    -- 5. Gating Logic
    let newBeamState = evaluateGating targetHeight gatingTolerance hysteresisMargin systemLatencyNS newKState oldBeamState

    -- 6. Hardware Actuation
    -- Only set beam if state changed to avoid UART spam (optimization)
    -- But safety says "Refresh always"?
    -- Let's set it always for now to ensure fail-safe (if hardware resets).
    let beamBool = case newBeamState of
            BeamOn -> True
            _      -> False
    setBeam beamBool

    -- 7. Update System State & Log
    atomically $ do
        s <- readTVar stateVar

        -- Log Beam Change
        when (newBeamState /= oldBeamState) $ do
             let msg = "Beam State Changed: " ++ show oldBeamState ++ " -> " ++ show newBeamState
             writeTBQueue (auditQueue s) (AuditEvent currTime Info "Gating" msg)

        -- Update State
        writeTVar stateVar $! s
            { currentPoints = pts
            , beamState = newBeamState
            , lastFrameTime = currTime
            , threadHeartbeats = Map.insert "Gating" currTime (threadHeartbeats s)
            , kalmanState = newKState
            }

-- | Evaluate Gating Decision with Hysteresis and Latency Compensation
-- Pure function for testability.
evaluateGating :: Double      -- ^ Target Height (mm)
               -> Double      -- ^ Tolerance (mm)
               -> Double      -- ^ Hysteresis Margin (mm)
               -> Double      -- ^ System Latency (ns)
               -> KalmanState -- ^ Current Filter State
               -> BeamState   -- ^ Previous Beam State
               -> BeamState   -- ^ New Beam State
evaluateGating target tol hyst latencyNS kState oldBeam =
    let -- Latency Compensation
        -- Predict position at (Now + Latency)
        -- x(t+dt) = x(t) + v(t)*dt + 0.5*a(t)*dt^2
        latencySec = latencyNS / 1_000_000_000.0
        (V3 pos vel acc) = x kState

        -- Check for NaN/Inf
        invalid = isNaN pos || isNaN vel || isInfinite pos || isInfinite vel

        predPos = pos + (vel * latencySec) + (0.5 * acc * (latencySec ** 2))

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
