{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE BangPatterns #-}
module Control.Gating (processFrame, evaluateGating) where

import Data.Types
import Data.Config
import Control.Concurrent.STM
import Data.Time.HighRes (getMonotonicTimeNS)
import Data.List (foldl')
import qualified Data.Map.Strict as Map
import Control.Monad (when)
import SignalProcessing.Kalman (KalmanState(..), KalmanConfig(..), pattern V3, predict, update)
import Hardware.Control (setBeam)
import Hardware.FFI.Bridge (handleHardwareResponse)
import Data.I18n (Translations, translateAudit, translateBeamState)
import qualified Data.Text as T
import Safety.Result (SafetyResult(..))
import Numeric.Kinematics
    ( Distance(..)
    , Velocity(..)
    , Acceleration(..)
    , Time(..)
    , SystemLatencyMs
    , KinematicMultiply(..)
    , ScalarMultiply(..)
    , KinematicMath(..)
    , systemLatencyTime
    , Proxy(..)
    )

kConfig :: KalmanConfig
kConfig = KalmanConfig
    { procNoise = 10.0
    , measNoise = 2.0
    }

processFrame :: Translations -> TVar SystemState -> RadarFrame -> IO ()
processFrame translations stateVar frame = do
    currTime <- getMonotonicTimeNS

    let pts = points frame

    oldSystemState <- readTVarIO stateVar
    let lastTime = lastFrameTime oldSystemState
        oldKState = kalmanState oldSystemState

    let dtNS = if currTime > lastTime then currTime - lastTime else 0
        dtSec = fromIntegral dtNS / 1_000_000_000.0

    let (!totalHeight, !count) = foldl' (\(!sumH, !cnt) pt -> (sumH + pz pt, cnt + 1)) (0.0, 0 :: Int) pts

    let predState = predict dtSec kConfig oldKState

    let newKState = if count > 0
            then let meas = totalHeight / fromIntegral count
                 in update meas kConfig predState
            else predState

    finalBeamState <- atomically $ do
        s <- readTVar stateVar
        let currentBeam = beamState s

        let latencyT = systemLatencyTime (Proxy :: Proxy SystemLatencyMs)
            targetD  = Distance targetHeight
            tolD     = Distance gatingTolerance
            hystD    = Distance hysteresisMargin
            proposedBeamState = evaluateGating targetD tolD hystD latencyT newKState currentBeam

        let resolvedBeamState = if currentBeam == BeamHold
                                then BeamHold
                                else if calibrationStatus s /= CalibrationValid
                                     then BeamOff
                                     else proposedBeamState

        when (resolvedBeamState /= currentBeam) $ do
             let msg = translateAudit translations (T.pack $ activeLanguage s) currentBeam resolvedBeamState
                 sev = if resolvedBeamState == BeamHold || currentBeam == BeamHold then Warning else Info
             writeTBQueue (auditQueue s) (AuditEvent currTime sev "Gating" msg)

        let locStr = T.unpack $ translateBeamState translations (T.pack $ activeLanguage s) resolvedBeamState

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

    let beamBool = case finalBeamState of
            BeamOn -> True
            _      -> False
    
    prevState <- atomically $ do
        s <- readTVar stateVar
        return (beamState s)

    when (prevState /= finalBeamState) $ do
        res <- setBeam stateVar beamBool
        
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

evaluateGating :: Distance -> Distance -> Distance -> Time -> KalmanState -> BeamState -> BeamState
evaluateGating target tol hyst latencyTime kState oldBeam =
    let 
        (pos, vel, acc) = case x kState of
            V3 pVal vVal aVal -> (pVal, vVal, aVal)
            _ -> (0, 0, 0)
        
        posD = Distance pos
        velV = Velocity vel
        accA = Acceleration acc

        invalid = isNaN pos || isNaN vel || isInfinite pos || isInfinite vel
    in if invalid then BeamOff else
       case (velV |*| latencyTime) :: SafetyResult Distance of
           Fault _ -> BeamOff
           Safe term1 ->
               case (accA |*| latencyTime) :: SafetyResult Velocity of
                   Fault _ -> BeamOff
                   Safe vAcc ->
                       case (vAcc |*| latencyTime) :: SafetyResult Distance of
                           Fault _ -> BeamOff
                           Safe term2Raw ->
                               let term2 = 0.5 |* term2Raw
                                   predPos = posD |+| term1 |+| term2
                                   err = kAbs (predPos |-| target)
                                   onLimit = tol
                                   offLimit = tol |+| hyst
                               in case oldBeam of
                                    BeamOff -> if err < onLimit then BeamOn else BeamOff
                                    BeamOn  -> if err < offLimit then BeamOn else BeamOff
                                    BeamHold -> BeamHold
