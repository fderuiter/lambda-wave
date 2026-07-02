{-# LANGUAGE PatternSynonyms #-}
module UI.Presentation (
    BeamDisplayInfo(..),
    getBeamDisplayInfo,
    scalePointToMeters,
    scaleKalmanStateToMeters,
    shouldTriggerAudioAlert
) where

import Data.Types (BeamState(..), Point3D(..))
import SignalProcessing.Kalman (KalmanState(..), pattern V3)

data BeamDisplayInfo = BeamDisplayInfo
    { bdiColorHex   :: String
    , bdiColorRGB   :: (Float, Float, Float)
    , bdiShape      :: String
    , bdiIconSymbol :: String
    } deriving (Show, Eq)

getBeamDisplayInfo :: BeamState -> BeamDisplayInfo
getBeamDisplayInfo BeamOn   = BeamDisplayInfo "#0f0" (0.0, 0.2, 0.0) "circle" "● "
getBeamDisplayInfo BeamOff  = BeamDisplayInfo "#f00" (0.2, 0.0, 0.0) "square" "■ "
getBeamDisplayInfo BeamHold = BeamDisplayInfo "#ff0" (0.2, 0.2, 0.0) "triangle" "▲ "

scalePointToMeters :: Point3D -> Point3D
scalePointToMeters pt = pt
    { px = px pt / 1000.0
    , py = py pt / 1000.0
    , pz = pz pt / 1000.0
    }

scaleKalmanStateToMeters :: KalmanState -> KalmanState
scaleKalmanStateToMeters ks =
    let (pos, vel, acc) = case x ks of
            V3 pVal vVal aVal -> (pVal, vVal, aVal)
            _ -> (0, 0, 0)
    in ks { x = V3 (pos / 1000.0) (vel / 1000.0) (acc / 1000.0) }

shouldTriggerAudioAlert :: Bool -> BeamState -> BeamState -> Bool
shouldTriggerAudioAlert audioEnabled prevState currentState =
    audioEnabled && prevState /= currentState
