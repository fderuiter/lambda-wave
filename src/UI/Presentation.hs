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
scalePointToMeters p = p
    { px = px p / 1000.0
    , py = py p / 1000.0
    , pz = pz p / 1000.0
    }

scaleKalmanStateToMeters :: KalmanState -> KalmanState
scaleKalmanStateToMeters ks =
    let V3 pos vel acc = x ks
    in ks { x = V3 (pos / 1000.0) (vel / 1000.0) (acc / 1000.0) }

shouldTriggerAudioAlert :: Bool -> BeamState -> BeamState -> Bool
shouldTriggerAudioAlert audioEnabled prevState currentState =
    audioEnabled && prevState /= BeamOff && currentState == BeamOff
