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
import Numeric.Kinematics (Millimeters(..), Meters(..), mmToMeters, MillimetersPerSecond(..), MetersPerSecond(..), mmPerSToMetersPerS, MillimetersPerSecondSquared(..), MetersPerSecondSquared(..), mmPerS2ToMetersPerS2)

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
    { px = let Meters m = mmToMeters (Millimeters (px pt)) in m
    , py = let Meters m = mmToMeters (Millimeters (py pt)) in m
    , pz = let Meters m = mmToMeters (Millimeters (pz pt)) in m
    }

scaleKalmanStateToMeters :: KalmanState -> KalmanState
scaleKalmanStateToMeters ks =
    let (pos, vel, acc) = case x ks of
            V3 pVal vVal aVal -> (pVal, vVal, aVal)
            _ -> (0, 0, 0)
    in ks { x = V3 (let Meters m = mmToMeters (Millimeters pos) in m)
                   (let MetersPerSecond m = mmPerSToMetersPerS (MillimetersPerSecond vel) in m)
                   (let MetersPerSecondSquared m = mmPerS2ToMetersPerS2 (MillimetersPerSecondSquared acc) in m) }

shouldTriggerAudioAlert :: Bool -> BeamState -> BeamState -> Bool
shouldTriggerAudioAlert audioEnabled prevState currentState =
    audioEnabled && prevState /= currentState
