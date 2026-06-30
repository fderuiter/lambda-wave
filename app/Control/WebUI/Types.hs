{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Control.WebUI.Types (
    WebPayload(..),
    encodeWebPayload
) where

import Data.Aeson
import qualified Data.Aeson.KeyMap as KM
import Data.Types
import SignalProcessing.Kalman (KalmanState(..), pattern V3)
import SignalProcessing.Matrix (Vector, vToList)
import UI.Presentation (getBeamDisplayInfo, bdiColorHex, bdiShape, bdiIconSymbol, scalePointToMeters, scaleKalmanStateToMeters)
import qualified Data.ByteString.Lazy as BL

data WebPayload = WebPayload SystemState Bool

encodeWebPayload :: SystemState -> Bool -> BL.ByteString
encodeWebPayload state beep = encode (WebPayload state beep)

instance ToJSON WebPayload where
    toJSON (WebPayload state beep) =
        let val = toJSON state
        in case val of
            Object m -> Object (KM.insert "triggerAudioAlert" (toJSON beep) m)
            _ -> val

instance ToJSON BeamState where
    toJSON BeamOn   = "BeamOn"
    toJSON BeamOff  = "BeamOff"
    toJSON BeamHold = "BeamHold"

instance ToJSON Point3D where
    toJSON (Point3D{..}) = object
        [ "x" .= px
        , "y" .= py
        , "z" .= pz
        , "v" .= v
        , "snr" .= snr
        ]

instance ToJSON KalmanState where
    toJSON (KalmanState{..}) = object
        [ "stateVector" .= case vToList x of
            [vx, vy, vz] -> object ["x" .= vx, "y" .= vy, "z" .= vz]
            lst -> toJSON lst
        ]

instance ToJSON SystemState where
    toJSON (SystemState{..}) = 
        let displayInfo = getBeamDisplayInfo beamState
        in object
        [ "beamState" .= localizedBeamState
        , "rawBeamState" .= beamState
        , "beamColorHex" .= bdiColorHex displayInfo
        , "beamShape" .= bdiShape displayInfo
        , "beamIconSymbol" .= bdiIconSymbol displayInfo
        , "pointCloud" .= map scalePointToMeters currentPoints
        , "respiratoryTrace" .= scaleKalmanStateToMeters kalmanState
        , "timestamp" .= lastFrameTime
        , "sequenceNumber" .= sequenceNumber
        , "heartbeats" .= threadHeartbeats
        , "audioAlertEnabled" .= audioAlertEnabled
        , "activeLanguage" .= activeLanguage
        , "calibrationStatus" .= show calibrationStatus
        ]
