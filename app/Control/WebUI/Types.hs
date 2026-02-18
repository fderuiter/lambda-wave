{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Control.WebUI.Types where

import Data.Aeson
import Data.Types
import SignalProcessing.Kalman (KalmanState(..), V3(..))
import qualified Data.Map.Strict as Map

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

instance ToJSON V3 where
    toJSON (V3 x y z) = object ["x" .= x, "y" .= y, "z" .= z]

instance ToJSON KalmanState where
    toJSON (KalmanState{..}) = object
        [ "stateVector" .= x
        ]

instance ToJSON SystemState where
    toJSON (SystemState{..}) = object
        [ "beamState" .= beamState
        , "pointCloud" .= currentPoints
        , "respiratoryTrace" .= kalmanState
        , "timestamp" .= lastFrameTime
        , "heartbeats" .= threadHeartbeats
        ]
