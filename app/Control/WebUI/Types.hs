{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Control.WebUI.Types (encodeSystemState) where

import Data.Aeson
import Data.Types
import SignalProcessing.Kalman (KalmanState(..), V3(..))
import qualified Data.Map.Strict as Map

-- Orphan instances for JSON serialization
-- We define them here to keep 'src/' dependency-free.

instance ToJSON Point3D where
    toJSON (Point3D{..}) = object
        [ "x" .= px
        , "y" .= py
        , "z" .= pz
        , "v" .= v
        , "snr" .= snr
        ]

instance ToJSON BeamState where
    toJSON BeamOn   = String "BeamOn"
    toJSON BeamOff  = String "BeamOff"
    toJSON BeamHold = String "BeamHold"

instance ToJSON V3 where
    toJSON (V3 x y z) = toJSON [x, y, z]

instance ToJSON KalmanState where
    toJSON (KalmanState x _) = object
        [ "x" .= x
        -- Skipping covariance matrix for bandwidth
        ]

instance ToJSON SystemState where
    toJSON (SystemState{..}) = object
        [ "currentPoints" .= currentPoints
        , "beamState" .= beamState
        , "lastFrameTime" .= lastFrameTime
        , "kalmanState" .= kalmanState
        , "threadHeartbeats" .= threadHeartbeats
        ]

encodeSystemState :: SystemState ->  Data.ByteString.Lazy.ByteString
encodeSystemState = encode
