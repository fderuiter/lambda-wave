{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module UI.Web (runServer) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.STM (TVar, readTVarIO)
import Control.Monad (forever)
import Data.Aeson (ToJSON(..), object, (.=), encode)
import Network.HTTP.Types (status200, status404)
import Network.Wai (Application, responseFile, responseLBS, pathInfo)
import Network.Wai.Handler.Warp (run)
import Network.Wai.Handler.WebSockets (websocketsOr)
import Network.WebSockets (ServerApp, Connection, sendTextData, acceptRequest, withPingThread, defaultConnectionOptions)

import Data.Types
import SignalProcessing.Kalman (KalmanState(..), V3(..))

-- Orphan ToJSON instances for Data.Types and SignalProcessing.Kalman

instance ToJSON Point3D where
    toJSON Point3D{..} = object
        [ "x"   .= px
        , "y"   .= py
        , "z"   .= pz
        , "v"   .= v
        , "snr" .= snr
        ]

instance ToJSON BeamState where
    toJSON BeamOn   = "BeamOn"
    toJSON BeamOff  = "BeamOff"
    toJSON BeamHold = "BeamHold"

instance ToJSON V3 where
    toJSON (V3 x y z) = object
        [ "x" .= x
        , "y" .= y
        , "z" .= z
        ]

-- We only serialize the Position (x) of the Kalman State for visualization
instance ToJSON KalmanState where
    toJSON KalmanState{..} = object
        [ "position" .= x
        ]

-- Wrapper to serialize the entire system state snapshot for the UI
data UIState = UIState
    { uiPoints :: [Point3D]
    , uiTarget :: KalmanState
    , uiBeam   :: BeamState
    }

instance ToJSON UIState where
    toJSON UIState{..} = object
        [ "points" .= uiPoints
        , "target" .= uiTarget
        , "beam"   .= uiBeam
        ]

-- | The main entry point for the Web UI Server
runServer :: Int -> TVar SystemState -> IO ()
runServer port stateVar = do
    putStrLn $ "Starting Web UI on http://localhost:" ++ show port
    run port $ websocketsOr defaultConnectionOptions (wsApp stateVar) httpApp

-- | HTTP Application: Serves static files
httpApp :: Application
httpApp req respond = do
    case pathInfo req of
        [] -> respond $ responseFile status200 [("Content-Type", "text/html")] "app/static/index.html" Nothing
        _  -> respond $ responseLBS status404 [] "Not Found"

-- | WebSocket Application: Streams state updates
wsApp :: TVar SystemState -> ServerApp
wsApp stateVar pending = do
    conn <- acceptRequest pending
    -- Keep connection alive with pings every 30s
    withPingThread conn 30 (return ()) $ do
        broadcastLoop conn stateVar

-- | Broadcast Loop: Sends state at ~30Hz
broadcastLoop :: Connection -> TVar SystemState -> IO ()
broadcastLoop conn stateVar = forever $ do
    sysState <- readTVarIO stateVar

    let uiState = UIState
            { uiPoints = currentPoints sysState
            , uiTarget = kalmanState sysState
            , uiBeam   = beamState sysState
            }

    sendTextData conn (encode uiState)
    threadDelay 33000 -- ~30Hz
