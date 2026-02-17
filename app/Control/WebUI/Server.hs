{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Control.WebUI.Server
Description : WebSocket and HTTP Server for the Real-Time Web Dashboard.
Copyright   : (c) 2026 Frederick de Ruiter
License     : AGPL-3.0-only
Maintainer  : Frederick de Ruiter <fpderuiter@gmail.com>

Provides a lightweight Warp server that:
1. Serves the embedded dashboard (index.html) on /.
2. Streams real-time SystemState updates via WebSockets on /ws.

This module is part of the application layer (Class B/A) and is isolated
from the safety-critical core (Class C).
-}
module Control.WebUI.Server (runServer) where

import Control.Monad (forever)
import Control.Concurrent (threadDelay, forkIO)
import Control.Concurrent.STM (TVar, readTVarIO, atomically)
import Control.Exception (finally)
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import Network.Wai
import Network.Wai.Handler.Warp (run)
import Network.HTTP.Types (status200, status404)
import Network.WebSockets (ServerApp, acceptRequest, sendTextData, withPingThread)
import Network.Wai.Handler.WebSockets (websocketsOr, defaultConnectionOptions)

import Data.Types (SystemState)
import Control.WebUI.Types (encodeSystemState)
import Control.WebUI.Assets (indexHtml)

-- | Starts the Web Dashboard Server on the specified port.
-- Spawns a background thread that pushes updates to connected clients.
runServer :: TVar SystemState -> Int -> IO ()
runServer stateVar port = do
    putStrLn $ "Starting Web Dashboard on http://localhost:" ++ show port
    run port (app stateVar)

-- | WAI Application that routes HTTP and WebSocket requests.
app :: TVar SystemState -> Application
app stateVar = websocketsOr defaultConnectionOptions (wsApp stateVar) httpApp

-- | HTTP Handler: Serves the static index.html
httpApp :: Application
httpApp req respond = respond $
    case pathInfo req of
        [] -> responseLBS status200 [("Content-Type", "text/html")] (toLazy indexHtml)
        _  -> responseLBS status404 [] "Not Found"

-- | WebSocket Handler: Accepts connection and enters the streaming loop.
wsApp :: TVar SystemState -> ServerApp
wsApp stateVar pending = do
    conn <- acceptRequest pending
    -- Keep connection alive with pings every 30s
    withPingThread conn 30 (return ()) $ do
        -- Streaming Loop (30Hz = ~33ms)
        forever $ do
            state <- readTVarIO stateVar
            let json = encodeSystemState state
            sendTextData conn json
            threadDelay 33000 -- 33ms
