{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Control.WebUI (runWebUI) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.STM
import Control.Monad (forever)
import Data.Aeson (encode)
import Data.FileEmbed (embedFile)
import Network.HTTP.Types (status200)
import Network.Wai
import Network.Wai.Handler.Warp (run)
import Network.Wai.Handler.WebSockets (websocketsOr)
import Network.WebSockets (ServerApp, acceptRequest, sendTextData, defaultConnectionOptions)
import Data.ByteString.Lazy (fromStrict)
import qualified Data.ByteString as B

import Control.WebUI.Types () -- Import instances
import Data.Types (SystemState)

indexHtml :: B.ByteString
indexHtml = $(embedFile "app/Control/WebUI/assets/index.html")

runWebUI :: TVar SystemState -> IO ()
runWebUI stateVar = do
    putStrLn "Starting Web UI on http://localhost:8080"
    run 8080 $ websocketsOr defaultConnectionOptions (wsApp stateVar) httpApp

httpApp :: Application
httpApp _ respond = respond $
    responseLBS status200 [("Content-Type", "text/html")] (fromStrict indexHtml)

wsApp :: TVar SystemState -> ServerApp
wsApp stateVar pending = do
    conn <- acceptRequest pending
    -- Simple loop: push state every 33ms
    forever $ do
        state <- readTVarIO stateVar
        sendTextData conn (encode state)
        threadDelay 33000 -- ~30Hz
