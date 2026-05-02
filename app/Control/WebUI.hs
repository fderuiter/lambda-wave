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
import Network.Wai.Handler.Warp (runSettings, defaultSettings, setPort, setHost, setServerName)
import Network.Wai.Handler.WebSockets (websocketsOr)
import qualified Network.WebSockets as WS
import Network.WebSockets (ServerApp, acceptRequest, rejectRequest, sendTextData, defaultConnectionOptions, pendingRequest, withPingThread)
import Data.ByteString.Lazy (fromStrict)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import System.IO (withBinaryFile, IOMode(ReadMode))
import Text.Printf (printf)

import Control.WebUI.Types () -- Import instances
import Data.Types (SystemState)

indexHtml :: B.ByteString
indexHtml = $(embedFile "app/Control/WebUI/assets/index.html")

runWebUI :: TVar SystemState -> IO ()
runWebUI stateVar = do
    tokenBytes <- withBinaryFile "/dev/urandom" ReadMode (`B.hGet` 16)
    let token = BC.pack (concatMap (printf "%02x") (B.unpack tokenBytes))
    putStrLn "Starting Web UI on http://127.0.0.1:8080"
    let settings = setServerName "" $ setPort 8080 $ setHost "127.0.0.1" defaultSettings
    runSettings settings $ websocketsOr defaultConnectionOptions (wsApp token stateVar) (httpApp token)

httpApp :: B.ByteString -> Application
httpApp token _ respond = respond $
    responseLBS status200
        [ ("Content-Type", "text/html")
        , ("X-Frame-Options", "DENY")
        , ("X-Content-Type-Options", "nosniff")
        , ("Content-Security-Policy", "default-src 'self'; connect-src 'self' ws: wss:; script-src 'self' 'sha256-Rd75yJnFRi2Z8uD+em684rhUZzDp2DJ4YUUO2/X+Vtc='; style-src 'self' 'sha256-7do0w+3F6Aj7dTDfYAP+QAJ9GzpeZOdGxCRDfmUXM9c='")
        , ("Strict-Transport-Security", "max-age=31536000; includeSubDomains")
        , ("Cache-Control", "no-store, no-cache, must-revalidate, max-age=0")
        , ("Pragma", "no-cache")
        , ("Referrer-Policy", "no-referrer")
        , ("Set-Cookie", "session=" <> token <> "; HttpOnly; Secure; SameSite=Strict; Path=/")
        ]
        (fromStrict indexHtml)

wsApp :: B.ByteString -> TVar SystemState -> ServerApp
wsApp token stateVar pending = do
    let headers = WS.requestHeaders (pendingRequest pending)
        origin = lookup "Origin" headers
        cookie = lookup "Cookie" headers
        expectedCookie = "session=" <> token
    if (origin == Just "http://127.0.0.1:8080" || origin == Just "http://localhost:8080")
       && maybe False (expectedCookie `BC.isInfixOf`) cookie
        then do
            conn <- acceptRequest pending
            -- Use a ping thread to detect dead connections and prevent socket leaks
            withPingThread conn 10 (return ()) $ do
                -- Simple loop: push state every 33ms
                forever $ do
                    state <- readTVarIO stateVar
                    sendTextData conn (encode state)
                    threadDelay 33000 -- ~30Hz
        else rejectRequest pending "Untrusted Origin or Invalid Token"
