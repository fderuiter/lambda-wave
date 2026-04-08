{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Control.WebUI (runWebUI) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.STM
import Control.Monad (forever)
import Data.Aeson (encode)
import Data.FileEmbed (embedFile)
import Network.HTTP.Types (status200, hCookie)
import Network.Wai
import Network.Wai.Handler.Warp (runSettings, defaultSettings, setPort, setHost, setServerName)
import Network.Wai.Handler.WebSockets (websocketsOr)
import qualified Network.WebSockets as WS
import Network.WebSockets (ServerApp, acceptRequest, rejectRequest, sendTextData, defaultConnectionOptions, pendingRequest, withPingThread)
import Data.ByteString.Lazy (fromStrict)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.Text as T
import Text.Printf (printf)
import System.IO (withBinaryFile, IOMode(ReadMode))

import Control.WebUI.Types () -- Import instances
import Data.Types (SystemState)

indexHtml :: B.ByteString
indexHtml = $(embedFile "app/Control/WebUI/assets/index.html")

-- | Generate a cryptographically secure random token (32 hex chars)
generateToken :: IO String
generateToken = withBinaryFile "/dev/urandom" ReadMode $ \h -> do
    bytes <- B.hGet h 16
    return $ concatMap (printf "%02x") (B.unpack bytes)

runWebUI :: TVar SystemState -> IO ()
runWebUI stateVar = do
    token <- generateToken
    putStrLn "Starting Web UI on http://127.0.0.1:8080"
    let settings = setServerName "" $ setPort 8080 $ setHost "127.0.0.1" defaultSettings
    runSettings settings $ websocketsOr defaultConnectionOptions (wsApp token stateVar) (httpApp token)

httpApp :: String -> Application
httpApp token _ respond = respond $
    responseLBS status200
        [ ("Content-Type", "text/html")
        , ("Set-Cookie", BC.pack $ "auth_token=" ++ token ++ "; HttpOnly; SameSite=Strict; Path=/")
        , ("X-Frame-Options", "DENY")
        , ("X-Content-Type-Options", "nosniff")
        , ("Content-Security-Policy", "default-src 'self'; connect-src 'self' ws: wss:; script-src 'self' 'sha256-GUYznWupE5ohSMN7U8nkzx+PLxw2mKz4g7z5VcHzG+4='; style-src 'self' 'sha256-2m8iwBnJLOqlqV6JkqY7KnQSQKpyjXb6x0oqx/s9IeE='")
        , ("Strict-Transport-Security", "max-age=31536000; includeSubDomains")
        , ("Cache-Control", "no-store, no-cache, must-revalidate, max-age=0")
        , ("Pragma", "no-cache")
        , ("Referrer-Policy", "no-referrer")
        ]
        (fromStrict indexHtml)

wsApp :: String -> TVar SystemState -> ServerApp
wsApp token stateVar pending = do
    let headers = WS.requestHeaders (pendingRequest pending)
        origin = lookup "Origin" headers
        cookie = lookup hCookie headers

        -- Simple cookie parser to extract auth_token
        extractToken :: B.ByteString -> Maybe String
        extractToken c =
            let cookies = map (T.breakOn "=" . T.strip) $ T.splitOn ";" (T.pack $ BC.unpack c)
                match = lookup "auth_token" cookies
            in fmap (T.unpack . T.drop 1) match

        clientToken = cookie >>= extractToken
        isAuthorized = clientToken == Just token
        isOriginSafe = origin == Just "http://127.0.0.1:8080" || origin == Just "http://localhost:8080"

    if isOriginSafe && isAuthorized
        then do
            conn <- acceptRequest pending
            -- Use a ping thread to detect dead connections and prevent socket leaks
            withPingThread conn 10 (return ()) $ do
                -- Simple loop: push state every 33ms
                forever $ do
                    state <- readTVarIO stateVar
                    sendTextData conn (encode state)
                    threadDelay 33000 -- ~30Hz
        else rejectRequest pending "Unauthorized"
