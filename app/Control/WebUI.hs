{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Control.WebUI (runWebUI) where

import Control.Concurrent (threadDelay, forkIO)
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
import System.Posix.IO.ByteString (fdRead)
import Data.Bits (xor, (.|.))
import Data.List (foldl')
import System.Posix.IO (openFd, closeFd, OpenMode(ReadOnly), defaultFileFlags, OpenFileFlags(..))
import System.Posix.Files (getFdStatus, isCharacterDevice)
import Control.Exception (bracket)
import Text.Printf (printf)

import Control.WebUI.Types () -- Import instances
import Data.Types (SystemState)

indexHtml :: B.ByteString
indexHtml = $(embedFile "app/Control/WebUI/assets/index.html")

runWebUI :: TVar SystemState -> IO ()
runWebUI stateVar = do
    tokenBytes <- bracket
#if MIN_VERSION_unix(2,8,0)
                    (openFd "/dev/urandom" ReadOnly defaultFileFlags{creat=Nothing})
#else
                    (openFd "/dev/urandom" ReadOnly Nothing defaultFileFlags)
#endif
                    closeFd $ \fd -> do
                        stat <- getFdStatus fd
                        if not (isCharacterDevice stat)
                            then error "Security Violation - /dev/urandom is not a character device"
                            else fdRead fd 16
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
        , ("Content-Security-Policy", "default-src 'self'; connect-src 'self' ws: wss:; script-src 'self' 'sha256-Yx2ngBSshvkQwqGI5RgkcOB/07Zs/XeH15RZ0L+thHg='; style-src 'self' 'sha256-FcoJ8qmZ8gDC3Xt3m7E1qw2i4QirXs2wG1cbnwmFSyM='")
        , ("Strict-Transport-Security", "max-age=31536000; includeSubDomains")
        , ("Cache-Control", "no-store, no-cache, must-revalidate, max-age=0")
        , ("Pragma", "no-cache")
        , ("Referrer-Policy", "no-referrer")
        , ("Set-Cookie", "session=" <> token <> "; HttpOnly; Secure; SameSite=Strict; Path=/")
        ]
        (fromStrict indexHtml)

constantTimeEq :: B.ByteString -> B.ByteString -> Bool
constantTimeEq a b
    | B.length a /= B.length b = False
    | otherwise = foldl' (\acc (x, y) -> acc .|. (x `xor` y)) 0 (B.zip a b) == 0

extractSessionCookie :: B.ByteString -> Maybe B.ByteString
extractSessionCookie cookieHeader =
    let cookies = BC.split ';' cookieHeader
        sessionCookies = filter ("session=" `BC.isPrefixOf`) (map (BC.dropWhile (== ' ')) cookies)
    in case sessionCookies of
        (c:_) -> Just (B.drop 8 c)
        _     -> Nothing

wsApp :: B.ByteString -> TVar SystemState -> ServerApp
wsApp token stateVar pending = do
    let headers = WS.requestHeaders (pendingRequest pending)
        origin = lookup "Origin" headers
        cookie = lookup "Cookie" headers

        isTokenValid = case cookie of
            Nothing -> False
            Just cookieHdr -> case extractSessionCookie cookieHdr of
                Nothing -> False
                Just val -> constantTimeEq val token

    if (origin == Just "http://127.0.0.1:8080" || origin == Just "http://localhost:8080")
       && isTokenValid
        then do
            conn <- acceptRequest pending
            -- Use a ping thread to detect dead connections and prevent socket leaks
            withPingThread conn 10 (return ()) $ do
                -- Simple loop: push state every 33ms
                _ <- forkIO $ forever $ do
                    msg <- WS.receiveData conn :: IO BC.ByteString
                    case msg of
                        "TOGGLE_SYNC" -> atomically $ modifyTVar' stateVar $ \s -> s { cameraSyncEnabled = not (cameraSyncEnabled s) }
                        _ -> return ()
                


                forever $ do
                    state <- readTVarIO stateVar
                    sendTextData conn (encode state)
                    threadDelay 33000 -- ~30Hz
        else rejectRequest pending "Untrusted Origin or Invalid Token"
