{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Control.WebUI (runWebUI) where

import Control.Concurrent (threadDelay)
import Safety.Thread (forkSafetyThread, ThreadShutdownAction(..))
import Control.Concurrent.STM
import Control.Monad (forever, void)
import Data.Aeson (encode, decode, FromJSON(..), withObject, (.:), (.:?))
import Data.FileEmbed (embedFile)
import Network.HTTP.Types (status200, status401, status302)
import Network.Wai
import Network.Wai.Handler.Warp (defaultSettings, setPort, setHost, setServerName)
import Network.Wai.Handler.WarpTLS (runTLS, tlsSettings)
import Network.Wai.Handler.WebSockets (websocketsOr)
import qualified Network.WebSockets as WS
import Network.WebSockets (ServerApp, acceptRequest, rejectRequest, sendTextData, defaultConnectionOptions, pendingRequest, withPingThread)
import Data.ByteString.Lazy (fromStrict)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import Data.Bits (xor, (.|.))
import Data.List (foldl')
import System.Process (callCommand)
import System.Directory (doesFileExist)
import Data.Time.Clock (getCurrentTime, addUTCTime, UTCTime)
import qualified Data.Map.Strict as Map

import Control.WebUI.Types (encodeWebPayload)
import Data.Types (SystemState(..), AuditEvent(..), Severity(..))
import Data.Time.HighRes (getMonotonicTimeNS)
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.ByteArray.Encoding (convertFromBase, Base(Base64))
import Crypto.Hash (hash, SHA256(..), Digest)
import UI.Presentation (shouldTriggerAudioAlert)
import Safety.Token (generateToken)
import Safety.Result (SafetyResult(..))
import Data.I18n (Translations, translate)
import qualified Data.Text as T

-- Provisioned credential map (username -> hashed password)
credentialStore :: Map.Map B.ByteString (Digest SHA256)
credentialStore = Map.fromList
    [ ("admin", hash (BC.pack "password") :: Digest SHA256)
    , ("operator", hash (BC.pack "password") :: Digest SHA256)
    ]


indexHtml :: B.ByteString
indexHtml = $(embedFile "app/Control/WebUI/assets/index.html")

dashboardHtml :: B.ByteString
dashboardHtml = $(embedFile "app/Control/WebUI/assets/dashboard.html")

dashboardJson :: B.ByteString
dashboardJson = $(embedFile "app/Control/WebUI/assets/dashboard.json")

a11yCss :: B.ByteString
a11yCss = $(embedFile "app/Control/WebUI/assets/a11y.css")

-- A session store
type SessionStore = TVar (Map.Map B.ByteString UTCTime)

runWebUI :: TVar SystemState -> Translations -> IO ()
runWebUI stateVar translations = do
    -- Ensure certificates exist
    certExists <- doesFileExist "cert.pem"
    keyExists <- doesFileExist "key.pem"
    if not (certExists && keyExists)
       then do
           putStrLn "Generating self-signed TLS certificates..."
           callCommand "openssl req -x509 -newkey rsa:2048 -keyout key.pem -out cert.pem -sha256 -days 365 -nodes -subj '/CN=localhost' 2>/dev/null"
       else putStrLn "TLS certificates found."

    certExists' <- doesFileExist "cert.pem"
    keyExists' <- doesFileExist "key.pem"
    if not (certExists' && keyExists')
       then error "Failed to generate or find TLS certificates. Aborting."
       else do
           store <- newTVarIO Map.empty
           
           putStrLn "Starting Web UI on https://127.0.0.1:8443"
           let settings = setServerName "" $ setPort 8443 $ setHost "127.0.0.1" defaultSettings
           let tSettings = tlsSettings "cert.pem" "key.pem"
           
           runTLS tSettings settings $ websocketsOr defaultConnectionOptions (wsApp store stateVar translations) (httpApp store stateVar translations)

httpApp :: SessionStore -> TVar SystemState -> Translations -> Application
httpApp store stateVar translations req respond = do
    let headers = requestHeaders req
        cookieHeader = lookup "Cookie" headers
        authHeader = lookup "Authorization" headers
        path = pathInfo req
        
    isValid <- checkSession store cookieHeader
    
    if isValid
       then case path of
            ["dashboard.html"] -> respond $ responseLBS status200
                [ ("Content-Type", "text/html")
                , ("X-Frame-Options", "DENY")
                , ("X-Content-Type-Options", "nosniff")
                , ("Content-Security-Policy", "default-src 'self'; connect-src 'self' wss:; script-src 'self' 'sha256-aP6rrcIxmSdZejB774XHCC0sACsHgn5QDDBDIQGx5n8='; style-src 'self' 'sha256-c+Klqm2arsVIfV+hOqaQ785BaCyaTZeUa93cO9/7yMs='")
                , ("Strict-Transport-Security", "max-age=31536000; includeSubDomains")
                ]
                (fromStrict dashboardHtml)
            ["dashboard.json"] -> respond $ responseLBS status200
                [ ("Content-Type", "application/json")
                ]
                (fromStrict dashboardJson)
            ["a11y.css"] -> respond $ responseLBS status200
                [ ("Content-Type", "text/css")
                ]
                (fromStrict a11yCss)
            _ -> respond $ responseLBS status200
                [ ("Content-Type", "text/html")
                , ("X-Frame-Options", "DENY")
                , ("X-Content-Type-Options", "nosniff")
                , ("Content-Security-Policy", "default-src 'self'; connect-src 'self' wss:; script-src 'self' 'sha256-EkErJL/uPo1ZkRym5WlmM6nOSvBTWkEmSaQeZS7WlZw='; style-src 'self' 'sha256-V7fbnrbvmt/RAZmtcdDEi0hpCJzLlyMgrKK0ibIWTKY='")
                , ("Strict-Transport-Security", "max-age=31536000; includeSubDomains")
                ]
                (fromStrict indexHtml)
       else case authHeader of
            Just auth | "Basic " `B.isPrefixOf` auth -> do
                let creds = B.drop 6 auth
                case convertFromBase Base64 creds :: Either String B.ByteString of
                    Right decoded -> do
                        let (user, passWithColon) = B.break (== 58) decoded -- 58 is ':'
                            pass = B.drop 1 passWithColon
                        let passHash = hash pass :: Digest SHA256
                        
                        let isValidUser = case Map.lookup user credentialStore of
                                Just expectedHash -> expectedHash == passHash
                                Nothing -> False
                        
                        if isValidUser
                            then do
                                tokenRes <- generateToken
                                case tokenRes of
                                    Safe token -> do
                                        now <- getCurrentTime
                                        let expiry = addUTCTime 1800 now -- 30 minutes
                                        atomically $ modifyTVar' store (Map.insert token expiry)
                                        
                                        state <- readTVarIO stateVar
                                        let lang = T.pack (activeLanguage state)
                                        let successMsgTemplate = T.unpack $ translate translations lang "AuthSuccess" "Login successful for user %s"
                                        let successMsg = replaceOne "%s" (BC.unpack user) successMsgTemplate
                                        logAuthEvent stateVar successMsg True
                                        
                                        respond $ responseLBS status302
                                            [ ("Set-Cookie", "session=" <> token <> "; HttpOnly; Secure; SameSite=Strict; Path=/")
                                            , ("Location", "/")
                                            ]
                                            ""
                                    Unsafe msg -> do
                                        state <- readTVarIO stateVar
                                        let lang = T.pack (activeLanguage state)
                                        logAuthEvent stateVar ("Token generation failed: " ++ msg) False
                                        -- It's not a real-time safety loop, we just reject the login.
                                        respond $ responseLBS status500 [] "Internal Server Error"
                            else do
                                state <- readTVarIO stateVar
                                let lang = T.pack (activeLanguage state)
                                let failedMsg = T.unpack $ translate translations lang "AuthFailed" "Login failed"
                                logAuthEvent stateVar failedMsg False
                                respond401 respond
                    Left _ -> do
                        state <- readTVarIO stateVar
                        let lang = T.pack (activeLanguage state)
                        let invalidMsg = T.unpack $ translate translations lang "AuthFailedInvalid" "Login failed: invalid encoding"
                        logAuthEvent stateVar invalidMsg False
                        respond401 respond
            _ -> respond401 respond

replaceOne :: String -> String -> String -> String
replaceOne needle replacement haystack =
    case span (not . startsWith needle) (tails haystack) of
        (_, []) -> haystack
        (before, _:_) -> concatMap head before ++ replacement ++ drop (length needle) (concat (take 1 (drop (length before) (tails haystack))))
  where
    startsWith n h = take (length n) h == n
    tails [] = []
    tails s@(_:xs) = s : tails xs
    head (x:_) = [x]
    head [] = []

respond401 :: (Response -> IO ResponseReceived) -> IO ResponseReceived
respond401 respond = respond $ responseLBS status401
    [ ("WWW-Authenticate", "Basic realm=\"Lambda-Wave\"") ]
    "Unauthorized"

logAuthEvent :: TVar SystemState -> String -> Bool -> IO ()
logAuthEvent stateVar msg success = do
    now <- getMonotonicTimeNS
    state <- readTVarIO stateVar
    let q = auditQueue state
    let sev = if success then Info else Warning
    atomically $ do
        full <- isFullTBQueue q
        if not full then writeTBQueue q (AuditEvent now sev "Auth" msg) else return ()

extractSessionCookie :: B.ByteString -> Maybe B.ByteString
extractSessionCookie cookieHeader =
    let cookies = BC.split ';' cookieHeader
        sessionCookies = filter ("session=" `BC.isPrefixOf`) (map (BC.dropWhile (== ' ')) cookies)
    in case sessionCookies of
        (c:_) -> Just (B.drop 8 c)
        _     -> Nothing

checkSession :: SessionStore -> Maybe B.ByteString -> IO Bool
checkSession store cookieHeader = case cookieHeader of
    Nothing -> return False
    Just hdr -> case extractSessionCookie hdr of
        Nothing -> return False
        Just token -> do
            now <- getCurrentTime
            atomically $ do
                m <- readTVar store
                case Map.lookup token m of
                    Just expiry | now < expiry -> do
                        -- extend session
                        modifyTVar' store (Map.insert token (addUTCTime 1800 now))
                        return True
                    _ -> return False

data ClientRequest = ClientRequest
    { reqLang :: Maybe String
    , reqPause :: Maybe Bool
    }

instance FromJSON ClientRequest where
    parseJSON = withObject "ClientRequest" $ \v -> ClientRequest
        <$> v .:? "lang"
        <*> v .:? "pause"

wsApp :: SessionStore -> TVar SystemState -> Translations -> ServerApp
wsApp store stateVar translations pending = do
    let headers = WS.requestHeaders (pendingRequest pending)
        origin = lookup "Origin" headers
        cookie = lookup "Cookie" headers

    isValid <- checkSession store cookie
    
    if (origin == Just "https://127.0.0.1:8443" || origin == Just "https://localhost:8443")
       && isValid
        then do
            conn <- acceptRequest pending
            withPingThread conn 10 (return ()) $ do
                _ <- forkSafetyThread (LogOnly putStrLn) "WebUIStream" $ do
                    prevStateRef <- newIORef BeamOff
                    forever $ do
                        state <- readTVarIO stateVar
                        prevState <- readIORef prevStateRef
                        
                        let beep = shouldTriggerAudioAlert (audioAlertEnabled state) prevState (beamState state)
                        writeIORef prevStateRef (beamState state)
                        
                        sendTextData conn (encodeWebPayload state beep)
                        threadDelay 33000
                forever $ do
                    msg <- WS.receiveData conn
                    let parsed = decode msg :: Maybe ClientRequest
                    case parsed of
                        Just req -> do
                            case reqLang req of
                                Just l -> atomically $ modifyTVar' stateVar (\s -> s { activeLanguage = l })
                                Nothing -> return ()
                            case reqPause req of
                                Just p -> do
                                    now <- getMonotonicTimeNS
                                    state <- readTVarIO stateVar
                                    let lang = T.pack (activeLanguage state)
                                    let msgKey = if p then "AuditOperatorPause" else "AuditOperatorResume"
                                    let defaultMsg = if p then "Operator paused trace" else "Operator resumed trace"
                                    let localizedMsg = T.unpack $ translate translations lang msgKey (T.pack defaultMsg)
                                    let q = auditQueue state
                                    atomically $ do
                                        full <- isFullTBQueue q
                                        if not full then writeTBQueue q (AuditEvent now Info "Operator" localizedMsg) else return ()
                                Nothing -> return ()
                        Nothing -> return ()
        else rejectRequest pending "Untrusted Origin or Invalid Token"
