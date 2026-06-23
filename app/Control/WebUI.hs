{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Control.WebUI (runWebUI) where

import Control.Concurrent (threadDelay, forkIO)
import Control.Concurrent.STM
import Control.Monad (forever, void)
import Data.Aeson (encode, decode, FromJSON(..), withObject, (.:))
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
import System.Posix.IO.ByteString (fdRead)
import Data.Bits (xor, (.|.))
import Data.List (foldl')
import System.Posix.IO (openFd, closeFd, OpenMode(ReadOnly), defaultFileFlags, OpenFileFlags(..))
import System.Posix.Files (getFdStatus, isCharacterDevice)
import Control.Exception (bracket)
import Text.Printf (printf)
import System.Process (callCommand)
import System.Directory (doesFileExist)
import Data.Time.Clock (getCurrentTime, addUTCTime, UTCTime)
import qualified Data.Map.Strict as Map

import Control.WebUI.Types ()
import Data.Types (SystemState(..), AuditEvent(..), Severity(..))
import Data.Time.HighRes (getMonotonicTimeNS)
import Data.ByteArray.Encoding (convertFromBase, Base(Base64))
import Crypto.Hash (hash, SHA256(..), Digest)

-- Provisioned credential map (username -> hashed password)
credentialStore :: Map.Map B.ByteString (Digest SHA256)
credentialStore = Map.fromList
    [ ("admin", hash (BC.pack "password") :: Digest SHA256)
    , ("operator", hash (BC.pack "password") :: Digest SHA256)
    ]


indexHtml :: B.ByteString
indexHtml = $(embedFile "app/Control/WebUI/assets/index.html")

-- A session store
type SessionStore = TVar (Map.Map B.ByteString UTCTime)

runWebUI :: TVar SystemState -> IO ()
runWebUI stateVar = do
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
           
           runTLS tSettings settings $ websocketsOr defaultConnectionOptions (wsApp store stateVar) (httpApp store stateVar)

generateToken :: IO B.ByteString
generateToken = do
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
    return $ BC.pack (concatMap (printf "%02x") (B.unpack tokenBytes))

httpApp :: SessionStore -> TVar SystemState -> Application
httpApp store stateVar req respond = do
    let headers = requestHeaders req
        cookieHeader = lookup "Cookie" headers
        authHeader = lookup "Authorization" headers
        
    isValid <- checkSession store cookieHeader
    
    if isValid
       then respond $ responseLBS status200
            [ ("Content-Type", "text/html")
            , ("X-Frame-Options", "DENY")
            , ("X-Content-Type-Options", "nosniff")
            , ("Content-Security-Policy", "default-src 'self'; connect-src 'self' wss:; script-src 'self' 'sha256-Yx2ngBSshvkQwqGI5RgkcOB/07Zs/XeH15RZ0L+thHg='; style-src 'self' 'sha256-FcoJ8qmZ8gDC3Xt3m7E1qw2i4QirXs2wG1cbnwmFSyM='")
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
                                token <- generateToken
                                now <- getCurrentTime
                                let expiry = addUTCTime 1800 now -- 30 minutes
                                atomically $ modifyTVar' store (Map.insert token expiry)
                                
                                logAuthEvent stateVar ("Login successful for user " ++ BC.unpack user) True
                                
                                respond $ responseLBS status302
                                    [ ("Set-Cookie", "session=" <> token <> "; HttpOnly; Secure; SameSite=Strict; Path=/")
                                    , ("Location", "/")
                                    ]
                                    ""
                            else do
                                logAuthEvent stateVar "Login failed" False
                                respond401 respond
                    Left _ -> do
                        logAuthEvent stateVar "Login failed: invalid encoding" False
                        respond401 respond
            _ -> respond401 respond

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

data LangRequest = LangRequest { reqLang :: String }

instance FromJSON LangRequest where
    parseJSON = withObject "LangRequest" $ \v -> LangRequest <$> v .: "lang"

wsApp :: SessionStore -> TVar SystemState -> ServerApp
wsApp store stateVar pending = do
    let headers = WS.requestHeaders (pendingRequest pending)
        origin = lookup "Origin" headers
        cookie = lookup "Cookie" headers

    isValid <- checkSession store cookie
    
    if (origin == Just "https://127.0.0.1:8443" || origin == Just "https://localhost:8443")
       && isValid
        then do
            conn <- acceptRequest pending
            withPingThread conn 10 (return ()) $ do
                _ <- forkIO $ forever $ do
                    state <- readTVarIO stateVar
                    sendTextData conn (encode state)
                    threadDelay 33000
                forever $ do
                    msg <- WS.receiveData conn
                    let parsed = decode msg :: Maybe LangRequest
                    case parsed of
                        Just lr -> atomically $ modifyTVar' stateVar (\s -> s { activeLanguage = reqLang lr })
                        Nothing -> return ()
        else rejectRequest pending "Untrusted Origin or Invalid Token"
