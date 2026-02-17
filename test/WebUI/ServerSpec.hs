{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : WebUISpec
Description : Verifies that the WebUI WebSocket server is reachable and serving valid JSON.
-}

import Network.WebSockets (runClient, receiveData)
import Data.Aeson (decode, Value(..))
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Control.Exception (catch, SomeException)
import System.Exit (exitFailure, exitSuccess)

main :: IO ()
main = do
    putStrLn "Connecting to WebUI WebSocket..."
    result <- (runClient "127.0.0.1" 8080 "/ws" app) `catch` handleErr
    putStrLn "Test Passed!"
    exitSuccess

app :: Network.WebSockets.ClientApp ()
app conn = do
    putStrLn "Connected!"
    msg <- receiveData conn
    putStrLn $ "Received: " ++ show msg

    -- Verify JSON
    case decode msg :: Maybe Value of
        Just _ -> putStrLn "Valid JSON received."
        Nothing -> do
            putStrLn "Invalid JSON!"
            exitFailure

handleErr :: SomeException -> IO ()
handleErr e = do
    putStrLn $ "Connection Failed: " ++ show e
    putStrLn "Is the server running?"
    exitFailure
