{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Control.Exception (catch, SomeException)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import System.Process (readProcess)
import System.Exit (exitFailure)
import Data.List (isInfixOf)

-- Helper to calculate sha256 via openssl and base64 encode it
sha256Base64 :: B.ByteString -> IO String
sha256Base64 bs = do
    -- use echo -n, openssl dgst -sha256 -binary, and base64
    let input = BC.unpack bs
    out <- readProcess "sh" ["-c", "openssl dgst -sha256 -binary | base64"] input
    -- strip newline
    return $ takeWhile (/= '\n') out

extractTag :: B.ByteString -> B.ByteString -> B.ByteString -> Maybe B.ByteString
extractTag start end bs = 
    let (_, rest) = B.breakSubstring start bs
    in if B.null rest 
       then Nothing
       else let content = B.drop (B.length start) rest
                (inner, _) = B.breakSubstring end content
            in Just inner

main :: IO ()
main = do
    putStrLn "Reading index.html..."
    content <- B.readFile "app/Control/WebUI/assets/index.html" `catch` \e -> do
        print (e :: SomeException)
        _ <- exitFailure
        return B.empty

    styleHash <- case extractTag "<style>" "</style>" content of
        Just styleContent -> do
            h <- sha256Base64 styleContent
            putStrLn $ "style-src: 'sha256-" ++ h ++ "'"
            return h
        Nothing -> do
            putStrLn "No <style> block found."
            return ""

    scriptHash <- case extractTag "<script>" "</script>" content of
        Just scriptContent -> do
            h <- sha256Base64 scriptContent
            putStrLn $ "script-src: 'sha256-" ++ h ++ "'"
            return h
        Nothing -> do
            putStrLn "No <script> block found."
            return ""

    if null styleHash || null scriptHash then
        putStrLn "Failed to extract hashes."
    else do
        putStrLn "Updating WebUI.hs..."
        webui <- readFile "app/Control/WebUI.hs"
        let newCsp = "default-src 'self'; connect-src 'self' ws: wss:; script-src 'self' 'sha256-" ++ scriptHash ++ "'; style-src 'self' 'sha256-" ++ styleHash ++ "'"
        let replaceCsp line
              | "(\"Content-Security-Policy\"" `isInfixOf` line = "        (\"Content-Security-Policy\", \"" ++ newCsp ++ "\")"
              | otherwise = line
        let webuiUpdated = unlines $ map replaceCsp (lines webui)
        writeFile "app/Control/WebUI.hs" webuiUpdated
        putStrLn $ "Updated CSP with hashes: script=" ++ scriptHash ++ ", style=" ++ styleHash

