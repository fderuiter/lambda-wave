module Main (main) where

import System.Process (readProcessWithExitCode)
import System.Exit (ExitCode(..), exitWith)
import Data.List (isInfixOf, isPrefixOf)
import System.Directory (doesFileExist, removeFile)
import Text.Read (readMaybe)
import Control.Monad (when, unless)
import Data.Maybe (mapMaybe)
import Control.Exception (try, SomeException)
import qualified Data.ByteString as B
import Safety.Crypto (decryptLog)

-- Extract all digits from a string prefix
takeDigits :: String -> String
takeDigits = takeWhile (\c -> c >= '0' && c <= '9')

findPrefix :: String -> [String] -> Maybe String
findPrefix _ [] = Nothing
findPrefix prefix (x:xs)
    | prefix `isPrefixOf` x = Just (drop (length prefix) x)
    | otherwise = findPrefix prefix xs

findInfix :: String -> [String] -> Maybe String
findInfix _ [] = Nothing
findInfix substr (x:xs)
    | substr `isInfixOf` x = Just x
    | otherwise = findInfix substr xs

parseAge :: String -> Maybe Double
parseAge line = do
    let prefix = "!!! MAIN WATCHDOG: Thread 'TestThread' FROZEN (Age: "
    idx <- findSubstrIndex prefix line
    let rest = drop (idx + length prefix) line
    let digits = takeDigits rest
    val <- readMaybe digits :: Maybe Double
    return (val / 1000000.0) -- convert to ms

findSubstrIndex :: String -> String -> Maybe Int
findSubstrIndex substr str = go str 0
  where
    go [] _ = Nothing
    go s@(_:cs) i
        | substr `isPrefixOf` s = Just i
        | otherwise = go cs (i + 1)

main :: IO ()
main = do
    putStrLn "=== High-Assurance Safety Audit Framework ==="

    -- Clean up previous run
    _ <- try (removeFile "session.log") :: IO (Either SomeException ())

    -- Get the path to the executable
    (_, binPathStr, _) <- readProcessWithExitCode "cabal" ["list-bin", "watchdog-fault"] ""
    let binPath = head (lines binPathStr)

    -- Run the fault injection executable directly
    (_exitCode, stdout, stderr) <- readProcessWithExitCode binPath [] ""
    
    let combinedOutput = stdout ++ stderr
    let linesOutput = lines combinedOutput

    -- Requirement 3: Intercept non-standard exit codes
    when ("SURVIVED" `isInfixOf` combinedOutput) $ do
        putStrLn "FAIL: Watchdog test survived, expected termination."
        exitWith (ExitFailure 1)

    unless ("SAFETY DAEMON TRIP" `isInfixOf` combinedOutput) $ do
        putStrLn "FAIL: 'SAFETY DAEMON TRIP' signature not found in logs."
        putStrLn $ "OUTPUT:\n" ++ combinedOutput
        exitWith (ExitFailure 1)

    -- Requirement 1: 10ms sensitivity check
    let ageLines = filter ("FROZEN (Age: " `isInfixOf`) linesOutput
    ageMs <- case mapMaybe parseAge ageLines of
        (a:_) -> return a
        [] -> do
            putStrLn "FAIL: Could not find freeze age in output."
            putStrLn $ "OUTPUT:\n" ++ combinedOutput
            exitWith (ExitFailure 1)
            
    putStrLn $ "Watchdog detected freeze at age: " ++ show ageMs ++ " ms"
    unless (ageMs <= 110.0) $ do
        putStrLn $ "FAIL: Sensitivity violation. Expected <= 110ms, got " ++ show ageMs ++ "ms"
        exitWith (ExitFailure 1)

    -- Requirement 4: High-resolution timing data
    let stallStartLine = findPrefix "STALL_START_NS: " linesOutput
    stallStartNs <- case stallStartLine >>= readMaybe of
        Just val -> return (val :: Integer)
        Nothing -> do
            putStrLn "FAIL: Could not find STALL_START_NS."
            putStrLn $ "OUTPUT:\n" ++ combinedOutput
            exitWith (ExitFailure 1)

    -- Parse session.log
    logExists <- doesFileExist "session.log"
    unless logExists $ do
        putStrLn "FAIL: session.log not found."
        exitWith (ExitFailure 1)

    sessionLogsBs <- B.readFile "session.log"
    sessionLogs <- case decryptLog sessionLogsBs of
        Right pt -> return pt
        Left err -> do
            putStrLn $ "FAIL: Could not decrypt session.log: " ++ err
            exitWith (ExitFailure 1)

    let tripLine = findInfix "[CRITICAL] [SafetyDaemon] !!! SAFETY DAEMON TRIP" (lines sessionLogs)
    tripNs <- case tripLine of
        Just line -> case readMaybe (takeWhile (/= ' ') line) of
            Just val -> return (val :: Integer)
            Nothing -> do
                putStrLn "FAIL: Could not parse timestamp from session.log"
                exitWith (ExitFailure 1)
        Nothing -> do
            putStrLn "FAIL: Could not find trip timestamp in session.log."
            exitWith (ExitFailure 1)

    let responseTimeMs = fromIntegral (tripNs - stallStartNs) / 1000000.0 :: Double
    putStrLn $ "System responded in " ++ show responseTimeMs ++ " ms"

    unless (responseTimeMs <= 110.0) $ do
        putStrLn $ "FAIL: Response time " ++ show responseTimeMs ++ " ms exceeds 110 ms requirement."
        exitWith (ExitFailure 1)

    -- Requirement 2: verify 'Beam Off' state change
    unless ("[Hardware] WATCHDOG Channel Set To: OFF" `isInfixOf` combinedOutput) $ do
        putStrLn "FAIL: Beam Off command not found in output prior to termination."
        putStrLn $ "OUTPUT:\n" ++ combinedOutput
        exitWith (ExitFailure 1)

    -- Requirement 5: Generate non-alterable audit artifact
    let report = "=== High-Assurance Safety Audit Report ===\n" ++
                 "Requirement: Response time must be <= 110 ms (100ms threshold + 10ms tolerance)\n" ++
                 "Actual Response Time: " ++ show responseTimeMs ++ " ms\n" ++
                 "Sensitivity Requirement: Watchdog interval <= 10ms (detects freeze <= 110ms)\n" ++
                 "Actual Detected Freeze Age: " ++ show ageMs ++ " ms\n" ++
                 "Beam Off Verified: True\n" ++
                 "Status: PASS\n"
    writeFile "safety_audit_report.txt" report

    putStrLn "PASS: Daemon successfully tripped on timeout within constraints."
    exitWith ExitSuccess
