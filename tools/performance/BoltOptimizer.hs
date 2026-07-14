{-# LANGUAGE LambdaCase #-}
module Main (main) where

import System.Environment (getArgs)
import Data.Char (isAlphaNum, isSpace, isAlpha)
import Data.List (isPrefixOf)
import System.IO (withFile, IOMode(ReadMode, WriteMode), hGetContents, hSetEncoding, utf8, hPutStr)

data Token = Ident String
           | Op String
           | Space String
           | Punct Char
           | Other Char
           deriving (Show, Eq)

isOpChar :: Char -> Bool
isOpChar c = c `elem` ("!#$%&*+./<=>?@\\^|-~:" :: String)

lexToken :: String -> (Token, String)
lexToken [] = (Space "", "")
lexToken (c:cs)
  | isSpace c = let (sp, rest) = span isSpace (c:cs) in (Space sp, rest)
  | isAlpha c || c == '_' = let (idStr, rest) = span (\x -> isAlphaNum x || x == '_') (c:cs) in (Ident idStr, rest)
  | c `elem` ("().," :: String) = (Punct c, cs)
  | isOpChar c = let (op, rest) = span isOpChar (c:cs) in (Op op, rest)
  | otherwise = (Other c, cs)

tokenize :: String -> [Token]
tokenize [] = []
tokenize str = let (t, rest) = lexToken str in t : tokenize rest

untokenize :: [Token] -> String
untokenize = concatMap toStr
  where
    toStr (Ident s) = s
    toStr (Op s) = s
    toStr (Space s) = s
    toStr (Punct c) = [c]
    toStr (Other c) = [c]

skipSpaces :: [Token] -> [Token]
skipSpaces = dropWhile (\case Space _ -> True; _ -> False)

-- matchers return Just (replacement, remaining_input)
matchSumZipWithDot :: [Token] -> Maybe ([Token], [Token])
matchSumZipWithDot (Ident "sum" : rest1) = case skipSpaces rest1 of
    (Punct '.' : rest2) -> case skipSpaces rest2 of
        (Ident "zipWith" : rest3) -> case skipSpaces rest3 of
            (Punct '(' : rest3a) -> case skipSpaces rest3a of
                (Op "*" : rest3b) -> case skipSpaces rest3b of
                    (Punct ')' : rest4) -> Just ([Ident "dot"], rest4)
                    _ -> Nothing
                _ -> Nothing
            _ -> Nothing
        _ -> Nothing
    _ -> Nothing
matchSumZipWithDot _ = Nothing

matchSumZipWithParen :: [Token] -> Maybe ([Token], [Token])
matchSumZipWithParen (Ident "sum" : rest1) = case skipSpaces rest1 of
    (Punct '(' : rest1a) -> case skipSpaces rest1a of
        (Ident "zipWith" : rest2) -> case skipSpaces rest2 of
            (Punct '(' : rest2a) -> case skipSpaces rest2a of
                (Op "*" : rest2b) -> case skipSpaces rest2b of
                    (Punct ')' : rest3) -> case skipSpaces rest3 of
                        (Ident arg1 : rest4) -> case skipSpaces rest4 of
                            (Ident arg2 : rest5) -> case skipSpaces rest5 of
                                (Punct ')' : rest6) -> Just ([Ident "dot", Space " ", Ident arg1, Space " ", Ident arg2], rest6)
                                _ -> Nothing
                            _ -> Nothing
                        _ -> Nothing
                    _ -> Nothing
                _ -> Nothing
            _ -> Nothing
        _ -> Nothing
    _ -> Nothing
matchSumZipWithParen _ = Nothing

matchSumZipWithDollar :: [Token] -> Maybe ([Token], [Token])
matchSumZipWithDollar (Ident "sum" : rest1) = case skipSpaces rest1 of
    (Op "$" : rest1a) -> case skipSpaces rest1a of
        (Ident "zipWith" : rest2) -> case skipSpaces rest2 of
            (Punct '(' : rest2a) -> case skipSpaces rest2a of
                (Op "*" : rest2b) -> case skipSpaces rest2b of
                    (Punct ')' : rest3) -> case skipSpaces rest3 of
                        (Ident arg1 : rest4) -> case skipSpaces rest4 of
                            (Ident arg2 : rest5) -> Just ([Ident "dot", Space " ", Ident arg1, Space " ", Ident arg2], rest5)
                            _ -> Nothing
                        _ -> Nothing
                    _ -> Nothing
                _ -> Nothing
            _ -> Nothing
        _ -> Nothing
    _ -> Nothing
matchSumZipWithDollar _ = Nothing

matchAngleBrackets :: [Token] -> Maybe ([Token], [Token])
matchAngleBrackets (Ident arg1 : rest1) = case skipSpaces rest1 of
    (Op "<>" : rest2) -> case skipSpaces rest2 of
        (Ident arg2 : rest3) -> Just ([Ident "multiply", Space " ", Ident arg1, Space " ", Ident arg2], rest3)
        _ -> Nothing
    _ -> Nothing
matchAngleBrackets _ = Nothing

matchHashBrackets :: [Token] -> Maybe ([Token], [Token])
matchHashBrackets (Ident arg1 : rest1) = case skipSpaces rest1 of
    (Op "#>" : rest2) -> case skipSpaces rest2 of
        (Ident arg2 : rest3) -> Just ([Ident "matVecMult", Space " ", Ident arg1, Space " ", Ident arg2], rest3)
        _ -> Nothing
    _ -> Nothing
matchHashBrackets _ = Nothing

matchers :: [[Token] -> Maybe ([Token], [Token])]
matchers = [matchSumZipWithDot, matchSumZipWithParen, matchSumZipWithDollar, matchAngleBrackets, matchHashBrackets]

replaceTokens :: [Token] -> [Token]
replaceTokens [] = []
replaceTokens ts =
    case foldl (\acc m -> case acc of Just _ -> acc; Nothing -> m ts) Nothing matchers of
        Just (rep, rest) -> rep ++ replaceTokens rest
        Nothing -> head ts : replaceTokens (tail ts)

getModuleName :: String -> Maybe String
getModuleName str =
  let ws = words str
  in case dropWhile (/= "module") ws of
       ("module" : name : _) -> Just name
       _ -> Nothing

isNumericModule :: String -> Bool
isNumericModule name = "Numeric" `isPrefixOf` name || "SignalProcessing" `isPrefixOf` name

processFile :: FilePath -> IO ()
processFile path = do
    content <- withFile path ReadMode $ \h -> do
        hSetEncoding h utf8
        c <- hGetContents h
        length c `seq` return c
    let mName = getModuleName content
    case mName of
        Just name | isNumericModule name -> do
            let newContent = untokenize (replaceTokens (tokenize content))
            if newContent /= content
                then do
                    withFile path WriteMode $ \h -> do
                        hSetEncoding h utf8
                        hPutStr h newContent
                    putStrLn $ "Optimized " ++ path
                else putStrLn $ "No changes for " ++ path
        Just name -> putStrLn $ "Skipping non-numeric module: " ++ name
        Nothing -> putStrLn $ "Skipping file without module name: " ++ path

main :: IO ()
main = do
    args <- getArgs
    if null args then
        putStrLn "Usage: bolt-optimizer <file1> <file2> ..."
    else do
        mapM_ processFile args
        putStrLn "Bolt optimizations applied."
