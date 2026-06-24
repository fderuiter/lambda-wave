module Main (main) where

import System.Environment (getArgs)
import Data.Char (isAlphaNum)
import Data.List (isPrefixOf)

replaceCode :: String -> String
replaceCode [] = []
replaceCode str
  | "sum (zipWith (*) " `isPrefixOf` str =
      let rest = drop 19 str
          (arg1, rest1) = span isIdChar (dropWhile (== ' ') rest)
          (arg2, rest2) = span isIdChar (dropWhile (== ' ') rest1)
          rest3 = dropWhile (== ' ') rest2
      in if not (null arg1) && not (null arg2) && ")" `isPrefixOf` rest3
         then "dot " ++ arg1 ++ " " ++ arg2 ++ replaceCode (drop 1 rest3)
         else str !! 0 : replaceCode (tail str)
  | "sum $ zipWith (*) " `isPrefixOf` str =
      let rest = drop 18 str
          (arg1, rest1) = span isIdChar (dropWhile (== ' ') rest)
          (arg2, rest2) = span isIdChar (dropWhile (== ' ') rest1)
      in if not (null arg1) && not (null arg2)
         then "dot " ++ arg1 ++ " " ++ arg2 ++ replaceCode rest2
         else str !! 0 : replaceCode (tail str)
  | otherwise =
      -- look for `<>` and `push` them to `multiply`
      -- We need to find `A <> B` where A and B are identifiers.
      -- To keep it simple, we can just parse word by word.
      -- A generic find and replace for binary operators:
      let (token, rest) = lexId str
      in if not (null token)
         then let (sp1, rest1) = span (== ' ') rest
              in if "<>" `isPrefixOf` rest1
                 then let rest2 = drop 2 rest1
                          (sp2, rest3) = span (== ' ') rest2
                          (tok2, rest4) = lexId rest3
                      in if not (null tok2)
                         then "multiply " ++ token ++ " " ++ tok2 ++ replaceCode rest4
                         else token ++ sp1 ++ "<>" ++ sp2 ++ replaceCode rest3
                 else if "#>" `isPrefixOf` rest1
                      then let rest2 = drop 2 rest1
                               (sp2, rest3) = span (== ' ') rest2
                               (tok2, rest4) = lexId rest3
                           in if not (null tok2)
                              then "matVecMult " ++ token ++ " " ++ tok2 ++ replaceCode rest4
                              else token ++ sp1 ++ "#>" ++ sp2 ++ replaceCode rest3
                      else token ++ replaceCode rest
         else str !! 0 : replaceCode (tail str)

isIdChar :: Char -> Bool
isIdChar c = isAlphaNum c || c == '_'

lexId :: String -> (String, String)
lexId = span isIdChar

main :: IO ()
main = do
    args <- getArgs
    if null args then
        putStrLn "Usage: bolt-optimizer <file>"
    else do
        mapM_ (\path -> do
            content <- readFile path
            writeFile path (replaceCode content)
            ) args
        putStrLn "Bolt optimizations applied."
