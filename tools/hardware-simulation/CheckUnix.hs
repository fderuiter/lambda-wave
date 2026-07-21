module Main (main) where

import System.Posix.Terminal

main :: IO ()
main = do
  let _ = EnableEcho :: TerminalMode
  let _ = EndOfFile :: ControlCharacter
  let _ = EndOfLine :: ControlCharacter
  putStrLn "Unix terminal types available"
