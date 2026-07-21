module Main (main) where

import System.Posix.Terminal

main :: IO ()
main = do
  print B9600
  print B38400
  print B115200
