module Main (main) where

import System.Posix.Terminal

main :: IO ()
main = do
    (master, slave) <- openPseudoTerminal
    print master
    print slave
