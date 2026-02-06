{-# LANGUAGE CPP #-}
module Main where
import System.Posix.IO.ByteString (fdRead)
import Data.ByteString (ByteString)
import System.Posix.Types (Fd(..))

main :: IO ()
main = do
#if MIN_VERSION_unix(2,8,0)
    putStrLn "Unix >= 2.8"
#else
    putStrLn "Unix < 2.8"
#endif
    putStrLn "Compiles"
