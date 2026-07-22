module Main (main) where

import Control.Monad (replicateM_)
import Foreign.Ptr (nullPtr)
import Hardware.Radar
import System.Exit (exitSuccess)
import System.Mem (performGC)

main :: IO ()
main = do
  -- Test memory leak safety of bracket pattern
  replicateM_ 1000 $ do
    withRadar $ \_ -> return ()

  -- Test memory leak safety of ForeignPtr pattern
  replicateM_ 1000 $ do
    _ <- attachRadar nullPtr
    return ()

  performGC
  putStrLn "Passed memory-leak tests."
  exitSuccess
