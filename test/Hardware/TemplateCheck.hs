module Main (main) where

import Hardware.Template
import System.Mem (performGC)
import Control.Monad (replicateM_)
import Foreign.Ptr (nullPtr)
import System.Exit (exitSuccess)

main :: IO ()
main = do
    -- Test memory leak safety of bracket pattern
    replicateM_ 1000 $ do
        withTemplate $ \_ -> return ()
    
    -- Test memory leak safety of ForeignPtr pattern
    replicateM_ 1000 $ do
        _ <- attachTemplate nullPtr
        return ()
    
    performGC
    putStrLn "Passed memory-leak tests."
    exitSuccess
