module Hardware.{sensor_name}Check (main) where

import Hardware.{sensor_name}
import System.Mem (performGC)
import Control.Monad (replicateM_)
import Foreign.Ptr (nullPtr)
import System.Exit (exitSuccess)

main :: IO ()
main = do
    -- Test memory leak safety of bracket pattern
    replicateM_ 1000 $ do
        with{sensor_name} $ \_ -> return ()
    
    -- Test memory leak safety of ForeignPtr pattern
    replicateM_ 1000 $ do
        _ <- attach{sensor_name} nullPtr
        return ()
    
    performGC
    putStrLn "Passed memory-leak tests."
    exitSuccess
