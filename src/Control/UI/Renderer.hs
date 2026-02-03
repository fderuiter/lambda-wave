module Control.UI.Renderer (renderLoop) where

import Data.Types
import Control.Concurrent (threadDelay)
import Control.Concurrent.STM

renderLoop :: TVar SystemState -> IO ()
renderLoop _ = do
    putStrLn "[UI] Renderer Disabled. Entering Sleep Loop."
    loop
  where
    loop = threadDelay 1000000 >> loop
