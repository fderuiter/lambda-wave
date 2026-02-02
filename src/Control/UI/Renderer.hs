module Control.UI.Renderer (renderLoop) where

import Data.Types
import Control.Concurrent.STM
import Control.Concurrent (threadDelay)
import Control.Monad (forever)

renderLoop :: TVar SystemState -> IO ()
renderLoop stateVar = forever $ do
    _state <- readTVarIO stateVar
    threadDelay 1000000 -- 1s
