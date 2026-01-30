module Control.UI.Renderer (renderLoop) where

import Data.Types
import Control.Concurrent.STM

renderLoop :: TVar SystemState -> IO ()
renderLoop _ = return ()
