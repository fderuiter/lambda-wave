module Control.UI.Input (handleInput) where

import Data.Types
import Control.Concurrent.STM

handleInput :: TVar SystemState -> IO ()
handleInput _ = return ()
