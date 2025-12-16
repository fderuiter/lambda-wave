module LambdaWave.UI.Window (initWindow) where

import Graphics.UI.GLUT

initWindow :: IO ()
initWindow = do
    _ <- getArgsAndInitialize
    _ <- createWindow "Lambda-Wave Visualizer"
    return ()
