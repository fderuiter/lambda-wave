module Control.UI.Window (initWindow) where

import Graphics.UI.GLUT

initWindow :: IO ()
initWindow = do
    _ <- getArgsAndInitialize
    initialDisplayMode $= [DoubleBuffered, RGBMode]
    _ <- createWindow "Lambda-Wave Visualizer"
    return ()
