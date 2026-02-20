{-|
Module      : Control.UI.Window
Description : OpenGL Window Management
Copyright   : (c) 2024
License     : AGPL-3.0-only

This module handles the initialization and creation of the GLUT window.
It ensures the correct display mode (Double Buffered, RGB) is requested.

Complexity: O(1) - One-time initialization.
Safety:
  - Fails fast if OpenGL context cannot be created.
-}
module Control.UI.Window (
    initWindow
) where

import Graphics.UI.GLUT

-- | Initialize GLUT and Create Window
-- * Requests Double Buffering (for smooth animation).
-- * Requests RGB Mode.
-- * Creates a window titled "Lambda-Wave Visualizer".
initWindow :: IO ()
initWindow = do
    _ <- getArgsAndInitialize
    initialDisplayMode $= [DoubleBuffered, RGBMode]
    _ <- createWindow "Lambda-Wave Visualizer"
    return ()
