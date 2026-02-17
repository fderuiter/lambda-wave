module Control.UI.Window (initWindow) where

import Graphics.UI.GLUT

-- | Initializes the GLUT window and sets up the OpenGL rendering context.
-- Configures:
-- * Display Mode (Double Buffered, RGB, Depth)
-- * Window Size (800x600) & Position
-- * Projection Matrix (Perspective)
-- * ModelView Matrix (Camera Setup)
-- * Depth Test (Less)
-- * Clear Color (Black)
initWindow :: IO ()
initWindow = do
    _ <- getArgsAndInitialize
    initialDisplayMode $= [DoubleBuffered, RGBMode, WithDepthBuffer]
    initialWindowSize $= Size 800 600
    initialWindowPosition $= Position 100 100
    _ <- createWindow "Lambda-Wave Visualizer"

    -- Setup Projection
    matrixMode $= Projection
    loadIdentity
    -- gluPerspective 45 degrees, aspect ratio 1.33, zNear 0.1, zFar 100.0
    perspective 45.0 1.33 0.1 100.0

    -- Setup ModelView
    matrixMode $= Modelview 0
    loadIdentity
    -- Camera at (0, 5, 5) looking at (0, 0, 0) up (0, 1, 0)
    -- Adjusted to look down at the patient
    lookAt (Vertex3 0 5 5) (Vertex3 0 0 0) (Vector3 0 1 0)

    -- Enable Depth Test
    depthFunc $= Just Less

    -- Background Color (Black)
    clearColor $= Color4 0 0 0 0

    return ()
