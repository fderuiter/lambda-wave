module Control.UI.Renderer (renderLoop) where

import Data.Types
import SignalProcessing.Kalman (KalmanState(..), V3(..))
import Control.Concurrent.STM
import Graphics.UI.GLUT

-- | Main rendering loop for the visualization UI.
-- Sets up the display and idle callbacks, and starts the GLUT main loop.
-- This function blocks until the window is closed.
renderLoop :: TVar SystemState -> IO ()
renderLoop stateVar = do
    displayCallback $= display stateVar
    idleCallback $= Just (postRedisplay Nothing)
    mainLoop

-- | Display callback for GLUT.
-- Clears the screen, sets up the camera, and draws the current system state:
-- * Isocenter (Origin axes)
-- * Current Points (Point Cloud)
-- * Kalman State (Predicted Target)
-- * Beam State Indicator (Color Coded)
display :: TVar SystemState -> IO ()
display stateVar = do
    clear [ColorBuffer, DepthBuffer]
    loadIdentity

    -- Reset Camera (gluLookAt)
    -- Eye (0, 3, 3), Center (0, 0, 0), Up (0, 1, 0)
    lookAt (Vertex3 0 3 3) (Vertex3 0 0 0) (Vector3 0 1 0)

    state <- readTVarIO stateVar

    -- Draw Isocenter (Origin)
    renderPrimitive Lines $ do
        color $ Color3 (0.5::GLfloat) 0.5 0.5
        -- X Axis
        vertex $ Vertex3 (-1.0::GLfloat) 0 0
        vertex $ Vertex3 ( 1.0::GLfloat) 0 0
        -- Z Axis (Y in GL)
        vertex $ Vertex3 (0::GLfloat) -1.0 0
        vertex $ Vertex3 (0::GLfloat)  1.0 0
        -- Y Axis (Z in GL)
        vertex $ Vertex3 (0::GLfloat) 0 (-1.0)
        vertex $ Vertex3 (0::GLfloat) 0 1.0

    -- Draw Points
    renderPrimitive Points $ do
        color $ Color3 (0.0::GLfloat) 1.0 1.0 -- Cyan
        mapM_ drawPoint (currentPoints state)

    -- Draw Kalman State (Target)
    let (KalmanState (V3 px py pz) _) = kalmanState state
    renderPrimitive Lines $ do
        color $ Color3 (1.0::GLfloat) 0.0 0.0 -- Red
        -- Crosshair at Kalman Position
        -- Convert to Float for GL
        let x = realToFrac px
        let y = realToFrac py
        let z = realToFrac pz
        let s = 0.2 -- size

        -- Draw Crosshair
        vertex $ Vertex3 (x - s) y z
        vertex $ Vertex3 (x + s) y z

        vertex $ Vertex3 x (y - s) z
        vertex $ Vertex3 x (y + s) z

        vertex $ Vertex3 x y (z - s)
        vertex $ Vertex3 x y (z + s)

    -- Status Text (Beam State) indicator
    preservingMatrix $ do
        -- Draw a small square at top-left
        loadIdentity
        translate $ Vector3 (-3.0::GLfloat) 2.0 (-10.0)

        color $ case beamState state of
            BeamOn   -> Color3 (0.0::GLfloat) 1.0 0.0 -- Green
            BeamOff  -> Color3 1.0 0.0 0.0 -- Red
            BeamHold -> Color3 1.0 1.0 0.0 -- Yellow

        renderPrimitive Quads $ do
             vertex $ Vertex3 (0::GLfloat) 0 0
             vertex $ Vertex3 (0.5::GLfloat) 0 0
             vertex $ Vertex3 (0.5::GLfloat) 0.5 0
             vertex $ Vertex3 (0::GLfloat) 0.5 0

    swapBuffers

-- | Draws a single 3D point in OpenGL coordinates.
-- Converts from TI Sensor coordinates (X, Y=Depth, Z=Height) to GL coordinates (X, Y=Height, Z=-Depth).
drawPoint :: Point3D -> IO ()
drawPoint (Point3D x y z v snr) = do
    -- TI X -> GL X
    -- TI Z -> GL Y (Up)
    -- TI Y -> GL -Z (Depth)
    vertex $ Vertex3 (realToFrac x) (realToFrac z) (-realToFrac y)
