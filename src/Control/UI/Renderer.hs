module Control.UI.Renderer (renderLoop) where

import Data.Types
import Control.Concurrent.STM
import Graphics.UI.GLUT

renderLoop :: TVar SystemState -> IO ()
renderLoop stateVar = do
    displayCallback $= display stateVar
    idleCallback $= Just (postRedisplay Nothing)
    mainLoop

display :: TVar SystemState -> IO ()
display stateVar = do
    clear [ColorBuffer]
    state <- readTVarIO stateVar

    -- Draw logic based on state
    -- Simple colored quad
    renderPrimitive Quads $ do
        color $ case beamState state of
            BeamOn   -> Color3 (0.0::GLfloat) 1.0 0.0 -- Green
            BeamOff  -> Color3 1.0 0.0 0.0 -- Red
            BeamHold -> Color3 1.0 1.0 0.0 -- Yellow

        vertex $ Vertex3 (-0.5::GLfloat) (-0.5) 0
        vertex $ Vertex3 ( 0.5::GLfloat) (-0.5) 0
        vertex $ Vertex3 ( 0.5::GLfloat) ( 0.5) 0
        vertex $ Vertex3 (-0.5::GLfloat) ( 0.5) 0

    flush
