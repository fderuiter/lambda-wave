{-# LANGUAGE BangPatterns #-}
module Control.Gating (processFrame) where

import Data.Types
import Data.Config
import Control.Mesher (fitPolynomialSurface)
import Control.Concurrent.STM
import System.Clock
import Data.List (foldl')

-- | The main logic function called every frame
processFrame :: TVar SystemState -> [Point3D] -> IO ()
processFrame stateVar pts = do
    -- 1. Mesh the surface
    let _coeffs = fitPolynomialSurface pts
    -- (In a real system, we'd use coeffs to calculate amplitude at isocenter)

    -- Calculate average height using a strict fold to prevent thunk leaks
    -- (sum (map pz pts) creates intermediate list of thunks)
    let (!totalHeight, !count) = foldl' (\(!sumH, !cnt) p -> (sumH + pz p, cnt + 1)) (0.0, 0 :: Int) pts
        avgHeight = if count == 0 then 0 else totalHeight / fromIntegral count

    -- 2. Schmidt Trigger Logic / Hysteresis
    -- (Simplified for skeleton)
    let newState = if abs (avgHeight - targetHeight) < gatingTolerance
                   then BeamOn
                   else BeamOff

    currTime <- getTime Monotonic
    atomically $ modifyTVar stateVar $ \s -> s
        { currentPoints = pts
        , beamState = newState
        , lastFrameTime = currTime
        }
