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

    let avgHeight = calculateAverageHeight pts

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

-- | Strictly calculate average height to avoid thunk buildup
calculateAverageHeight :: [Point3D] -> Double
calculateAverageHeight [] = 0
calculateAverageHeight ps = total / count
  where
    (total, count) = foldl' (\(!s, !c) p -> (s + pz p, c + 1.0)) (0.0, 0.0) ps
