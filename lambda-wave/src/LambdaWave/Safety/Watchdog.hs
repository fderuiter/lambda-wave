module LambdaWave.Safety.Watchdog (watchdogLoop) where

import LambdaWave.Types
import LambdaWave.Config (watchdogTimeoutNS)
import Control.Concurrent.STM
import Control.Concurrent (threadDelay)
import System.Clock
import Control.Monad (forever, when)
import System.Exit (exitFailure)

watchdogLoop :: TVar SystemState -> IO ()
watchdogLoop stateVar = forever $ do
    now <- getTime Monotonic
    lastTime <- lastFrameTime <$> readTVarIO stateVar

    let diff = toNanoSecs (diffTimeSpec now lastTime)

    when (diff > watchdogTimeoutNS) $ do
        putStrLn "!!! WATCHDOG TRIP: SYSTEM FROZEN !!!"
        putStrLn "!!! FORCING BEAM OFF !!!"
        -- In real HW, this would toggle a GPIO pin immediately
        exitFailure

    threadDelay 10000 -- Check every 10ms
