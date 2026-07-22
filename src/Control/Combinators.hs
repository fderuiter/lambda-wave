module Control.Combinators
  ( retryEither,
    retryAction,
    paceMapM_,
    paceWhen,
  )
where

import Control.Concurrent (threadDelay)
import Control.Monad (when)

-- | Retry an IO action that returns an Either, applying a delay on Left.
retryEither :: Int -> Int -> e -> IO (Either e a) -> IO (Either e a)
retryEither attempts delayMcs defaultErr action = go attempts
  where
    go n
      | n <= 0 = return (Left defaultErr)
      | otherwise = do
          res <- action
          case res of
            Right val -> return (Right val)
            Left _ -> do
              threadDelay delayMcs
              go (n - 1)

-- | Map a monadic action over a list with pacing between executions.
paceMapM_ :: Int -> (a -> IO ()) -> [a] -> IO ()
paceMapM_ _ _ [] = return ()
paceMapM_ _ f [x] = f x
paceMapM_ delayMcs f (x : xs) = do
  f x
  threadDelay delayMcs
  paceMapM_ delayMcs f xs

-- | Execute an action after a delay if a condition is met.
paceWhen :: Bool -> Int -> IO a -> IO a
paceWhen condition delayMcs action = do
  when condition $ threadDelay delayMcs
  action

-- | Retry a custom IO action based on a predicate, applying a delay before retrying.
retryAction :: Int -> Int -> (res -> Bool) -> res -> IO res -> IO res
retryAction attempts delayMcs shouldRetry defaultRes action = go attempts
  where
    go n
      | n <= 0 = return defaultRes
      | otherwise = do
          res <- action
          if shouldRetry res
            then do
              threadDelay delayMcs
              go (n - 1)
            else return res
