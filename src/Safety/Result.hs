module Safety.Result 
    ( SafetyResult(..)
    ) where

-- | A mandatory result type for safety-critical operations.
-- It lacks standard functional instances (Functor, Applicative, Monad)
-- to force developers to explicitly pattern match on the result.
data SafetyResult a = Safe a | Fault String
    deriving (Show, Eq)
