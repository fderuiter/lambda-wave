module Safety.Result
  ( SafetyResult (..),
  )
where

-- | A unified result type for safety-critical operations.
-- It deliberately does not derive Functor, Applicative, or Monad
-- to force explicit pattern matching on every use, preventing
-- silent propagation of failures.
data SafetyResult a
  = Safe !a
  | Unsafe !String
  | ClampedToMin !a
  | ClampedToMax !a
  | DivByZeroSafe !a
  deriving (Show, Eq)
