{-# OPTIONS_GHC -Werror #-}
import qualified System.Posix.IO.ByteString as PBS
import System.Posix.Types (Fd(..))

main = do
    (s, _) <- PBS.fdRead (Fd 0) 10
    putStrLn s
