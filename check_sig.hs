import System.Posix.IO.ByteString
import System.Posix.Types
main = do { x <- fdRead (Fd 0) 10; print x }
