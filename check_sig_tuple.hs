import System.Posix.IO.ByteString
import System.Posix.Types
main = do { (bs, c) <- fdRead (Fd 0) 10; print c }
