import System.Posix.IO.ByteString
import System.Posix.Types
import Data.ByteString
main = do { (bs, _) <- fdRead (Fd 0) 10; print (bs :: ByteString) }
