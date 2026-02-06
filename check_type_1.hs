
import qualified System.Posix.IO.ByteString as PBS
import qualified Data.ByteString as B
import System.Posix.Types (Fd(..), ByteCount)

check :: Fd -> ByteCount -> IO (B.ByteString, ByteCount)
check = PBS.fdRead

main = putStrLn "Type checks"
