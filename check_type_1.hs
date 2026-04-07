import System.Posix.IO
import System.Posix.Types

main = do
    let _ = openFd :: FilePath -> OpenMode -> Maybe FileMode -> OpenFileFlags -> IO Fd
    putStrLn "Type checks 1"
