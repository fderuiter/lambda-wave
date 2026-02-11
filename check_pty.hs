import System.Posix.Terminal
import System.Posix.IO

main :: IO ()
main = do
    (master, slave) <- openPseudoTerminal
    print master
    print slave
