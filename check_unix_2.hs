import System.Posix.Terminal

main = do
    print (EnableEcho :: TerminalMode)
