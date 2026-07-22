import re

with open('app/VisualizerMain.hs', 'r') as f:
    text = f.read()

# Replace the inner macro with top level
macro_code = """
#if MIN_VERSION_unix(2,8,0)
openTelemetryPipe :: FilePath -> IO Fd
openTelemetryPipe path = openFd path ReadOnly defaultFileFlags { nonBlock = False, creat = Nothing }
#else
openTelemetryPipe :: FilePath -> IO Fd
openTelemetryPipe path = openFd path ReadOnly Nothing defaultFileFlags { nonBlock = False }
#endif
"""

# The problematic part in origin/main looks like:
#     forever $ do
# #if MIN_VERSION_unix(2,8,0)
#         let flags' = flags { creat = Nothing }
#         fdRes <- try (openFd pipePath ReadOnly flags') :: IO (Either IOException Fd)
# #else
#         fdRes <- try (openFd pipePath ReadOnly Nothing flags) :: IO (Either IOException Fd)
# #endif

# We will just replace ipcReceiverLoop entirely.
new_ipc = """#if MIN_VERSION_unix(2,8,0)
openTelemetryPipe :: FilePath -> IO Fd
openTelemetryPipe path = openFd path ReadOnly defaultFileFlags { nonBlock = False, creat = Nothing }
#else
openTelemetryPipe :: FilePath -> IO Fd
openTelemetryPipe path = openFd path ReadOnly Nothing defaultFileFlags { nonBlock = False }
#endif

ipcReceiverLoop :: TVar SystemState -> IO ()
ipcReceiverLoop stateVar = do
  let pipePath = "/tmp/sgrt_telemetry.fifo"
  forever $ do
    fdRes <- try (openTelemetryPipe pipePath) :: IO (Either IOException Fd)
    case fdRes of
      Left _ -> threadDelay 1000000
      Right fd -> do
        readData fd stateVar"""

# Find the start of ipcReceiverLoop
start_idx = text.find('ipcReceiverLoop :: TVar SystemState -> IO ()')
# Find the start of readData
end_idx = text.find('readData :: Fd -> TVar SystemState -> IO ()')

text = text[:start_idx] + new_ipc + "\n\n" + text[end_idx:]

with open('app/VisualizerMain.hs', 'w') as f:
    f.write(text)

