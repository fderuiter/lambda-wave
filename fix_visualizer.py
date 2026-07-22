import sys

def fix_file(filename):
    with open(filename, 'r') as f:
        lines = f.readlines()
        
    out = []
    i = 0
    while i < len(lines):
        if lines[i].startswith('<<<<<<< HEAD'):
            # Find middle
            mid = -1
            end = -1
            for j in range(i+1, len(lines)):
                if lines[j].startswith('======='):
                    mid = j
                elif lines[j].startswith('>>>>>>>'):
                    end = j
                    break
            
            # We are in VisualizerMain.hs
            # First conflict is the imports block. 
            # We will just write a custom imports block for the first one.
            # Second conflict is the sync loop. We want the origin/main version.
            if "import Control.Exception" in lines[i+1]:
                # First conflict block 1
                out.append("""import Control.Exception (IOException, try)
import Control.Monad (forever, void, when)
import Data.Aeson (FromJSON (..), withObject, (.:))
import qualified Data.Aeson as A
import Data.Binary (decode)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.Config (targetHeight)
import Data.I18n (loadTranslations, translate, translateBeamState)
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Data.Types
import Data.Word (Word32, Word64, Word8)
import FFI.Hud.Types (HudStateC (..), Point3DC (..))
import FFI.RingBuffer.IO (attachRingBuffer)
import FFI.RingBuffer.Types (RingBufferControl)
import Foreign.C.String (CString, newCString, peekCString, withCString)
import Foreign.C.Types (CSize (..))
import Foreign.ForeignPtr (ForeignPtr)
import Foreign.Marshal.Alloc (alloca, allocaBytes)
import Foreign.Marshal.Array (withArrayLen)
import Foreign.Ptr (FunPtr, Ptr, castPtr, nullPtr, plusPtr)
import Foreign.Storable (Storable (..))
import Hardware.Consumer (consumerLoop)
import Safety.Thread (ThreadShutdownAction (..), forkSafetyThreadOS)
import SignalProcessing.Kalman (KalmanConfig (..), KalmanState (..), initKalman, pattern V3)
import System.Exit (exitFailure)
import System.Posix.IO (OpenFileFlags (..), OpenMode (..), defaultFileFlags, fdReadBuf, openFd)
import System.Posix.Types (ByteCount, Fd)
import UI.Presentation (BeamDisplayInfo (..), getBeamDisplayInfo, indicatorScaleLimitMax, indicatorScaleLimitMin, pointCloudColorRGB)
""")
                
                # We need to skip all lines from i to end, and also notice there's TWO conflict blocks for imports in VisualizerMain.
                # Actually, wait, let's just use git checkout --theirs/ours approach or do it properly.
                
    
