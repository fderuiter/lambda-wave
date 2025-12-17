module Hardware.Parser (parserLoop) where

import Control.Monad (forever)
import Control.Concurrent (threadDelay)
import qualified Data.ByteString as B
import Data.Types
import Hardware.Ingestion (readChunkFromC)
import qualified Control.Gating as Gating
import Control.Concurrent.STM

-- | The Parser Thread
-- Reads from the C Ring Buffer (via Ingestion helper) and parses frames
parserLoop :: TVar SystemState -> IO ()
parserLoop stateVar = forever $ do
    -- Read chunk from C buffer (e.g., 4KB)
    chunk <- readChunkFromC 4096
    if B.null chunk
        then threadDelay 1000 -- Wait a bit if empty
        else do
            -- In a real implementation, we would maintain a buffer for partial frames.
            -- Here we simplify: assume we get full frames or just drop.
            -- Real implementation needs a stateful parser (using Data.Binary.Get.pushChunk).
            let frames = parseFrames chunk
            mapM_ (\f -> Gating.processFrame stateVar (points f)) frames

parseFrames :: B.ByteString -> [RadarFrame]
parseFrames bs =
    -- Placeholder: Try to find magic word and parse
    -- For skeleton, return empty or dummy if magic word found?
    -- Let's just pretend we parsed something if the buffer is large enough
    if B.length bs > 50
    then [RadarFrame bs [Point3D 10 10 10 0 10]]
    else []
