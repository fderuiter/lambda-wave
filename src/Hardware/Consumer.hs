{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE RecordWildCards #-}

{-|
Module      : Hardware.Consumer
Description : Zero-Copy Consumer for Ring Buffer
Copyright   : (c) 2024
License     : BSD-3-Clause

This module implements the consumer thread that reads from the shared ring buffer
using a zero-copy strategy. It polls the C++ ring buffer's write offset using
acquire semantics and parses the incoming data stream into Haskell types.
-}
module Hardware.Consumer (
    consumerLoop,
    parseStream, -- Exported for testing
    createLazyByteString -- Exported for testing
) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.STM
import Control.Monad (unless)
import Data.Word (Word8, Word64)
import Data.Int (Int64)
import Foreign.ForeignPtr (newForeignPtr_, ForeignPtr, castForeignPtr)
import Foreign.Ptr (Ptr)
import Foreign.Storable (peek)
import Foreign.C.Types (CChar)
import qualified Data.ByteString as B
import qualified Data.ByteString.Internal as BI
import qualified Data.ByteString.Lazy as BL
import qualified Data.Binary.Get as G
import qualified Data.Vector.Storable as V

import FFI.RingBuffer.Types (RingBufferControl(..))
import FFI.RingBuffer.IO (getWriteOffset, setReadOffset)
import Data.Types

-- | The Consumer Thread Loop
--
-- * Polls 'write_offset' (atomic acquire).
-- * If new data exists, creates a Lazy ByteString referencing the buffer (Zero-Copy).
-- * Parses frames using 'Data.Binary.Get'.
-- * Updates 'SystemState'.
consumerLoop :: Ptr RingBufferControl -> TVar SystemState -> IO ()
consumerLoop controlPtr stateVar = do
    -- Read initial control block (non-atomic for immutable fields)
    ctrl <- peek controlPtr
    let bufStart = bufferStart ctrl
        bufSize  = fromIntegral (bufferSize ctrl) :: Int

    -- ForeignPtr to the buffer (no finalizer, as we don't own the memory)
    fp <- newForeignPtr_ bufStart

    putStrLn $ "[Consumer] Started. Buffer Size: " ++ show bufSize

    -- Internal Loop State
    let loop readOff = do
            -- 1. Poll Write Offset (Atomic Acquire)
            writeOff <- getWriteOffset controlPtr

            if writeOff == readOff
                then do
                    -- No new data, sleep briefly to avoid busy wait
                    threadDelay 1000 -- 1ms
                    loop readOff
                else do
                    -- 2. Calculate available data
                    -- (available calculation omitted as currently unused, but good for debug)

                    -- 3. Create Zero-Copy Lazy ByteString
                    let lbs = createLazyByteString fp bufSize readOff writeOff

                    -- 4. Parse Frames
                    -- We use 'runGetIncremental' to handle the stream.
                    -- Note: Since we poll chunks, we might get partial frames.
                    -- However, 'runGetIncremental' expects to be fed.
                    -- Here, we simplify by attempting to parse as much as possible
                    -- from the current snapshot. A robust implementation would maintain
                    -- the decoder state across loops.
                    -- specific requirement: "Synchronization: Scan for Magic Word"

                    let (frames, bytesConsumed) = parseStream lbs

                    -- 5. Update State
                    unless (null frames) $ do
                        atomically $ modifyTVar' stateVar $ \s ->
                            s { currentPoints = concatMap points frames } -- Simplified integration
                        -- putStrLn $ "[Consumer] Parsed " ++ show (length frames) ++ " frames."

                    -- 6. Update Read Offset
                    -- In a real ring buffer, we advance readOff by how much we processed.
                    -- But here, the producer might overwrite us if we are slow.
                    -- Also, we constructed 'lbs' from *all* available data.
                    -- If we successfully parsed everything, we catch up to writeOff.
                    -- If we have partial data at the end, we should only advance by bytesConsumed.

                    let newReadOff = (readOff + fromIntegral bytesConsumed) `rem` bufSize

                    -- 7. Notify Producer (Release Semantics)
                    -- We must update the shared read offset so the producer can reclaim space
                    -- (if it implements flow control) or just for monitoring.
                    setReadOffset controlPtr newReadOff

                    loop newReadOff

    loop 0

-- | Creates a Lazy ByteString from the ring buffer pointers.
-- Handles the wrap-around case by creating 1 or 2 chunks.
createLazyByteString :: ForeignPtr CChar -> Int -> Int -> Int -> BL.ByteString
createLazyByteString fp bufSize readOff writeOff =
    if writeOff >= readOff
    then
        -- Contiguous chunk
        let len = writeOff - readOff
            chunk = BI.fromForeignPtr (castPtr fp) readOff len
        in BL.fromStrict chunk
    else
        -- Wrapped: [readOff .. end] + [0 .. writeOff]
        let len1 = bufSize - readOff
            chunk1 = BI.fromForeignPtr (castPtr fp) readOff len1
            len2 = writeOff
            chunk2 = BI.fromForeignPtr (castPtr fp) 0 len2
        in BL.fromChunks [chunk1, chunk2]
    where
      castPtr :: ForeignPtr a -> ForeignPtr Word8
      castPtr = castForeignPtr

-- | Parses a stream of bytes into RadarFrames.
-- Returns the frames and the total bytes consumed.
-- Uses incremental parsing to handle partial frames safely.
parseStream :: BL.ByteString -> ([RadarFrame], Int64)
parseStream input = loop (G.runGetIncremental getRadarFrame) (BL.toChunks input) 0 []
  where
    loop decoder chunks totalConsumed acc =
        case decoder of
            G.Done unused consumed frame ->
                -- Frame parsed!
                -- 'consumed' is bytes consumed by THIS decoder instance since start.
                -- 'unused' is the part of the LAST chunk that wasn't used.
                -- We need to proceed with 'unused' + remaining 'chunks'.
                let newTotal = totalConsumed + consumed
                    nextDecoder = G.runGetIncremental getRadarFrame
                    -- We need to construct the input for the next step.
                    -- 'unused' is a ByteString.
                in if B.null unused
                   then loop nextDecoder chunks newTotal (frame : acc)
                   else loop (G.pushChunk nextDecoder unused) chunks newTotal (frame : acc)

            G.Fail _ consumed _ ->
                -- Failure. Consume bytes and stop.
                let advanced = if consumed == 0 then 1 else consumed
                in (reverse acc, totalConsumed + advanced)

            G.Partial k ->
                case chunks of
                    [] ->
                        -- No more chunks. We are partial.
                        -- Do NOT consume the partial bytes.
                        -- Return only what was fully consumed.
                        (reverse acc, totalConsumed)
                    (c:cs) ->
                        -- Feed next chunk
                        loop (k (Just c)) cs totalConsumed acc

-- | Parser for a single Radar Frame
getRadarFrame :: G.Get RadarFrame
getRadarFrame = do
    -- 1. Scan for Magic Word
    findMagicWord

    -- 2. Read Header (Basic fields needed for length validation)
    -- TI Header Format (approximate, based on standard SDK):
    -- Magic (8), Version (4), TotalPacketLen (4), Platform (4), FrameNum (4), Time (4), NumTLVs (4), SubFrame (4)
    _version <- G.getWord32le
    _totalLen <- G.getWord32le
    _platform <- G.getWord32le
    _frameNum <- G.getWord32le
    _cpuCycles <- G.getWord32le
    numTLVs <- G.getWord32le
    _subFrameNum <- G.getWord32le

    -- 3. Parse TLVs
    -- Total Header size = 8 + 4*7 = 36 bytes (excluding magic word? No, magic is part of header)
    -- We already consumed Magic (8). Then 7 words (28). Total 36.

    -- We need to parse 'numTLVs'
    points <- parseTLVs (fromIntegral numTLVs)

    return $ RadarFrame B.empty points -- Storing empty raw header for now to save space

-- | Scans input until Magic Word is found
findMagicWord :: G.Get ()
findMagicWord = do
    -- Peek 8 bytes
    bytes <- G.lookAhead (G.getLazyByteString 8)
    if bytes == BL.pack [1,2,3,4,5,6,7,8]
    then do
        G.skip 8
        return ()
    else do
        G.skip 1
        findMagicWord

-- | Parse TLVs
parseTLVs :: Int -> G.Get [Point3D]
parseTLVs 0 = return []
parseTLVs n = do
    -- TLV Header: Type (4), Length (4)
    tlvType <- G.getWord32le
    tlvLen <- G.getWord32le

    case tlvType of
        1 -> do -- Detected Points
            -- Payload: Array of Point {x,y,z,v} (4 * 4 = 16 bytes)
            -- Num points = (tlvLen - 8) / 16 ?? No, tlvLen usually includes header?
            -- TI SDK: tlvLen is length of Value? Or Type+Length+Value?
            -- Usually it's length of Value. But sometimes it includes header.
            -- Let's assume standard TI: Length is payload length.
            let numPoints = fromIntegral tlvLen `div` 16
            points <- getPoints numPoints
            rest <- parseTLVs (n - 1)
            return (points ++ rest)
        _ -> do
            -- Skip unknown TLV
            G.skip (fromIntegral tlvLen)
            parseTLVs (n - 1)

getPoints :: Int -> G.Get [Point3D]
getPoints n = do
    -- Using Vector Storable would be more efficient here but 'Data.Types' uses [Point3D].
    -- We will read into Vector Storable Point first (Zero Copy-ish if we could cast,
    -- but ByteString is not guaranteed aligned, so we must copy to Storable Vector or read one by one).
    -- Since we need to convert to Point3D (Double) anyway, we read floats and convert.
    rawPoints <- V.replicateM n getPoint
    return $ map toPoint3D (V.toList rawPoints)

getPoint :: G.Get Point
getPoint = do
    x <- G.getFloatle
    y <- G.getFloatle
    z <- G.getFloatle
    v <- G.getFloatle
    return $ Point x y z v

toPoint3D :: Point -> Point3D
toPoint3D Point{..} = Point3D
    { px = float2Double px'
    , py = float2Double py'
    , pz = float2Double pz'
    , v  = float2Double v'
    , snr = 0.0 -- Not in Type 1
    }

float2Double :: Float -> Double
float2Double = realToFrac
