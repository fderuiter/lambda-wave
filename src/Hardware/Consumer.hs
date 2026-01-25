{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}

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
import Control.Monad (unless, when)
import Control.DeepSeq (force)
import Control.Exception (evaluate)
import Data.Word (Word8)
import Data.Int (Int64)
import Foreign.ForeignPtr (newForeignPtr_, ForeignPtr, castForeignPtr, withForeignPtr)
import Foreign.Storable (peek)
import Foreign.C.Types (CChar)
import qualified Data.ByteString as B
import qualified Data.ByteString.Internal as BI
import qualified Data.ByteString.Lazy as BL
import qualified Data.Binary.Get as G
import qualified Data.Vector.Storable as V
import System.IO (hPutStrLn, stderr)

import FFI.RingBuffer.Types (RingBufferControl(..))
import FFI.RingBuffer.IO (getWriteOffset, setReadOffset)
import Data.Types

-- | The Magic Word sequence for TI Millimeter Wave Radar
magicPattern :: BL.ByteString
magicPattern = BL.pack [1, 2, 3, 4, 5, 6, 7, 8]

-- | The Consumer Thread Loop
--
-- * Polls 'write_offset' (atomic acquire).
-- * If new data exists, creates a Lazy ByteString referencing the buffer (Zero-Copy).
-- * Parses frames using 'Data.Binary.Get'.
-- * Updates 'SystemState'.
consumerLoop :: ForeignPtr RingBufferControl -> TVar SystemState -> IO ()
consumerLoop controlFp stateVar = withForeignPtr controlFp $ \controlPtr -> do
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
            -- Pass the ForeignPtr to ensure safety, although we are already inside withForeignPtr,
            -- this double check is fine or we rely on the fact that controlFp is alive.
            writeOff <- getWriteOffset controlFp

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

                    let (frames, bytesConsumed, corrupted) = parseStream lbs

                    -- 5. Force Evaluation (Critical for FFI Safety)
                    -- We must ensure all data is copied out of the Ring Buffer (via Lazy ByteString)
                    -- BEFORE we update the read_offset. If we don't, the producer might overwrite
                    -- the memory while we are lazily parsing it.
                    _ <- evaluate (force frames)

                    -- Log corruption if detected
                    when corrupted $ do
                         hPutStrLn stderr "[Consumer] Corrupt Packet detected."

                    -- 6. Update State
                    unless (null frames) $ do
                        atomically $ modifyTVar' stateVar $ \s ->
                            s { currentPoints = concatMap points frames } -- Simplified integration
                        -- putStrLn $ "[Consumer] Parsed " ++ show (length frames) ++ " frames."

                    when (bytesConsumed > 0 && null frames) $
                        putStrLn "[Consumer] Warning: Skipped garbage data (Magic Word search or Parse Error)."

                    -- 7. Update Read Offset
                    -- In a real ring buffer, we advance readOff by how much we processed.
                    -- But here, the producer might overwrite us if we are slow.
                    -- Also, we constructed 'lbs' from *all* available data.
                    -- If we successfully parsed everything, we catch up to writeOff.
                    -- If we have partial data at the end, we should only advance by bytesConsumed.

                    let newReadOff = (readOff + fromIntegral bytesConsumed) `rem` bufSize

                    -- 8. Notify Producer (Release Semantics)
                    -- We must update the shared read offset so the producer can reclaim space
                    -- (if it implements flow control) or just for monitoring.
                    setReadOffset controlFp newReadOff

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

-- | Skips garbage until a potential magic word start is found.
-- Efficiently scans for 0x01 using elemIndex instead of Get monad.
-- Uses tail recursion with strict accumulator to avoid stack overflow.
-- Returns (bytesSkipped, remainingInput)
skipToMagicWord :: BL.ByteString -> (Int64, BL.ByteString)
skipToMagicWord input = go 0 input
  where
    go !acc bs =
        case BL.elemIndex 1 bs of
            Nothing -> (acc + BL.length bs, BL.empty) -- No magic word start found, consume all
            Just idx ->
                let candidate = BL.drop idx bs
                in if BL.isPrefixOf magicPattern candidate
                   then (acc + idx, candidate) -- Found exact match
                   else
                       if BL.length candidate < 8
                       then (acc + idx, candidate) -- Keep partial match (might be valid end of buffer)
                       else
                           -- Found 0x01 but not followed by correct sequence (Garbage)
                           -- Skip the 0x01 and recurse
                           go (acc + idx + 1) (BL.drop 1 candidate)


-- | Parses a stream of bytes into RadarFrames.
-- Returns the frames, the total bytes consumed, and a boolean indicating corruption.
-- Uses incremental parsing to handle partial frames safely.
-- Optimization: Pre-scans for Magic Word to skip garbage efficiently.
parseStream :: BL.ByteString -> ([RadarFrame], Int64, Bool)
parseStream input =
    let (skipped, cleanInput) = skipToMagicWord input
        (frames, consumed, corrupted) = parseLoop (G.runGetIncremental getRadarFrame) (BL.toChunks cleanInput) 0 []
    in (frames, skipped + consumed, corrupted)
  where
    parseLoop decoder chunks totalConsumed acc =
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
                   then parseLoop nextDecoder chunks newTotal (frame : acc)
                   else parseLoop (G.pushChunk nextDecoder unused) chunks newTotal (frame : acc)

            G.Fail _ consumed _ ->
                -- Failure. Consume bytes and stop.
                let advanced = if consumed == 0 then 1 else consumed
                in (reverse acc, totalConsumed + advanced, True)

            G.Partial k ->
                case chunks of
                    [] ->
                        -- No more chunks. We are partial.
                        -- Do NOT consume the partial bytes.
                        -- Return only what was fully consumed.
                        (reverse acc, totalConsumed, False)
                    (c:cs) ->
                        -- Feed next chunk
                        parseLoop (k (Just c)) cs totalConsumed acc

-- | Parser for a single Radar Frame
getRadarFrame :: G.Get RadarFrame
getRadarFrame = do
    -- 1. Scan for Magic Word
    -- (We assume we are positioned at Magic Word or Partial Magic Word by skipToMagicWord)
    magic <- G.getLazyByteString 8
    unless (magic == magicPattern) $ fail "Invalid Magic Word"

    -- 2. Read Header (Basic fields needed for length validation)
    -- TI Header Format (approximate, based on standard SDK):
    -- Magic (8), Version (4), TotalPacketLen (4), Platform (4), FrameNum (4), Time (4), NumTLVs (4), SubFrame (4)
    _version <- G.getWord32le
    totalLen <- G.getWord32le
    _platform <- G.getWord32le
    _frameNum <- G.getWord32le
    _cpuCycles <- G.getWord32le
    numTLVs <- G.getWord32le
    _subFrameNum <- G.getWord32le

    -- Sanity Checks to enable Fail on corruption
    if totalLen < 36 || totalLen > 1000000
       then fail "Invalid Packet Length"
       else return ()

    if numTLVs > 200
       then fail "Too many TLVs"
       else return ()

    -- 3. Parse TLVs
    -- Total Header size = 8 + 4*7 = 36 bytes (excluding magic word? No, magic is part of header)
    -- We already consumed Magic (8). Then 7 words (28). Total 36.

    -- We need to parse 'numTLVs'
    points <- parseTLVs (fromIntegral numTLVs)

    return $ RadarFrame B.empty points -- Storing empty raw header for now to save space

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
            -- TI SDK: Length usually includes the header (8 bytes).
            -- We assume tlvLen = Header(8) + Payload.
            let len = fromIntegral tlvLen
                -- Use max to prevent negative if malformed
                payloadLen = if len >= 8 then len - 8 else 0
                numPoints = payloadLen `div` 16
                bytesRead = numPoints * 16
                padding = payloadLen - bytesRead

            points <- getPoints numPoints

            -- Sentinel: Ensure we align to the next TLV boundary by skipping padding
            when (padding > 0) $ G.skip padding

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
