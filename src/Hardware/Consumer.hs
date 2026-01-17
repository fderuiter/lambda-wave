{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

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

import FFI.RingBuffer.Types (RingBufferControl(..))
import FFI.RingBuffer.IO (getWriteOffset, setReadOffset)
import Data.Types

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
                    -- specific requirement: "Synchronization: Scan for Magic Word"

                    let (frames, bytesConsumed) = parseStream lbs

                    -- 5. Force Evaluation (Critical for FFI Safety)
                    -- We must ensure all data is copied out of the Ring Buffer (via Lazy ByteString)
                    -- BEFORE we update the read_offset. If we don't, the producer might overwrite
                    -- the memory while we are lazily parsing it.
                    _ <- evaluate (force frames)

                    -- 6. Update State
                    unless (null frames) $ do
                        atomically $ modifyTVar' stateVar $ \s ->
                            s { currentPoints = concatMap points frames } -- Simplified integration
                        -- putStrLn $ "[Consumer] Parsed " ++ show (length frames) ++ " frames."

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

-- | Helper to find the offset of the Magic Word in a Lazy ByteString
findMagicOffset :: BL.ByteString -> Maybe Int64
findMagicOffset lbs = go 0 lbs
  where
    magic = BL.pack [1,2,3,4,5,6,7,8]
    go skipped input =
        case BL.elemIndex 1 input of -- Find 0x01
            Nothing -> Nothing
            Just off ->
                let candidate = BL.drop off input
                in if BL.take 8 candidate == magic
                   then Just (skipped + off)
                   else go (skipped + off + 1) (BL.drop (off + 1) input)

-- | Parses a stream of bytes into RadarFrames.
-- Returns the frames and the total bytes consumed.
-- Uses incremental parsing to handle partial frames safely.
-- Now resilient to garbage via 'findMagicOffset' and tail-recursive.
parseStream :: BL.ByteString -> ([RadarFrame], Int64)
parseStream input = go input 0 []
  where
    go currentInput totalConsumed accFrames =
        case findMagicOffset currentInput of
            Nothing ->
                -- Magic word not found.
                -- Be conservative: Keep the last 7 bytes in case magic word is split.
                let len = BL.length currentInput
                    skipped = if len > 7 then len - 7 else 0
                in (reverse accFrames, totalConsumed + skipped)
            Just off ->
                -- Magic word found at 'off'.
                -- We invoke the parser on the stream starting at 'off'.
                let input' = BL.drop off currentInput
                    -- Use parseLoop to parse *one* frame or run until failure/partial
                    -- We pass 'off' as already consumed garbage.
                in parseLoop (G.runGetIncremental getRadarFrame) (BL.toChunks input') (totalConsumed + off) accFrames

    parseLoop decoder chunks currentConsumed accFrames =
        case decoder of
            G.Done unused consumed frame ->
                -- Frame parsed!
                let newConsumed = currentConsumed + consumed
                    -- Re-construct remaining input to find NEXT frame (or skip garbage)
                    remainingLBS = BL.fromChunks (unused : chunks)
                in
                    -- Tail Call: Continue parsing from the remaining input
                    go remainingLBS newConsumed (frame : accFrames)

            G.Fail _ consumed _ ->
                -- Failure (e.g. malformed frame).
                -- consume bytes and stop.
                let advanced = if consumed == 0 then 1 else consumed
                in (reverse accFrames, currentConsumed + advanced)

            G.Partial k ->
                case chunks of
                    [] ->
                        -- No more chunks. We are partial.
                        (reverse accFrames, currentConsumed)
                    (c:cs) ->
                        -- Feed next chunk
                        parseLoop (k (Just c)) cs currentConsumed accFrames

-- | Parser for a single Radar Frame
getRadarFrame :: G.Get RadarFrame
getRadarFrame = do
    -- 1. Verify Magic Word (It should be there because we searched for it)
    bytes <- G.getLazyByteString 8
    if bytes /= BL.pack [1,2,3,4,5,6,7,8]
    then fail "Magic Word Mismatch"
    else return ()

    -- 2. Read Header
    _version <- G.getWord32le
    _totalLen <- G.getWord32le
    _platform <- G.getWord32le
    _frameNum <- G.getWord32le
    _cpuCycles <- G.getWord32le
    numTLVs <- G.getWord32le
    _subFrameNum <- G.getWord32le

    -- 3. Parse TLVs
    points <- parseTLVs (fromIntegral numTLVs)

    return $ RadarFrame B.empty points

-- | Scans input until Magic Word is found (Deprecated/Unused in favor of outer search)
findMagicWord :: G.Get ()
findMagicWord = do
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
    tlvType <- G.getWord32le
    tlvLen <- G.getWord32le

    case tlvType of
        1 -> do -- Detected Points
            -- Let's assume standard TI: Length is payload length.
            let numPoints = fromIntegral tlvLen `div` 16
            points <- getPoints numPoints
            rest <- parseTLVs (n - 1)
            return (points ++ rest)
        _ -> do
            G.skip (fromIntegral tlvLen)
            parseTLVs (n - 1)

getPoints :: Int -> G.Get [Point3D]
getPoints n = do
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
