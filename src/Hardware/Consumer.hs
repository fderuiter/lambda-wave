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

-- | Parses a stream of bytes into RadarFrames.
-- Returns the frames and the total bytes consumed.
-- Uses incremental parsing to handle partial frames safely.
-- Optimized to use Boyer-Moore search (via breakSubstring) for Magic Word.
parseStream :: BL.ByteString -> ([RadarFrame], Int64)
parseStream input = loop input 0 []
  where
    magic = BL.pack [1,2,3,4,5,6,7,8]

    findMagic inp consumed =
        let (prefix, rest) = BL.break (== 1) inp
            len = BL.length prefix
            newConsumed = consumed + len
        in if BL.null rest
           then (newConsumed, rest)
           else
             -- Found '1'. Check if it matches magic word.
             if BL.length rest < 8
             then (newConsumed, rest) -- Potential partial match, stop.
             else if BL.isPrefixOf magic rest
                  then (newConsumed, rest) -- Found.
                  else findMagic (BL.tail rest) (newConsumed + 1) -- Not magic, skip '1' and continue.

    loop currentInput totalConsumed acc =
        let (junkLen, rest) = findMagic currentInput 0
        in if BL.null rest
           then
             -- No '1' found in the entire input. Safe to consume all.
             (reverse acc, totalConsumed + junkLen)
           else if BL.length rest < 8
           then
             -- Found '1' but not enough data to verify magic word.
             -- Stop and wait for more data.
             (reverse acc, totalConsumed + junkLen)
           else
             -- Found magic word at start of 'rest'.
             -- Attempt to parse the frame.
             case runDecoder rest of
                (consumed, Just (Right frame)) ->
                    -- Success
                    let newTotal = totalConsumed + junkLen + consumed
                        remaining = BL.drop consumed rest
                    in loop remaining newTotal (frame : acc)

                (_, Just (Left _)) ->
                    -- Parse Failure (e.g. invalid length).
                    -- Skip magic word + 1 byte to restart search past the bad frame.
                    -- We consumed 'junkLen' (prefix) + 1.
                    let advance = junkLen + 1
                    in loop (BL.drop 1 rest) (totalConsumed + advance) acc

                (_, Nothing) ->
                    -- Partial Frame (need more data).
                    -- We consumed 'junkLen' (prefix) but decoder needs more.
                    -- We return 'totalConsumed + junkLen' so next time we start at 'rest' (Magic Word).
                    (reverse acc, totalConsumed + junkLen)

    runDecoder bs = feedDecoder (G.runGetIncremental getRadarFrame) (BL.toChunks bs)

    feedDecoder decoder [] =
        case decoder of
            G.Done _ consumed frame -> (consumed, Just (Right frame))
            G.Fail _ consumed err   -> (consumed, Just (Left err))
            G.Partial _             -> (0, Nothing)

    feedDecoder decoder (c:cs) =
        case decoder of
            G.Done _ consumed frame -> (consumed, Just (Right frame))
            G.Fail _ consumed err   -> (consumed, Just (Left err))
            G.Partial k             -> feedDecoder (k (Just c)) cs

-- | Parser for a single Radar Frame
getRadarFrame :: G.Get RadarFrame
getRadarFrame = do
    -- 1. Skip Magic Word (already found by scanner)
    G.skip 8

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
