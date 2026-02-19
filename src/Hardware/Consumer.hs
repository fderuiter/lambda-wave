{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}

{-|
Module      : Hardware.Consumer
Description : Zero-Copy Consumer for Ring Buffer
Copyright   : (c) 2024
License     : AGPL-3.0-only

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
import Control.Monad (unless, when, forM_)
import Control.DeepSeq (force)
import Control.Exception (evaluate)
import Data.Word (Word8, Word32)
import Data.Int (Int64)
import Foreign.ForeignPtr (ForeignPtr, castForeignPtr, withForeignPtr, touchForeignPtr)
import Foreign.C.Types (CChar)
import qualified Foreign.Concurrent as FC
import qualified Data.ByteString as B
import qualified Data.ByteString.Internal as BI
import qualified Data.ByteString.Lazy as BL
import qualified Data.Binary.Get as G
import System.IO (hPutStrLn, stderr)
import Data.Time.HighRes (getMonotonicTimeNS)

import FFI.RingBuffer.Types (RingBufferControl(..), peekStaticFields)
import FFI.RingBuffer.IO (getWriteOffset, setReadOffset)
import Data.Types
import Control.Gating (processFrame)
import Hardware.Types

-- | The Magic Word sequence for TI Millimeter Wave Radar
magicPattern :: BL.ByteString
magicPattern = BL.pack [1, 2, 3, 4, 5, 6, 7, 8]

-- | Maximum allowed TLV size to prevent Denial of Service (DoS) attacks
-- where a malicious packet claims a huge size, causing the parser to hang
-- or attempt massive allocations.
maxTLVSize :: Word32
maxTLVSize = 65536

-- | The Consumer Thread Loop
--
-- * Polls 'write_offset' (atomic acquire).
-- * If new data exists, creates a Lazy ByteString referencing the buffer (Zero-Copy).
-- * Parses frames using 'Data.Binary.Get'.
-- * Updates 'SystemState'.
-- * Logs errors to 'auditQueue'.
consumerLoop :: ForeignPtr RingBufferControl -> TVar SystemState -> IO ()
consumerLoop controlFp stateVar = withForeignPtr controlFp $ \controlPtr -> do
    -- Read initial control block (non-atomic for immutable fields)
    -- We use a dedicated peek to avoid reading atomic offsets (0, 8) which could race.
    (ptrStart, rawSize) <- peekStaticFields controlPtr
    let bufStart = ptrStart
        bufSize  = fromIntegral rawSize :: Int

    -- ForeignPtr to the buffer.
    -- We attach a Haskell finalizer that touches 'controlFp'.
    -- This ensures that as long as 'fp' (and any ByteString derived from it) is alive,
    -- 'controlFp' (and thus the Ring Buffer) remains alive.
    fp <- FC.newForeignPtr bufStart (touchForeignPtr controlFp)

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

                    let (frames, bytesConsumed, maybeErr) = parseStream lbs

                    -- 5. Force Evaluation (Critical for FFI Safety)
                    -- We must ensure all data is copied out of the Ring Buffer (via Lazy ByteString)
                    -- BEFORE we update the read_offset. If we don't, the producer might overwrite
                    -- the memory while we are lazily parsing it.
                    _ <- evaluate (force frames)

                    -- Log Error if detected
                    case maybeErr of
                        Nothing -> return ()
                        Just err -> do
                            -- Construct AuditEvent
                            now <- getMonotonicTimeNS
                            let (sev, msg) = case err of
                                    DoSAttackDetected -> (Critical, "Potential DoS: TLV Too Large")
                                    ParseError m -> (Warning, "Parse Error: " ++ m)
                                    MagicWordMissing -> (Warning, "Sync Lost: Magic Word Missing")
                                    InvalidLength -> (Warning, "Corrupt Packet: Invalid Length")
                                    TlvError m -> (Warning, "TLV Error: " ++ m)
                                    _ -> (Warning, show err)

                            let evt = AuditEvent
                                    { eventTime = now
                                    , severity  = sev
                                    , component = "Consumer"
                                    , message   = msg
                                    }

                            atomically $ do
                                st <- readTVar stateVar
                                writeTBQueue (auditQueue st) evt

                            -- Also print to stderr for immediate feedback during dev
                            hPutStrLn stderr $ "[Consumer] Error: " ++ show err

                    -- 6. Update State
                    -- Link Kalman State & Gating Logic (P1-003)
                    -- We process each frame individually to maintain correct time-steps for the filter.
                    unless (null frames) $ do
                        forM_ frames $ \frame ->
                            processFrame stateVar (points frame)

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
skipToMagicWord = go 0
  where
    go !acc bs =
        case BL.elemIndex 1 bs of
            Nothing -> (acc + BL.length bs, BL.empty) -- No magic word start found, consume all
            Just idx ->
                let candidate = BL.drop idx bs
                in if BL.isPrefixOf magicPattern candidate || BL.length candidate < 8
                   then (acc + idx, candidate) -- Found match or keep partial match
                   else
                       -- Found 0x01 but not followed by correct sequence (Garbage)
                       -- Skip the 0x01 and recurse
                       go (acc + idx + 1) (BL.drop 1 candidate)


-- | Parses a stream of bytes into RadarFrames.
-- Returns the frames, the total bytes consumed, and an optional error.
-- Uses incremental parsing to handle partial frames safely.
-- Optimization: Pre-scans for Magic Word to skip garbage efficiently.
parseStream :: BL.ByteString -> ([RadarFrame], Int64, Maybe HardwareError)
parseStream input =
    let (skipped, cleanInput) = skipToMagicWord input
        (frames, consumed, err) = parseLoop (G.runGetIncremental getRadarFrame) (BL.toChunks cleanInput) 0 []
    in (frames, skipped + consumed, err)
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

            G.Fail _ consumed msg ->
                -- Map failure message to HardwareError
                let advanced = if consumed == 0 then 1 else consumed
                    hwError = case msg of
                        "TLV Too Large" -> DoSAttackDetected
                        "Invalid TLV Length (Partial Header)" -> InvalidLength -- Or TlvError
                        "Invalid Packet Length" -> InvalidLength
                        "Too many TLVs" -> TlvError "Too many TLVs"
                        "Invalid Magic Word" -> MagicWordMissing
                        _ -> ParseError msg
                in (reverse acc, totalConsumed + advanced, Just hwError)

            G.Partial k ->
                case chunks of
                    [] ->
                        -- No more chunks. We are partial.
                        -- Do NOT consume the partial bytes.
                        -- Return only what was fully consumed.
                        (reverse acc, totalConsumed, Nothing)
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
    when (totalLen < 36 || totalLen > 1000000) $
        fail "Invalid Packet Length"

    when (numTLVs > 200) $
        fail "Too many TLVs"

    -- 3. Parse TLVs
    -- Total Header size = 8 + 4*7 = 36 bytes (excluding magic word? No, magic is part of header)
    -- We already consumed Magic (8). Then 7 words (28). Total 36.

    -- We need to parse 'numTLVs'
    points <- parseTLVs (fromIntegral numTLVs)

    return $ RadarFrame B.empty points -- Storing empty raw header for now to save space

-- | Parse TLVs (Tail Recursive)
parseTLVs :: Int -> G.Get [Point3D]
parseTLVs count = go count []
  where
    go 0 acc = return (concat $ reverse acc)
    go n acc = do
        tlvType <- G.getWord32le
        tlvLen <- G.getWord32le

        -- 🛡️ SECURITY FIX: Validate TLV length for ALL types to prevent DoS (P1-002)
        when (tlvLen > maxTLVSize) $ fail "TLV Too Large"
        when (tlvLen < 8) $ fail "Invalid TLV Length (Partial Header)"

        case tlvType of
            1 -> do -- Detected Points
                -- Payload: Array of Point {x,y,z,v} (4 * 4 = 16 bytes)
                -- TI SDK Standard: tlvLen includes Header (8 bytes).
                -- So Payload Length = tlvLen - 8.
                let payloadLen = tlvLen - 8

                -- Num points
                let numPoints = fromIntegral payloadLen `div` 16

                -- Read the points
                points <- getPoints numPoints

                -- SAFETY CHECK: Calculate actual bytes read and skip any remaining (padding/header mismatch)
                let bytesRead = numPoints * 16
                    padding = fromIntegral payloadLen - bytesRead

                when (padding > 0) $ G.skip padding

                go (n - 1) (points : acc)
            _ -> do
                -- Skip unknown TLV
                -- tlvLen includes Header (8 bytes). We already read header.
                let skipLen = fromIntegral (tlvLen - 8)
                G.skip skipLen
                go (n - 1) acc

getPoints :: Int -> G.Get [Point3D]
getPoints count = go count []
  where
    go 0 acc = return (reverse acc)
    go n acc = do
        p <- getPoint
        go (n - 1) (toPoint3D p : acc)

getPoint :: G.Get Point
getPoint = do
    x <- G.getFloatle
    y <- G.getFloatle
    z <- G.getFloatle
    Point x y z <$> G.getFloatle

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
