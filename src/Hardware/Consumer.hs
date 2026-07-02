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
import Data.I18n (Translations)
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
import qualified GHC.Float
import Data.Time.HighRes (getMonotonicTimeNS)
import Data.Maybe (isJust)

import FFI.RingBuffer.Types (RingBufferControl(..), peekStaticFields)
import FFI.RingBuffer.IO (getWriteOffset, setReadOffset)
import FFI.RingBuffer.Generated (c_rb_available_data, c_rb_next_read_offset)
import Data.Types
import Control.Gating (processFrame)
import Control.Mesher (reconstructPolynomialSurface)
import Hardware.Types
import Hardware.Control (setBeam)
import Hardware.FFI.Bridge (handleHardwareResponse)
import Text.Printf (printf)

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

consumerLoop :: Double -> Translations -> Bool -> ForeignPtr RingBufferControl -> TVar SystemState -> IO ()
consumerLoop mountingOffset translations isPrimary controlFp stateVar = withForeignPtr controlFp $ \controlPtr -> do
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

    putStrLn $ "[Consumer] Started. Primary=" ++ show isPrimary ++ ". Buffer Size: " ++ show bufSize

    -- Internal Loop State
    let loop readOff = do
            -- 1. Poll Write Offset (Atomic Acquire)
            -- Pass the ForeignPtr to ensure safety, although we are already inside withForeignPtr,
            -- this double check is fine or we rely on the fact that controlFp is alive.
            writeOff <- getWriteOffset controlFp

            -- 2. Calculate available data via FFI (using C++ master logic)
            availableBytesC <- c_rb_available_data controlPtr (fromIntegral readOff)
            let availableBytes = fromIntegral availableBytesC :: Int

            if availableBytes == 0
                then do
                    -- No new data, sleep briefly to avoid busy wait
                    threadDelay 1000 -- 1ms
                    loop readOff
                else do
                    let saturation = fromIntegral availableBytes / fromIntegral bufSize :: Double

                    when (saturation >= 0.90) $ do
                        now <- getMonotonicTimeNS
                        let evt = AuditEvent
                                { eventTime = now
                                , severity  = Critical
                                , component = "Consumer"
                                , message   = "Buffer pressure reached " ++ show (round (saturation * 100) :: Int) ++ "% saturation. Triggering Beam Off."
                                }
                        atomically $ do
                            st <- readTVar stateVar
                            writeTBQueue (auditQueue st) evt
                            writeTVar stateVar (st { beamState = BeamOff })
                        when isPrimary $ do
                            res <- setBeam stateVar False
                            handleHardwareResponse
                                (\err -> do
                                    let evt2 = AuditEvent now Critical "Hardware" ("Actuation Error: " ++ show err)
                                    atomically $ do
                                        st2 <- readTVar stateVar
                                        writeTBQueue (auditQueue st2) evt2
                                )
                                (\() -> return ())
                                res

                    -- 3. Create Zero-Copy Lazy ByteString
                    let lbs = createLazyByteString fp bufSize readOff writeOff

                    -- 4. Parse Frames
                    -- We use 'runGetIncremental' to handle the stream.
                    -- Note: Since we poll chunks, we might get partial frames.
                    -- However, 'runGetIncremental' expects to be fed.
                    -- Here, we simplify by attempting to parse as much as possible
                    -- from the current snapshot. A robust implementation would maintain
                    -- the decoder state across loops.

                    let (frames, bytesConsumed, maybeErr) = parseStream mountingOffset lbs

                    -- Check for Busy Loop Condition:
                    -- If we consumed 0 bytes and have no error, it means we are stuck
                    -- (likely due to a partial magic word at the end of the buffer).
                    -- We MUST sleep to allow the producer to add more data.
                    unless (bytesConsumed > 0 || isJust maybeErr) $
                        threadDelay 1000 -- 1ms

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
                                writeTVar stateVar (st { beamState = BeamOff })

                            when isPrimary $ do
                                res <- setBeam stateVar False
                                handleHardwareResponse
                                    (\errHardware -> do
                                        let evt2 = AuditEvent now Critical "Hardware" ("Actuation Error: " ++ show errHardware)
                                        atomically $ do
                                            st2 <- readTVar stateVar
                                            writeTBQueue (auditQueue st2) evt2
                                    )
                                    (\() -> return ())
                                    res

                            -- Also print to stderr for immediate feedback during dev
                            hPutStrLn stderr $ "[Consumer] Error: " ++ show err

                    -- 6. Update State
                    -- Link Kalman State & Gating Logic (P1-003)
                    -- We process each frame individually to maintain correct time-steps for the filter.
                    unless (null frames) $ do
                        forM_ frames $ \frame -> do
                            now <- getMonotonicTimeNS
                            let headerHex = concatMap (\x -> printf "%02X" (fromIntegral x :: Int)) (B.unpack (header frame))
                            let evt = AuditEvent
                                    { eventTime = now
                                    , severity  = Info
                                    , component = "Consumer"
                                    , message   = "Processed frame " ++ show (seqNum frame) ++ " | Header: " ++ headerHex
                                    }
                            atomically $ do
                                st <- readTVar stateVar
                                writeTBQueue (auditQueue st) evt

                        if isPrimary
                            then forM_ frames $ \frame -> processFrame translations stateVar frame
                            else atomically $ modifyTVar' stateVar $ \s -> s { currentPoints = concatMap points frames }

                    -- 7. Update Read Offset via FFI Master Logic
                    let safeConsumed = max 0 bytesConsumed :: Int64
                    newReadOffC <- c_rb_next_read_offset controlPtr (fromIntegral readOff) (fromIntegral safeConsumed)
                    let newReadOff = fromIntegral newReadOffC :: Int

                    -- 8. Notify Producer (Release Semantics)
                    -- We must update the shared read offset so the producer can reclaim space
                    -- (if it implements flow control) or just for monitoring.
                    -- ONLY the Primary Consumer (SafetyCore) gets to update the flow control offset!
                    when isPrimary $
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
                -- ⚡ Bolt Optimization: Replace `BL.length candidate < 8` with `BL.length (BL.take 8 candidate) < 8`
                -- to avoid forcing an O(N) evaluation of the entire chunk chain just to check length. Restores O(1) streaming performance.
                in if BL.isPrefixOf magicPattern candidate || BL.length (BL.take 8 candidate) < 8
                   then (acc + idx, candidate) -- Found match or keep partial match
                   else
                       -- Found 0x01 but not followed by correct sequence (Garbage)
                       -- Skip the 0x01 and recurse
                       go (acc + idx + 1) (BL.drop 1 candidate)


-- | Parses a stream of bytes into RadarFrames.
-- Returns the frames, the total bytes consumed, and an optional error.
-- Uses incremental parsing to handle partial frames safely.
-- Optimization: Pre-scans for Magic Word to skip garbage efficiently.
parseStream :: Double -> BL.ByteString -> ([RadarFrame], Int64, Maybe HardwareError)
parseStream mountingOffset input =
    let (skipped, cleanInput) = skipToMagicWord input
        (frames, consumed, err) = parseLoop (G.runGetIncremental (getRadarFrame mountingOffset)) (BL.toChunks cleanInput) 0
    in (frames, skipped + consumed, err)
  where
    parseLoop decoder chunks !totalConsumed =
        case decoder of
            G.Done unused consumed !frame ->
                -- Frame parsed!
                -- 'consumed' is bytes consumed by THIS decoder instance since start.
                -- 'unused' is the part of the LAST chunk that wasn't used.
                -- We need to proceed with 'unused' + remaining 'chunks'.
                let !newTotal = totalConsumed + consumed
                    nextDecoder = G.runGetIncremental (getRadarFrame mountingOffset)
                    -- We need to construct the input for the next step.
                    -- 'unused' is a ByteString.
                    (frames, finalConsumed, err) = if B.null unused
                       then parseLoop nextDecoder chunks newTotal
                       else parseLoop (G.pushChunk nextDecoder unused) chunks newTotal
                in (frame : frames, finalConsumed, err)

            G.Fail _ consumed msg ->
                -- Map failure message to HardwareError
                let !advanced = if consumed == 0 then 1 else consumed
                    hwError = case msg of
                        "TLV Too Large" -> DoSAttackDetected
                        "Invalid TLV Length (Partial Header)" -> InvalidLength -- Or TlvError
                        "Invalid Packet Length" -> InvalidLength
                        "Too many TLVs" -> TlvError "Too many TLVs"
                        "Invalid Magic Word" -> MagicWordMissing
                        _ -> ParseError msg
                in ([], totalConsumed + advanced, Just hwError)

            G.Partial k ->
                case chunks of
                    [] ->
                        -- No more chunks. We are partial.
                        -- Do NOT consume the partial bytes.
                        -- Return only what was fully consumed.
                        ([], totalConsumed, Nothing)
                    (c:cs) ->
                        -- Feed next chunk
                        parseLoop (k (Just c)) cs totalConsumed

-- | Parser for a single Radar Frame
getRadarFrame :: Double -> G.Get RadarFrame
getRadarFrame mountingOffset = do
    rawHeader <- B.copy <$> G.lookAhead (G.getByteString 36)

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
    frameNum <- G.getWord32le
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
    -- 🛡️ SECURITY FIX: Enforce boundary for the whole TLV block to prevent out-of-bounds reads
    -- P1-002: TLV Parser DoS
    let tlvBlockLen = fromIntegral (totalLen - 36)
    points <- G.isolate tlvBlockLen $ parseTLVs mountingOffset (fromIntegral numTLVs)

    -- SENTINEL SAFETY NOTE: We keep 'header' empty or must copy it.
    -- Holding a ByteString pointing to the Ring Buffer (ForeignPtr) while
    -- advancing read_offset allows the producer to overwrite the memory,
    -- leading to race conditions if we read that ByteString later.
    return $ RadarFrame rawHeader frameNum points

-- | Parse TLVs (Tail Recursive)
parseTLVs :: Double -> Int -> G.Get [Point3D]
parseTLVs mountingOffset count = go count []
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
                let payloadLen = fromIntegral (tlvLen - 8)

                points <- G.isolate payloadLen $ do
                    -- Num points
                    let numPoints = payloadLen `div` 16

                    -- Read the points
                    pts <- getPoints mountingOffset numPoints

                    -- Explicitly consume any remaining bytes to satisfy G.isolate strictness
                    _padding <- G.getRemainingLazyByteString
                    return pts

                go (n - 1) (points : acc)
            2 -> do -- Surface Coefficients
                let payloadLen = fromIntegral (tlvLen - 8)
                points <- G.isolate payloadLen $ do
                    c0 <- G.getFloatle
                    c1 <- G.getFloatle
                    c2 <- G.getFloatle
                    c3 <- G.getFloatle
                    c4 <- G.getFloatle
                    c5 <- G.getFloatle
                    _padding <- G.getRemainingLazyByteString
                    let coeffs = [float2Double c0, float2Double c1, float2Double c2, float2Double c3, float2Double c4, float2Double c5]
                    let rawPts = reconstructPolynomialSurface coeffs
                    let pts = map (\p -> p { pz = pz p + mountingOffset }) rawPts
                    if null pts then fail "NaN/Inf detected in surface reconstruction" else return pts

                go (n - 1) (points : acc)
            _ -> do
                -- Skip unknown TLV
                -- tlvLen includes Header (8 bytes). We already read header.
                let payloadLen = fromIntegral (tlvLen - 8)
                G.isolate payloadLen $ do
                    _padding <- G.getRemainingLazyByteString
                    return ()
                go (n - 1) acc

getPoints :: Double -> Int -> G.Get [Point3D]
getPoints mountingOffset count = go count []
  where
    go 0 acc = return (reverse acc)
    go n acc = do
        p <- getPoint
        go (n - 1) (toPoint3D mountingOffset p : acc)

getPoint :: G.Get Point
getPoint = do
    x <- G.getFloatle
    y <- G.getFloatle
    z <- G.getFloatle
    Point x y z <$> G.getFloatle

toPoint3D :: Double -> Point -> Point3D
toPoint3D mountingOffset Point{..} = Point3D
    { px = float2Double px'
    , py = float2Double py'
    , pz = float2Double pz' + mountingOffset
    , v  = float2Double v'
    , snr = 0.0 -- Not in Type 1
    }

-- ⚡ Bolt Optimization: Use specialized native GHC.Float.float2Double
-- instead of realToFrac to avoid type class dictionary lookups,
-- minimizing overhead and intermediate allocations during parsing.
float2Double :: Float -> Double
float2Double = GHC.Float.float2Double

-- Requirement FR-DAQ-003
-- Hazard H-SYS-002: Sensor disconnection
