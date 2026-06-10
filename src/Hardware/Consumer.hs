{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}

{-|
Module      : Hardware.Consumer
Description : Zero-Copy Consumer for Ring Buffer
Copyright   : (c) 2024
License     : AGPL-3.0-only

This module implements the core execution loop that reads from the UART into the ring buffer
and parses the incoming data stream into Haskell types within a single process.
-}
module Hardware.Consumer (
    coreLoop,
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
import qualified GHC.Float
import Data.Time.HighRes (getMonotonicTimeNS)
import Data.Maybe (isJust)
import System.Posix.Types (Fd)
import System.Exit (exitFailure)

import FFI.RingBuffer.Types (RingBufferControl(..), peekStaticFields)
import FFI.RingBuffer.IO (getWriteOffset, setReadOffset, readFromUart, ReadResult(..))
import Data.Types
import Control.Gating (processFrame)
import Control.Mesher (reconstructPolynomialSurface)
import Hardware.Types
import Hardware.Control (setBeam)
import Safety.Watchdog (checkWatchdog)
import Text.Printf (printf)

-- | The Magic Word sequence for TI Millimeter Wave Radar
magicPattern :: BL.ByteString
magicPattern = BL.pack [1, 2, 3, 4, 5, 6, 7, 8]

-- | Maximum allowed TLV size to prevent Denial of Service (DoS) attacks
maxTLVSize :: Word32
maxTLVSize = 65536

-- | The Core Execution Loop
--
-- * Reads from UART.
-- * Polls 'write_offset' (atomic acquire).
-- * If new data exists, creates a Lazy ByteString referencing the buffer (Zero-Copy).
-- * Parses frames using 'Data.Binary.Get'.
-- * Updates 'SystemState'.
-- * Evaluates Watchdog
-- * Logs errors to 'auditQueue'.
coreLoop :: Bool -> ForeignPtr RingBufferControl -> Fd -> TVar SystemState -> IO ()
coreLoop isPrimary controlFp fd stateVar = withForeignPtr controlFp $ \controlPtr -> do
    (ptrStart, rawSize) <- peekStaticFields controlPtr
    let bufStart = ptrStart
        bufSize  = fromIntegral rawSize :: Int

    fp <- FC.newForeignPtr bufStart (touchForeignPtr controlFp)

    putStrLn $ "[CoreLoop] Started. Primary=" ++ show isPrimary ++ ". Buffer Size: " ++ show bufSize

    let loop readOff = do
            -- 1. Read from UART (Non-blocking or fast blocking)
            res <- readFromUart controlFp fd
            case res of
                ReadError -> do
                    hPutStrLn stderr "CRITICAL FAILURE: readFromUart returned error. Core loop TERMINATING."
                    exitFailure
                ReadEOF -> do
                    hPutStrLn stderr "Device Disconnected (EOF). Terminating."
                    exitFailure
                _ -> return ()

            -- 2. Poll Write Offset
            writeOff <- getWriteOffset controlFp

            if writeOff == readOff
                then do
                    when isPrimary $ checkWatchdog stateVar
                    threadDelay 1000 -- 1ms
                    loop readOff
                else do
                    let availableBytes = if writeOff >= readOff
                                         then writeOff - readOff
                                         else bufSize - readOff + writeOff
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
                        when isPrimary $ setBeam False

                    let lbs = createLazyByteString fp bufSize readOff writeOff
                    let (frames, bytesConsumed, maybeErr) = parseStream lbs

                    unless (bytesConsumed > 0 || isJust maybeErr) $ do
                        when isPrimary $ checkWatchdog stateVar
                        threadDelay 1000 -- 1ms

                    _ <- evaluate (force frames)

                    case maybeErr of
                        Nothing -> return ()
                        Just err -> do
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

                            when isPrimary $ setBeam False
                            hPutStrLn stderr $ "[CoreLoop] Error: " ++ show err

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
                            then forM_ frames $ \frame -> processFrame stateVar frame
                            else atomically $ modifyTVar' stateVar $ \s -> s { currentPoints = concatMap points frames }

                    let safeConsumed = max 0 (fromIntegral bytesConsumed)
                    let newReadOff = (readOff + safeConsumed) `rem` bufSize

                    when isPrimary $ do
                        setReadOffset controlFp newReadOff
                        checkWatchdog stateVar

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
parseStream :: BL.ByteString -> ([RadarFrame], Int64, Maybe HardwareError)
parseStream input =
    let (skipped, cleanInput) = skipToMagicWord input
        (frames, consumed, err) = parseLoop (G.runGetIncremental getRadarFrame) (BL.toChunks cleanInput) 0
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
                    nextDecoder = G.runGetIncremental getRadarFrame
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
getRadarFrame :: G.Get RadarFrame
getRadarFrame = do
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
    points <- G.isolate tlvBlockLen $ parseTLVs (fromIntegral numTLVs)

    -- SENTINEL SAFETY NOTE: We keep 'header' empty or must copy it.
    -- Holding a ByteString pointing to the Ring Buffer (ForeignPtr) while
    -- advancing read_offset allows the producer to overwrite the memory,
    -- leading to race conditions if we read that ByteString later.
    return $ RadarFrame rawHeader frameNum points

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
                let payloadLen = fromIntegral (tlvLen - 8)

                points <- G.isolate payloadLen $ do
                    -- Num points
                    let numPoints = payloadLen `div` 16

                    -- Read the points
                    pts <- getPoints numPoints

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
                    return (reconstructPolynomialSurface coeffs)

                go (n - 1) (points : acc)
            _ -> do
                -- Skip unknown TLV
                -- tlvLen includes Header (8 bytes). We already read header.
                let payloadLen = fromIntegral (tlvLen - 8)
                G.isolate payloadLen $ do
                    _padding <- G.getRemainingLazyByteString
                    return ()
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

-- ⚡ Bolt Optimization: Use specialized native GHC.Float.float2Double
-- instead of realToFrac to avoid type class dictionary lookups,
-- minimizing overhead and intermediate allocations during parsing.
float2Double :: Float -> Double
float2Double = GHC.Float.float2Double

-- Requirement FR-DAQ-003
-- Hazard H-SYS-002: Sensor disconnection
