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
import Control.Monad (unless, when, replicateM)
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
import System.IO (hPutStrLn, stderr)

import FFI.RingBuffer.Types (RingBufferControl(..))
import FFI.RingBuffer.IO (getWriteOffset, setReadOffset)
import Data.Types

-- | The Magic Word sequence for TI Millimeter Wave Radar
magicPattern :: BL.ByteString
magicPattern = BL.pack [1, 2, 3, 4, 5, 6, 7, 8]

-- | The Consumer Thread Loop
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
            writeOff <- getWriteOffset controlFp

            if writeOff == readOff
                then do
                    -- No new data, sleep briefly to avoid busy wait
                    threadDelay 1000 -- 1ms
                    loop readOff
                else do
                    -- 3. Create Zero-Copy Lazy ByteString
                    let lbs = createLazyByteString fp bufSize readOff writeOff

                    -- 4. Parse Frames
                    let (frames, bytesConsumed, corrupted) = parseStream lbs

                    -- 5. Force Evaluation (Critical for FFI Safety)
                    _ <- evaluate (force frames)

                    -- Log corruption if detected
                    when corrupted $ do
                         hPutStrLn stderr "[Consumer] Corrupt Packet detected."

                    -- 6. Update State
                    unless (null frames) $ do
                        atomically $ modifyTVar' stateVar $ \s ->
                            s { currentPoints = concatMap points frames }

                    when (bytesConsumed > 0 && null frames) $
                        putStrLn "[Consumer] Warning: Skipped garbage data (Magic Word search or Parse Error)."

                    -- 7. Update Read Offset
                    let newReadOff = (readOff + fromIntegral bytesConsumed) `rem` bufSize

                    -- 8. Notify Producer (Release Semantics)
                    setReadOffset controlFp newReadOff

                    loop newReadOff

    loop 0

-- | Creates a Lazy ByteString from the ring buffer pointers.
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
skipToMagicWord :: BL.ByteString -> (Int64, BL.ByteString)
skipToMagicWord = go 0
  where
    go !acc bs =
        case BL.elemIndex 1 bs of
            Nothing -> (acc + BL.length bs, BL.empty) -- No magic word start found, consume all
            Just idx ->
                let candidate = BL.drop idx bs
                in if BL.isPrefixOf magicPattern candidate || BL.length candidate < 8
                   then (acc + idx, candidate) -- Found exact match or partial match
                   else
                           -- Found 0x01 but not followed by correct sequence (Garbage)
                           -- Skip the 0x01 and recurse
                           go (acc + idx + 1) (BL.drop 1 candidate)


-- | Parses a stream of bytes into RadarFrames.
parseStream :: BL.ByteString -> ([RadarFrame], Int64, Bool)
parseStream input =
    let (skipped, cleanInput) = skipToMagicWord input
        (frames, consumed, corrupted) = parseLoop (G.runGetIncremental getRadarFrame) (BL.toChunks cleanInput) 0 []
    in (frames, skipped + consumed, corrupted)
  where
    parseLoop decoder chunks totalConsumed acc =
        case decoder of
            G.Done unused consumed frame ->
                let newTotal = totalConsumed + consumed
                    nextDecoder = G.runGetIncremental getRadarFrame
                in if B.null unused
                   then parseLoop nextDecoder chunks newTotal (frame : acc)
                   else parseLoop (G.pushChunk nextDecoder unused) chunks newTotal (frame : acc)

            G.Fail _ consumed _ ->
                let advanced = if consumed == 0 then 1 else consumed
                in (reverse acc, totalConsumed + advanced, True)

            G.Partial k ->
                case chunks of
                    [] -> (reverse acc, totalConsumed, False)
                    (c:cs) -> parseLoop (k (Just c)) cs totalConsumed acc

-- | Parser for a single Radar Frame
getRadarFrame :: G.Get RadarFrame
getRadarFrame = do
    magic <- G.getLazyByteString 8
    unless (magic == magicPattern) $ fail "Invalid Magic Word"

    _version <- G.getWord32le
    totalLen <- G.getWord32le
    _platform <- G.getWord32le
    _frameNum <- G.getWord32le
    _cpuCycles <- G.getWord32le
    numTLVs <- G.getWord32le
    _subFrameNum <- G.getWord32le

    when (totalLen < 36 || totalLen > 1000000) $
       fail "Invalid Packet Length"

    when (numTLVs > 200) $
       fail "Too many TLVs"

    points <- parseTLVs (fromIntegral numTLVs)

    return $ RadarFrame B.empty points

-- | Parse TLVs
parseTLVs :: Int -> G.Get [Point3D]
parseTLVs 0 = return []
parseTLVs n = do
    tlvType <- G.getWord32le
    tlvLen <- G.getWord32le

    case tlvType of
        1 -> do -- Detected Points
            -- Payload Length = tlvLen - 8.
            let payloadLen = if tlvLen >= 8 then tlvLen - 8 else 0

            -- Num points
            let numPoints = fromIntegral payloadLen `div` 16

            -- Read the points
            points <- getPoints numPoints

            -- SAFETY CHECK
            let bytesRead = numPoints * 16
                padding = fromIntegral payloadLen - bytesRead

            when (padding > 0) $ G.skip padding

            rest <- parseTLVs (n - 1)
            return (points ++ rest)
        _ -> do
            -- Skip unknown TLV
            when (tlvLen < 8) $ fail "Invalid TLV Length (Partial Header)"
            let skipLen = fromIntegral (tlvLen - 8)
            G.skip skipLen
            parseTLVs (n - 1)

getPoints :: Int -> G.Get [Point3D]
getPoints n = do
    -- Replaced Vector.replicateM with standard replicateM
    rawPoints <- replicateM n getPoint
    return $ map toPoint3D rawPoints

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
