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
consumerLoop :: ForeignPtr RingBufferControl -> TVar SystemState -> IO ()
consumerLoop controlFp stateVar = withForeignPtr controlFp $ \controlPtr -> do
    ctrl <- peek controlPtr
    let bufStart = bufferStart ctrl
        bufSize  = fromIntegral (bufferSize ctrl) :: Int
    fp <- newForeignPtr_ bufStart

    putStrLn $ "[Consumer] Started. Buffer Size: " ++ show bufSize

    let loop readOff = do
            writeOff <- getWriteOffset controlFp

            if writeOff == readOff
                then do
                    threadDelay 1000 -- 1ms
                    loop readOff
                else do
                    let lbs = createLazyByteString fp bufSize readOff writeOff
                    let (frames, bytesConsumed) = parseStream lbs

                    _ <- evaluate (force frames)

                    unless (null frames) $ do
                        atomically $ modifyTVar' stateVar $ \s ->
                            s { currentPoints = concatMap points frames }

                    let newReadOff = (readOff + fromIntegral bytesConsumed) `rem` bufSize
                    setReadOffset controlFp newReadOff

                    -- Avoid busy-wait if we have data but couldn't parse a full frame (Partial)
                    if bytesConsumed == 0
                        then do
                            threadDelay 100 -- 0.1ms (Aggressive poll but not 100% CPU)
                            loop newReadOff
                        else loop newReadOff

    loop 0

-- | Creates a Lazy ByteString from the ring buffer pointers.
createLazyByteString :: ForeignPtr CChar -> Int -> Int -> Int -> BL.ByteString
createLazyByteString fp bufSize readOff writeOff =
    if writeOff >= readOff
    then
        let len = writeOff - readOff
            chunk = BI.fromForeignPtr (castPtr fp) readOff len
        in BL.fromStrict chunk
    else
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
-- Uses BL.elemIndex for efficient Magic Word search.
parseStream :: BL.ByteString -> ([RadarFrame], Int64)
parseStream input = go input 0 []
  where
    magicWord = BL.pack [1, 2, 3, 4, 5, 6, 7, 8]
    magicLen = 8
    headerMinSize = 20 -- Magic(8) + Ver(4) + Len(4) + Plat(4) at least
    maxFrameSize = 1000000 -- Sanity check (1MB)

    go :: BL.ByteString -> Int64 -> [RadarFrame] -> ([RadarFrame], Int64)
    go bs consumed acc =
        case BL.elemIndex 1 bs of
            Nothing ->
                -- Magic byte not found. Consume all.
                (reverse acc, consumed + BL.length bs)
            Just offset ->
                let bsAtMagic = BL.drop offset bs
                    -- Check if we have enough data to verify Magic Word
                    available = BL.length bsAtMagic
                in if available < magicLen
                   then
                       -- Found partial magic at end. Stop here.
                       -- Do NOT consume the partial magic.
                       (reverse acc, consumed + offset)
                   else
                       -- Verify full Magic Word
                       if BL.take magicLen bsAtMagic == magicWord
                       then
                           -- Magic matches. Check Header.
                           if available < 32 -- Full header size approx
                           then (reverse acc, consumed + offset)
                           else
                               -- Read Packet Length (Offset 12: Magic 8 + Version 4)
                               let lenBytes = BL.take 4 (BL.drop 12 bsAtMagic)
                                   packetLen = fromIntegral (G.runGet G.getWord32le lenBytes) :: Int64
                               in if packetLen < 32 || packetLen > maxFrameSize
                                  then
                                      -- Invalid length. Skip 1 byte (false positive)
                                      go (BL.drop (offset + 1) bs) (consumed + offset + 1) acc
                                  else if available < packetLen
                                      then
                                          -- Partial frame. Stop.
                                          (reverse acc, consumed + offset)
                                      else
                                          -- Full frame available. Parse it.
                                          let frameBytes = BL.take packetLen bsAtMagic
                                          in case G.runGetOrFail getRadarFrameBody frameBytes of
                                              Right (_, _, frame) ->
                                                  -- Success
                                                  go (BL.drop packetLen bsAtMagic) (consumed + offset + packetLen) (frame : acc)
                                              Left _ ->
                                                  -- Parse error inside frame.
                                                  -- Skip the whole claimed frame to avoid loops?
                                                  -- Or skip 1 byte?
                                                  -- Given 64-bit magic match, better skip frame or at least magic.
                                                  -- Let's skip magicLen to retry search.
                                                  go (BL.drop magicLen bsAtMagic) (consumed + offset + magicLen) acc
                       else
                           -- Magic mismatch. Skip 1 byte.
                           go (BL.drop (offset + 1) bs) (consumed + offset + 1) acc

-- | Parser for Radar Frame Body (Assumes Magic Word is present at start or stripped?
-- Actually, we pass the WHOLE frameBytes including Magic Word to getRadarFrameBody
-- because the packetLen includes it.
-- So we need to skip Magic inside.
getRadarFrameBody :: G.Get RadarFrame
getRadarFrameBody = do
    -- 1. Skip Magic (we already verified it)
    G.skip 8

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

-- | Parse TLVs
parseTLVs :: Int -> G.Get [Point3D]
parseTLVs 0 = return []
parseTLVs n = do
    tlvType <- G.getWord32le
    tlvLen <- G.getWord32le

    -- tlvLen includes Type(4) + Len(4) = 8 bytes?
    -- TI Standard: TLV length usually includes the header.
    -- However, we must be careful not to read past frame end.
    -- The caller ensures input is limited to frame size.

    case tlvType of
        1 -> do -- Detected Points
            -- Payload size = tlvLen - 8
            let payloadLen = if tlvLen >= 8 then fromIntegral tlvLen - 8 else 0
            let numPoints = payloadLen `div` 16
            points <- getPoints numPoints
            -- If there is padding or mismatch, skip remainder
            let consumed = numPoints * 16
            let remaining = payloadLen - consumed
            G.skip remaining

            rest <- parseTLVs (n - 1)
            return (points ++ rest)
        _ -> do
            -- Skip unknown TLV
            let payloadLen = if tlvLen >= 8 then fromIntegral tlvLen - 8 else 0
            G.skip payloadLen
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
    , snr = 0.0
    }

float2Double :: Float -> Double
float2Double = realToFrac
