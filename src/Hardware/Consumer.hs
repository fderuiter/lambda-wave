{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}

module Hardware.Consumer (
    consumerLoop,
    parseStream,
    createLazyByteString
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
import Foreign.Ptr (plusPtr)

import FFI.RingBuffer.Types (RingBufferControl(..), peekStaticFields)
import FFI.RingBuffer.IO (checkoutBlock, releaseBlock, getBlockBytesWritten)
import Data.Types
import Control.Gating (processFrame)
import Control.Mesher (reconstructPolynomialSurface)
import Hardware.Types

magicPattern :: BL.ByteString
magicPattern = BL.pack [1, 2, 3, 4, 5, 6, 7, 8]

maxTLVSize :: Word32
maxTLVSize = 65536

consumerLoop :: Bool -> ForeignPtr RingBufferControl -> TVar SystemState -> IO ()
consumerLoop isPrimary controlFp stateVar = withForeignPtr controlFp $ \controlPtr -> do
    (ptrStart, rawSize) <- peekStaticFields controlPtr
    let bufStart = ptrStart
        bufSize  = fromIntegral rawSize :: Int
        blockSize = bufSize `div` 4

    fp <- FC.newForeignPtr bufStart (touchForeignPtr controlFp)

    putStrLn $ "[Consumer] Started. Primary=" ++ show isPrimary ++ ". Buffer Size: " ++ show bufSize

    -- Internal Loop State. We now maintain 'leftover' bytes across blocks
    -- because frames can be split across block boundaries.
    let loop leftover = do
            maybeBlock <- checkoutBlock controlFp
            case maybeBlock of
                Nothing -> do
                    threadDelay 1000 -- 1ms
                    loop leftover
                Just blockIdx -> do
                    bytesWritten <- getBlockBytesWritten controlFp blockIdx
                    
                    let blockStartOffset = blockIdx * blockSize
                        chunk = BI.fromForeignPtr (castPtr fp) blockStartOffset bytesWritten
                        lbs = BL.append leftover (BL.fromStrict chunk)

                    let (frames, bytesConsumed, maybeErr) = parseStream lbs

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

                            hPutStrLn stderr $ "[Consumer] Error: " ++ show err

                    unless (null frames) $ do
                        if isPrimary
                            then forM_ frames $ \frame -> processFrame stateVar frame
                            else atomically $ modifyTVar' stateVar $ \s -> s { currentPoints = concatMap points frames }

                    when isPrimary $
                        releaseBlock controlFp blockIdx

                    let newLeftover = BL.fromStrict $ B.copy $ BL.toStrict $ BL.drop (fromIntegral bytesConsumed) lbs

                    loop newLeftover

    loop BL.empty

-- The old createLazyByteString is kept to satisfy exports, though unused in the new loop
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

castPtr :: ForeignPtr a -> ForeignPtr Word8
castPtr = castForeignPtr

skipToMagicWord :: BL.ByteString -> (Int64, BL.ByteString)
skipToMagicWord = go 0
  where
    go !acc bs =
        case BL.elemIndex 1 bs of
            Nothing -> (acc + BL.length bs, BL.empty)
            Just idx ->
                let candidate = BL.drop idx bs
                in if BL.isPrefixOf magicPattern candidate || BL.length (BL.take 8 candidate) < 8
                   then (acc + idx, candidate)
                   else go (acc + idx + 1) (BL.drop 1 candidate)

parseStream :: BL.ByteString -> ([RadarFrame], Int64, Maybe HardwareError)
parseStream input =
    let (skipped, cleanInput) = skipToMagicWord input
        (frames, consumed, err) = parseLoop (G.runGetIncremental getRadarFrame) (BL.toChunks cleanInput) 0
    in (frames, skipped + consumed, err)
  where
    parseLoop decoder chunks !totalConsumed =
        case decoder of
            G.Done unused consumed !frame ->
                let !newTotal = totalConsumed + consumed
                    nextDecoder = G.runGetIncremental getRadarFrame
                    (frames, finalConsumed, err) = if B.null unused
                       then parseLoop nextDecoder chunks newTotal
                       else parseLoop (G.pushChunk nextDecoder unused) chunks newTotal
                in (frame : frames, finalConsumed, err)

            G.Fail _ consumed msg ->
                let !advanced = if consumed == 0 then 1 else consumed
                    hwError = case msg of
                        "TLV Too Large" -> DoSAttackDetected
                        "Invalid TLV Length (Partial Header)" -> InvalidLength
                        "Invalid Packet Length" -> InvalidLength
                        "Too many TLVs" -> TlvError "Too many TLVs"
                        "Invalid Magic Word" -> MagicWordMissing
                        _ -> ParseError msg
                in ([], totalConsumed + advanced, Just hwError)

            G.Partial k ->
                case chunks of
                    [] -> ([], totalConsumed, Nothing)
                    (c:cs) -> parseLoop (k (Just c)) cs totalConsumed

getRadarFrame :: G.Get RadarFrame
getRadarFrame = do
    magic <- G.getLazyByteString 8
    unless (magic == magicPattern) $ fail "Invalid Magic Word"

    _version <- G.getWord32le
    totalLen <- G.getWord32le
    _platform <- G.getWord32le
    frameNum <- G.getWord32le
    _cpuCycles <- G.getWord32le
    numTLVs <- G.getWord32le
    _subFrameNum <- G.getWord32le

    when (totalLen < 36 || totalLen > 1000000) $
        fail "Invalid Packet Length"

    when (numTLVs > 200) $
        fail "Too many TLVs"

    let tlvBlockLen = fromIntegral (totalLen - 36)
    points <- G.isolate tlvBlockLen $ parseTLVs (fromIntegral numTLVs)

    return $ RadarFrame B.empty frameNum points

parseTLVs :: Int -> G.Get [Point3D]
parseTLVs count = go count []
  where
    go 0 acc = return (concat $ reverse acc)
    go n acc = do
        tlvType <- G.getWord32le
        tlvLen <- G.getWord32le

        when (tlvLen > maxTLVSize) $ fail "TLV Too Large"
        when (tlvLen < 8) $ fail "Invalid TLV Length (Partial Header)"

        case tlvType of
            1 -> do
                let payloadLen = fromIntegral (tlvLen - 8)
                points <- G.isolate payloadLen $ do
                    let numPoints = payloadLen `div` 16
                    pts <- getPoints numPoints
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
    , snr = 0.0
    }

float2Double :: Float -> Double
float2Double = GHC.Float.float2Double

-- Requirement FR-DAQ-003
-- Hazard H-SYS-002: Sensor disconnection
