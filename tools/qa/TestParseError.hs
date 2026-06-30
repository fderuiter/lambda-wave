module Main (main) where

import qualified Data.Binary.Put as P
import Hardware.Consumer
import Data.Types

main :: IO ()
main = do
    let point = Point 1.0 2.0 3.0 4.0
        testPoints = [point]
        magic = mapM_ P.putWord8 [1, 2, 3, 4, 5, 6, 7, 8]
        unknownTlv = do
            P.putWord32le 999; P.putWord32le 20
            P.putWord32le 0xAAAAAAAA; P.putWord32le 0xBBBBBBBB; P.putWord32le 0xCCCCCCCC
        validTlv = do
            P.putWord32le 1; P.putWord32le 24; mapM_ putPoint testPoints
        putPoint (Point x y z vel) = do
            P.putFloatle x; P.putFloatle y; P.putFloatle z; P.putFloatle vel
        hdr = do
            P.putWord32le 0; P.putWord32le 80; P.putWord32le 0; P.putWord32le 1
            P.putWord32le 0; P.putWord32le 2; P.putWord32le 0
        payload = P.runPut (magic >> hdr >> unknownTlv >> validTlv)
        (frames, consumed, err) = parseStream 0.0 payload
    print frames
    print consumed
    print err
