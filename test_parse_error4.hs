import qualified Data.ByteString.Lazy as BL
import qualified Data.Binary.Put as P
import qualified Data.ByteString as B
import Hardware.Consumer
import Data.Types

main :: IO ()
main = do
    let point = Point 1.0 2.0 3.0 4.0
        testPoints = [point]
        magic = mapM_ P.putWord8 [1, 2, 3, 4, 5, 6, 7, 8]
        testHeader = do
            P.putWord32le 0; P.putWord32le 64; P.putWord32le 0; P.putWord32le 1
            P.putWord32le 0; P.putWord32le 1; P.putWord32le 0
        tlv = do
            P.putWord32le 1; P.putWord32le 28; mapM_ putPoint testPoints
            P.putWord32le 0xDEADBEEF
        putPoint (Point x y z vel) = do
            P.putFloatle x; P.putFloatle y; P.putFloatle z; P.putFloatle vel
        payload = P.runPut (magic >> testHeader >> tlv)
        payload2 = payload <> payload
        (frames, consumed, err) = parseStream payload2
    print frames
    print consumed
    print err
