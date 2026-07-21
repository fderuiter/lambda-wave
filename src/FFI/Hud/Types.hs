module FFI.Hud.Types (
    HudStateC(..),
    Point3DC(..)
) where

-- | Types for HUD FFI.
-- 
-- Failure Modes: Memory corruption if C struct layout mismatches.
-- Mitigations: Explicit Storable instances with fixed byte offsets.
-- Traceability: REQ-HUD-001


import Foreign.Storable (Storable(..))
import Foreign.C.String (CString)
import Foreign.Ptr (Ptr)
import Data.Word (Word64, Word32, Word8)

data Point3DC = Point3DC Double Double Double

instance Storable Point3DC where
    sizeOf _ = 24
    alignment _ = 8
    peek ptr = Point3DC <$> peekByteOff ptr 0 <*> peekByteOff ptr 8 <*> peekByteOff ptr 16
    poke ptr (Point3DC p_x p_y p_z) = do
        pokeByteOff ptr 0 p_x
        pokeByteOff ptr 8 p_y
        pokeByteOff ptr 16 p_z

data HudStateC = HudStateC
    { hscBeamState :: Word32
    , hscPoints :: Ptr Point3DC
    , hscNumPoints :: Word64
    , hscRespZ :: Double
    , hscAudioAlertEnabled :: Word8
    , hscCalibrationStatus :: Word32
    , hscBeamColorR :: Float
    , hscBeamColorG :: Float
    , hscBeamColorB :: Float
    , hscTraceScaleMin :: Float
    , hscTraceScaleMax :: Float
    , hscPointColorR :: Float
    , hscPointColorG :: Float
    , hscPointColorB :: Float
    }

instance Storable HudStateC where
    sizeOf _ = 72
    alignment _ = 8
    peek ptr = HudStateC
        <$> peekByteOff ptr 0
        <*> peekByteOff ptr 8
        <*> peekByteOff ptr 16
        <*> peekByteOff ptr 24
        <*> peekByteOff ptr 32
        <*> peekByteOff ptr 36
        <*> peekByteOff ptr 40
        <*> peekByteOff ptr 44
        <*> peekByteOff ptr 48
        <*> peekByteOff ptr 52
        <*> peekByteOff ptr 56
        <*> peekByteOff ptr 60
        <*> peekByteOff ptr 64
        <*> peekByteOff ptr 68

    poke ptr (HudStateC bS pT nP rZ aA cS bR bG bB tMin tMax pR pG pB) = do
        pokeByteOff ptr 0 bS
        pokeByteOff ptr 8 pT
        pokeByteOff ptr 16 nP
        pokeByteOff ptr 24 rZ
        pokeByteOff ptr 32 aA
        pokeByteOff ptr 36 cS
        pokeByteOff ptr 40 bR
        pokeByteOff ptr 44 bG
        pokeByteOff ptr 48 bB
        pokeByteOff ptr 52 tMin
        pokeByteOff ptr 56 tMax
        pokeByteOff ptr 60 pR
        pokeByteOff ptr 64 pG
        pokeByteOff ptr 68 pB
