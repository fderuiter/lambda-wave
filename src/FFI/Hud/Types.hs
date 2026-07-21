module FFI.Hud.Types (
    HudStateC(..),
    Point3DC(..)
) where

-- SAFETY-CRITICAL
-- | Types for HUD FFI.
-- 
-- Failure Modes:
-- Memory corruption if C struct layout mismatches.
-- 
-- Mitigations:
-- Explicit Storable instances with fixed byte offsets, and continuous layout automated checks.
-- 
-- = Audit Events
-- Validated FFI boundaries produce Info/Warning events on configuration failures.
-- 
-- Traceability: FR-UI-001, FR-UI-002, H-USE-001


import Foreign.Storable (Storable(..))
import Foreign.C.String (CString)
import Foreign.C.Types (CInt(..), CDouble(..), CFloat(..), CSize(..), CBool(..))
import Foreign.Ptr (Ptr)

data Point3DC = Point3DC
    { x :: CDouble
    , y :: CDouble
    , z :: CDouble
    }

instance Storable Point3DC where
    sizeOf _ = 24
    alignment _ = 8
    peek ptr = Point3DC <$> peekByteOff ptr 0 <*> peekByteOff ptr 8 <*> peekByteOff ptr 16
    poke ptr (Point3DC p_x p_y p_z) = do
        pokeByteOff ptr 0 p_x
        pokeByteOff ptr 8 p_y
        pokeByteOff ptr 16 p_z

data HudStateC = HudStateC
    { beamState :: CInt
    , points :: Ptr Point3DC
    , numPoints :: CSize
    , respZ :: CDouble
    , audioAlertEnabled :: CBool
    , activeLanguage :: CString
    , localizedBeamState :: CString
    , calibrationStatus :: CInt
    , beamColorR :: CFloat
    , beamColorG :: CFloat
    , beamColorB :: CFloat
    , traceScaleMin :: CFloat
    , traceScaleMax :: CFloat
    , pointColorR :: CFloat
    , pointColorG :: CFloat
    , pointColorB :: CFloat
    }

instance Storable HudStateC where
    sizeOf _ = 96
    alignment _ = 8
    peek ptr = HudStateC
        <$> peekByteOff ptr 0
        <*> peekByteOff ptr 8
        <*> peekByteOff ptr 16
        <*> peekByteOff ptr 24
        <*> peekByteOff ptr 32
        <*> peekByteOff ptr 40
        <*> peekByteOff ptr 48
        <*> peekByteOff ptr 56
        <*> peekByteOff ptr 60
        <*> peekByteOff ptr 64
        <*> peekByteOff ptr 68
        <*> peekByteOff ptr 72
        <*> peekByteOff ptr 76
        <*> peekByteOff ptr 80
        <*> peekByteOff ptr 84
        <*> peekByteOff ptr 88

    poke ptr (HudStateC bS pT nP rZ aA aL lBS cS bR bG bB tMin tMax pR pG pB) = do
        pokeByteOff ptr 0 bS
        pokeByteOff ptr 8 pT
        pokeByteOff ptr 16 nP
        pokeByteOff ptr 24 rZ
        pokeByteOff ptr 32 aA
        pokeByteOff ptr 40 aL
        pokeByteOff ptr 48 lBS
        pokeByteOff ptr 56 cS
        pokeByteOff ptr 60 bR
        pokeByteOff ptr 64 bG
        pokeByteOff ptr 68 bB
        pokeByteOff ptr 72 tMin
        pokeByteOff ptr 76 tMax
        pokeByteOff ptr 80 pR
        pokeByteOff ptr 84 pG
        pokeByteOff ptr 88 pB
