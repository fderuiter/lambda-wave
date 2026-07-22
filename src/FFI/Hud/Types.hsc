{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface #-}
module FFI.Hud.Types (
    HudStateC(..),
    Point3DC(..)
) where

import Foreign.Storable (Storable(..))
import Foreign.C.String (CString)
import Foreign.Ptr (Ptr)
import Data.Word (Word64, Word32, Word8)

#include "hud.h"

data Point3DC = Point3DC Double Double Double

instance Storable Point3DC where
    sizeOf _ = #{size Point3DC}
    alignment _ = #{alignment Point3DC}
    peek ptr = Point3DC 
        <$> #{peek Point3DC, x} ptr
        <*> #{peek Point3DC, y} ptr
        <*> #{peek Point3DC, z} ptr
    poke ptr (Point3DC p_x p_y p_z) = do
        #{poke Point3DC, x} ptr p_x
        #{poke Point3DC, y} ptr p_y
        #{poke Point3DC, z} ptr p_z

data HudStateC = HudStateC
    { hscBeamState :: Word32
    , hscPoints :: Ptr Point3DC
    , hscNumPoints :: Word64
    , hscRespZ :: Double
    , hscAudioAlertEnabled :: Word8
    , hscActiveLanguage :: CString
    , hscLocalizedBeamState :: CString
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
    sizeOf _ = #{size HudStateC}
    alignment _ = #{alignment HudStateC}
    peek ptr = HudStateC
        <$> #{peek HudStateC, beam_state} ptr
        <*> #{peek HudStateC, points} ptr
        <*> #{peek HudStateC, num_points} ptr
        <*> #{peek HudStateC, resp_z} ptr
        <*> #{peek HudStateC, audio_alert_enabled} ptr
        <*> #{peek HudStateC, active_language} ptr
        <*> #{peek HudStateC, localized_beam_state} ptr
        <*> #{peek HudStateC, calibration_status} ptr
        <*> #{peek HudStateC, beam_color_r} ptr
        <*> #{peek HudStateC, beam_color_g} ptr
        <*> #{peek HudStateC, beam_color_b} ptr
        <*> #{peek HudStateC, trace_scale_min} ptr
        <*> #{peek HudStateC, trace_scale_max} ptr
        <*> #{peek HudStateC, point_color_r} ptr
        <*> #{peek HudStateC, point_color_g} ptr
        <*> #{peek HudStateC, point_color_b} ptr

    poke ptr (HudStateC bS pT nP rZ aA aL lBS cS bR bG bB tMin tMax pR pG pB) = do
        #{poke HudStateC, beam_state} ptr bS
        #{poke HudStateC, points} ptr pT
        #{poke HudStateC, num_points} ptr nP
        #{poke HudStateC, resp_z} ptr rZ
        #{poke HudStateC, audio_alert_enabled} ptr aA
        #{poke HudStateC, active_language} ptr aL
        #{poke HudStateC, localized_beam_state} ptr lBS
        #{poke HudStateC, calibration_status} ptr cS
        #{poke HudStateC, beam_color_r} ptr bR
        #{poke HudStateC, beam_color_g} ptr bG
        #{poke HudStateC, beam_color_b} ptr bB
        #{poke HudStateC, trace_scale_min} ptr tMin
        #{poke HudStateC, trace_scale_max} ptr tMax
        #{poke HudStateC, point_color_r} ptr pR
        #{poke HudStateC, point_color_g} ptr pG
        #{poke HudStateC, point_color_b} ptr pB
