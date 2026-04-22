{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE CApiFFI #-}

{-|
Module      : Data.Time.HighRes
Description : High-Resolution Monotonic Time via FFI
Copyright   : (c) 2024
License     : AGPL-3.0-only

Provides access to CLOCK_MONOTONIC for nanosecond-precision timing.
Replaces the 'clock' dependency.
-}
module Data.Time.HighRes (
    getMonotonicTimeNS,
    getRealTimeNS
) where

import Data.Int (Int32)
import Foreign.C.Types
import Foreign.Ptr
import Foreign.Storable
import Data.Word (Word64)
import Foreign.Marshal.Alloc (alloca)
import Control.Monad (when)
import Foreign.C.Error (throwErrno)

-- | Corresponds to C 'struct timespec'
data TimeSpec = TimeSpec
    { _sec  :: {-# UNPACK #-} !CTime
    , _nsec :: {-# UNPACK #-} !CLong
    }


instance Storable TimeSpec where
    alignment _ = max (alignment (0 :: CTime)) (alignment (0 :: CLong))
    sizeOf _ =
        let s = sizeOf (0 :: CTime)
            n = sizeOf (0 :: CLong)
            a = alignment (TimeSpec 0 0)
            n_align = alignment (0 :: CLong)
            padding_s = (n_align - (s `rem` n_align)) `rem` n_align
            total = s + padding_s + n
            tail_padding = (a - (total `rem` a)) `rem` a
        in total + tail_padding

    peek ptr = do
        let s_size = sizeOf (0 :: CTime)
            n_align = alignment (0 :: CLong)
            padding = (n_align - (s_size `rem` n_align)) `rem` n_align
            n_offset = s_size + padding
        s <- peekByteOff ptr 0
        n <- peekByteOff ptr n_offset
        return (TimeSpec s n)

    poke ptr (TimeSpec s n) = do
        let s_size = sizeOf (0 :: CTime)
            n_align = alignment (0 :: CLong)
            padding = (n_align - (s_size `rem` n_align)) `rem` n_align
            n_offset = s_size + padding
        pokeByteOff ptr 0 s
        pokeByteOff ptr n_offset n

-- | Clock ID for CLOCK_REALTIME (0) and CLOCK_MONOTONIC (1)
-- These vary by OS, but 1 is standard for Monotonic on Linux.
-- We use CPP or just hardcode for standard Linux/Posix.
foreign import ccall unsafe "time.h clock_gettime"
    c_clock_gettime :: Int32 -> Ptr TimeSpec -> IO CInt

clockMonotonic :: Int32
clockMonotonic = 1

clockRealtime :: Int32
clockRealtime = 0

-- | Get Monotonic Time in Nanoseconds (Word64)
getMonotonicTimeNS :: IO Word64
getMonotonicTimeNS = alloca $ \ptr -> do
    res <- c_clock_gettime clockMonotonic ptr
    when (res /= 0) $ throwErrno "clock_gettime(CLOCK_MONOTONIC)"
    TimeSpec (CTime s) (CLong n) <- peek ptr
    let s' = fromIntegral s :: Word64
    let n' = fromIntegral n :: Word64
    return (s' * 1_000_000_000 + n')

-- | Get Real Time in Nanoseconds (Word64)
getRealTimeNS :: IO Word64
getRealTimeNS = alloca $ \ptr -> do
    res <- c_clock_gettime clockRealtime ptr
    when (res /= 0) $ throwErrno "clock_gettime(CLOCK_REALTIME)"
    TimeSpec (CTime s) (CLong n) <- peek ptr
    let s' = fromIntegral s :: Word64
    let n' = fromIntegral n :: Word64
    return (s' * 1_000_000_000 + n')
