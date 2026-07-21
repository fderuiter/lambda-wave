{-# LANGUAGE CPP #-}
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

#include <time.h>

-- | Corresponds to C 'struct timespec'
data TimeSpec = TimeSpec
    { _sec  :: {-# UNPACK #-} !CTime
    , _nsec :: {-# UNPACK #-} !CLong
    }

instance Storable TimeSpec where
    alignment _ = #{alignment struct timespec}
    sizeOf _ = #{size struct timespec}

    peek ptr = TimeSpec
        <$> #{peek struct timespec, tv_sec} ptr
        <*> #{peek struct timespec, tv_nsec} ptr

    poke ptr (TimeSpec s n) = do
        #{poke struct timespec, tv_sec} ptr s
        #{poke struct timespec, tv_nsec} ptr n

foreign import ccall unsafe "time.h clock_gettime"
    c_clock_gettime :: Int32 -> Ptr TimeSpec -> IO CInt

clockMonotonic :: Int32
clockMonotonic = #{const CLOCK_MONOTONIC}

clockRealtime :: Int32
clockRealtime = #{const CLOCK_REALTIME}

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
