{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE CApiFFI #-}

{-|
Module      : Data.Time.HighRes
Description : High-Resolution Monotonic Time via FFI
Copyright   : (c) 2024
License     : BSD-3-Clause

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
    alignment _ = max (alignment (undefined :: CTime)) (alignment (undefined :: CLong))
    sizeOf _ =
        let sSec = sizeOf (undefined :: CTime)
            aLong = alignment (undefined :: CLong)
            sLong = sizeOf (undefined :: CLong)
            offsetNSec = (sSec + aLong - 1) `div` aLong * aLong
            structSize = offsetNSec + sLong
            structAlign = alignment (undefined :: TimeSpec)
        in (structSize + structAlign - 1) `div` structAlign * structAlign

    peek ptr = do
        let sSec = sizeOf (undefined :: CTime)
            aLong = alignment (undefined :: CLong)
            offsetNSec = (sSec + aLong - 1) `div` aLong * aLong
        s <- peekByteOff ptr 0
        n <- peekByteOff ptr offsetNSec
        return (TimeSpec s n)

    poke ptr (TimeSpec s n) = do
        let sSec = sizeOf (undefined :: CTime)
            aLong = alignment (undefined :: CLong)
            offsetNSec = (sSec + aLong - 1) `div` aLong * aLong
        pokeByteOff ptr 0 s
        pokeByteOff ptr offsetNSec n

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
