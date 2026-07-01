{-# LANGUAGE OverloadedStrings #-}
-- |
-- Scaffolded Hardware Integration: {sensor_name}
-- 
-- Implements exception-safe resource allocation, asynchronous exception masking,
-- and FFI safety patterns.
module Hardware.{sensor_name} (
    with{sensor_name},
    attach{sensor_name},
    initialize{sensor_name},
    c_destroy_{lower_name}_fun_ptr
) where

import Control.Exception (bracket, mask_, uninterruptibleMask_)
import Foreign.Ptr (Ptr, FunPtr)
import Foreign.ForeignPtr (ForeignPtr, newForeignPtr)
import Foreign.Marshal.Alloc (mallocBytes, free, finalizerFree)
import Hardware.FFI.Bridge (MustHandle, bridgeHardwareCallCustom)
import Hardware.Types (HardwareError(..))
import Hardware.FFI.Common (HardwareResult(..))
import Foreign.C.Types (CInt)

-- | Dummy FFI stand-ins for scaffolding
c_create_{lower_name} :: IO (Ptr ())
c_create_{lower_name} = mallocBytes 1024

c_destroy_{lower_name} :: Ptr () -> IO ()
c_destroy_{lower_name} = free

c_attach_{lower_name} :: Ptr () -> IO (Ptr ())
c_attach_{lower_name} _ = mallocBytes 1024

c_destroy_{lower_name}_fun_ptr :: FunPtr (Ptr a -> IO ())
c_destroy_{lower_name}_fun_ptr = finalizerFree

-- | Lifecycle Stage 1: Creation (bracket pattern)
-- Exception-safe resource allocation [cite:source1, source4]
with{sensor_name} :: (Ptr () -> IO a) -> IO a
with{sensor_name} = bracket allocate freeResource
  where
    allocate = mask_ $ do
        -- Mask asynchronous exceptions during allocation
        c_create_{lower_name}
    freeResource ptr = uninterruptibleMask_ $ do
        -- Cleanup must not be interrupted
        c_destroy_{lower_name} ptr

-- | Lifecycle Stage 2: Attachment to existing memory [cite:source2]
attach{sensor_name} :: Ptr () -> IO (ForeignPtr ())
attach{sensor_name} existingPtr = do
    -- Uses ForeignPtr finalizer for GC-managed cleanup
    attached <- c_attach_{lower_name} existingPtr
    newForeignPtr c_destroy_{lower_name}_fun_ptr attached

-- | Example BridgeCall and MustHandle integration [cite:source6]
initialize{sensor_name} :: IO (MustHandle ())
initialize{sensor_name} = do
    let mockResult = return 0 :: IO CInt
    bridgeHardwareCallCustom (const $ return ()) mockResult
