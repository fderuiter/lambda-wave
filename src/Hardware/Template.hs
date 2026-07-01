{-# LANGUAGE OverloadedStrings #-}
-- |
-- Scaffolded Hardware Integration: Template
-- 
-- Implements exception-safe resource allocation, asynchronous exception masking,
-- and FFI safety patterns.
module Hardware.Template (
    withTemplate,
    attachTemplate,
    initializeTemplate,
    c_destroy_template_fun_ptr
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
c_create_template :: IO (Ptr ())
c_create_template = mallocBytes 1024

c_destroy_template :: Ptr () -> IO ()
c_destroy_template = free

c_attach_template :: Ptr () -> IO (Ptr ())
c_attach_template _ = mallocBytes 1024

c_destroy_template_fun_ptr :: FunPtr (Ptr a -> IO ())
c_destroy_template_fun_ptr = finalizerFree

-- | Lifecycle Stage 1: Creation (bracket pattern)
-- Exception-safe resource allocation [cite:source1, source4]
withTemplate :: (Ptr () -> IO a) -> IO a
withTemplate = bracket allocate freeResource
  where
    allocate = mask_ $ do
        -- Mask asynchronous exceptions during allocation
        c_create_template
    freeResource ptr = uninterruptibleMask_ $ do
        -- Cleanup must not be interrupted
        c_destroy_template ptr

-- | Lifecycle Stage 2: Attachment to existing memory [cite:source2]
attachTemplate :: Ptr () -> IO (ForeignPtr ())
attachTemplate existingPtr = do
    -- Uses ForeignPtr finalizer for GC-managed cleanup
    attached <- c_attach_template existingPtr
    newForeignPtr c_destroy_template_fun_ptr attached

-- | Example BridgeCall and MustHandle integration [cite:source6]
initializeTemplate :: IO (MustHandle ())
initializeTemplate = do
    let mockResult = return 0 :: IO CInt
    bridgeHardwareCallCustom (const $ return ()) mockResult
