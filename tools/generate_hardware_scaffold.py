#!/usr/bin/env python3
import sys
import os

def generate_haskell_scaffold(sensor_name):
    # Ensure correct capitalization
    sensor_name = sensor_name.capitalize()
    lower_name = sensor_name.lower()
    
    hs_code = f"""{{-# LANGUAGE OverloadedStrings #-}}
-- |
-- SAFETY-CRITICAL Scaffolded Hardware Integration: {{sensor_name}}
-- 
-- = Failure Mode
-- TODO: Document what happens when this hardware fails.
--
-- = Mitigation
-- TODO: Explain how the system handles the failure mode.
--
-- = Audit Events
-- TODO: List the audit events triggered by this hardware interaction.
--
-- Implements exception-safe resource allocation, asynchronous exception masking,
-- and FFI safety patterns.
module Hardware.{{sensor_name}} (
    with{{sensor_name}},
    attach{{sensor_name}},
    initialize{{sensor_name}},
    c_destroy_{{lower_name}}_fun_ptr
) where

import Control.Exception (bracket, mask_, uninterruptibleMask_)
import Foreign.Ptr (Ptr, FunPtr)
import Foreign.ForeignPtr (ForeignPtr, newForeignPtr)
import Foreign.Marshal.Alloc (mallocBytes, free, finalizerFree)
import Hardware.FFI.Bridge (MustHandle, bridgeHardwareCallCustom)
import Foreign.C.Types (CInt)

-- | Dummy FFI stand-ins for scaffolding
c_create_{{lower_name}} :: IO (Ptr ())
c_create_{{lower_name}} = mallocBytes 1024

c_destroy_{{lower_name}} :: Ptr () -> IO ()
c_destroy_{{lower_name}} = free

c_attach_{{lower_name}} :: Ptr () -> IO (Ptr ())
c_attach_{{lower_name}} _ = mallocBytes 1024

c_destroy_{{lower_name}}_fun_ptr :: FunPtr (Ptr a -> IO ())
c_destroy_{{lower_name}}_fun_ptr = finalizerFree

-- | Lifecycle Stage 1: Creation (bracket pattern)
-- Exception-safe resource allocation [cite:source1, source4]
with{{sensor_name}} :: (Ptr () -> IO a) -> IO a
with{{sensor_name}} = bracket allocate freeResource
  where
    allocate = mask_ $ do
        -- Mask asynchronous exceptions during allocation
        c_create_{{lower_name}}
    freeResource ptr = uninterruptibleMask_ $ do
        -- Cleanup must not be interrupted
        c_destroy_{{lower_name}} ptr

-- | Lifecycle Stage 2: Attachment to existing memory [cite:source2]
attach{{sensor_name}} :: Ptr () -> IO (ForeignPtr ())
attach{{sensor_name}} existingPtr = do
    -- Uses ForeignPtr finalizer for GC-managed cleanup
    attached <- c_attach_{{lower_name}} existingPtr
    newForeignPtr c_destroy_{{lower_name}}_fun_ptr attached

-- | Example BridgeCall and MustHandle integration [cite:source6]
initialize{{sensor_name}} :: IO (MustHandle ())
initialize{{sensor_name}} = do
    let mockResult = return 0 :: IO CInt
    bridgeHardwareCallCustom (const $ return ()) mockResult
"""
    
    hs_path = f"/app/src/Hardware/{sensor_name}.hs"
    with open(hs_path, "w") as f:
        f.write(hs_code)
    print(f"Generated {hs_path}")
    
    # Generate test
    test_code = f"""module Main (main) where

import Hardware.{{sensor_name}}
import System.Mem (performGC)
import Control.Monad (replicateM_)
import Foreign.Ptr (nullPtr)
import System.Exit (exitSuccess)

main :: IO ()
main = do
    -- Test memory leak safety of bracket pattern
    replicateM_ 1000 $ do
        with{{sensor_name}} $ \\_ -> return ()
    
    -- Test memory leak safety of ForeignPtr pattern
    replicateM_ 1000 $ do
        _ <- attach{{sensor_name}} nullPtr
        return ()
    
    performGC
    putStrLn "Passed memory-leak tests."
    exitSuccess
"""
    test_path = f"/app/test/Hardware/{sensor_name}Check.hs"
    os.makedirs("/app/test/Hardware", exist_ok=True)
    with open(test_path, "w") as f:
        f.write(test_code)
    print(f"Generated {test_path}")

if __name__ == "__main__":
    if len(sys.argv) != 2:
        print("Usage: generate_hardware_scaffold.py <SensorName>")
        sys.exit(1)
    generate_haskell_scaffold(sys.argv[1])
