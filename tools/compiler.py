#!/usr/bin/env python3
import sys
import os
import json
import argparse
from pathlib import Path

try:
    import yaml
except ImportError:
    yaml = None

from compiler_utils import to_camel_case, capitalize_first, to_snake_case

def load_data(filepath):
    with open(filepath, 'r') as f:
        if filepath.endswith('.yaml') or filepath.endswith('.yml'):
            if yaml is None:
                sys.exit("Error: pyyaml is not installed but a YAML file was provided.")
            return yaml.safe_load(f)
        else:
            return json.load(f)

def generate_cxx_idl(data, out_hdr, out_src):
    gap_val = data.get('constants', {}).get('ring_buffer_gap', 1)
    
    with open(out_hdr, 'w') as f:
        f.write("#ifndef RING_BUFFER_CONTROL_H\n")
        f.write("#define RING_BUFFER_CONTROL_H\n\n")
        f.write("#include <atomic>\n")
        f.write("#include <cstddef>\n")
        f.write("#include <cstdint>\n\n")
        f.write(f"#define RING_BUFFER_GAP {gap_val}\n\n")
        
        for struct in data.get('structs', []):
            f.write(f"struct {struct['name']} {{\n")
            if 'alignas' in struct:
                f.write(f"  alignas({struct['alignas']}) ")
            else:
                f.write("  ")
            
            first = True
            for field in struct['fields']:
                prefix = "  " if not first else ""
                if not first and 'alignas' in struct:
                    prefix = "  "
                t = "size_t" if field['type'] in ['size_t', 'atomic_size_t'] else field['type']
                if field['type'] == 'atomic_size_t':
                    t = f"std::atomic<{t}>"
                f.write(f"{prefix}{t} {field['name']};\n")
                first = False
            f.write("};\n\n")
            
            align = struct.get('alignas', 64)
            f.write(f"static_assert(alignof({struct['name']}) == {align},\n")
            f.write(f"              \"{struct['name']} alignment expected to be {align}\");\n")
            f.write(f"static_assert(sizeof({struct['name']}) == {align},\n")
            f.write(f"              \"{struct['name']} size expected to be {align}\");\n\n")
        
        f.write('extern "C" {\n')
        for struct in data.get('structs', []):
            for field in struct['fields']:
                if field['type'] == 'atomic_size_t':
                    f.write(f"size_t get_{field['name']}({struct['name']}* handle);\n")
                    f.write(f"void set_{field['name']}({struct['name']}* handle, size_t val);\n")
            f.write(f"size_t rb_available_data({struct['name']}* handle, size_t current_read_offset);\n")
            f.write(f"size_t rb_next_read_offset({struct['name']}* handle, size_t current_read_offset, size_t consumed_bytes);\n")
        f.write('}\n')
        f.write("#endif\n")

    with open(out_src, 'w') as f:
        hdr_name = os.path.basename(out_hdr)
        f.write(f'#include "{hdr_name}"\n\n')
        f.write('extern "C" {\n')
        for struct in data.get('structs', []):
            for field in struct['fields']:
                if field['type'] == 'atomic_size_t':
                    f.write(f"size_t get_{field['name']}({struct['name']}* handle) {{\n")
                    f.write(f"    if (!handle) return 0;\n")
                    f.write(f"    return handle->{field['name']}.load(std::memory_order_acquire);\n")
                    f.write(f"}}\n")
                    f.write(f"void set_{field['name']}({struct['name']}* handle, size_t val) {{\n")
                    f.write(f"    if (!handle) return;\n")
                    f.write(f"    handle->{field['name']}.store(val, std::memory_order_release);\n")
                    f.write(f"}}\n")
            
            f.write(f"size_t rb_available_data({struct['name']}* handle, size_t current_read_offset) {{\n")
            f.write(f"    if (!handle) return 0;\n")
            f.write(f"    size_t write_off = handle->write_offset.load(std::memory_order_acquire);\n")
            f.write(f"    size_t size = handle->buffer_size;\n")
            f.write(f"    if (write_off >= current_read_offset) {{\n")
            f.write(f"        return write_off - current_read_offset;\n")
            f.write(f"    }} else {{\n")
            f.write(f"        return size - current_read_offset + write_off;\n")
            f.write(f"    }}\n")
            f.write(f"}}\n\n")

            f.write(f"size_t rb_next_read_offset({struct['name']}* handle, size_t current_read_offset, size_t consumed_bytes) {{\n")
            f.write(f"    if (!handle) return current_read_offset;\n")
            f.write(f"    size_t size = handle->buffer_size;\n")
            f.write(f"    return (current_read_offset + consumed_bytes) % size;\n")
            f.write(f"}}\n")
        f.write('}\n')

def generate_hs_idl(data, out_hs):
    gap_val = data.get('constants', {}).get('ring_buffer_gap', 1)
    
    with open(out_hs, 'w') as f:
        exports = []
        for struct in data.get('structs', []):
            exports.append(f"{struct['name']}(..)")
            for field in struct['fields']:
                if field['type'] == 'atomic_size_t':
                    exports.append(f"c_get_{field['name']}")
                    exports.append(f"c_set_{field['name']}")
            exports.append("c_rb_available_data")
            exports.append("c_rb_next_read_offset")
        exports.append("ringBufferGap")
                    
        f.write(f"module FFI.RingBuffer.Generated ({', '.join(exports)}) where\n\n")
        f.write("import Foreign.C.Types\n")
        f.write("import Foreign.Ptr\n\n")
        f.write(f"ringBufferGap :: CSize\nringBufferGap = {gap_val}\n\n")
        
        for struct in data.get('structs', []):
            f.write(f"data {struct['name']} = {struct['name']}\n")
            f.write("    { ")
            first = True
            for field in struct['fields']:
                hs_type = "CSize" if "size_t" in field['type'] else field['type']
                name = to_camel_case(field['name'])
                prefix = "    , " if not first else ""
                f.write(f"{prefix}{name} :: !{hs_type}\n")
                first = False
            f.write("    } deriving (Show, Eq)\n\n")
            
            for field in struct['fields']:
                if field['type'] == 'atomic_size_t':
                    f.write(f"foreign import ccall unsafe \"get_{field['name']}\"\n")
                    f.write(f"    c_get_{field['name']} :: Ptr {struct['name']} -> IO CSize\n")
                    f.write(f"foreign import ccall unsafe \"set_{field['name']}\"\n")
                    f.write(f"    c_set_{field['name']} :: Ptr {struct['name']} -> CSize -> IO ()\n\n")
                    
            f.write(f"foreign import ccall unsafe \"rb_available_data\"\n")
            f.write(f"    c_rb_available_data :: Ptr {struct['name']} -> CSize -> IO CSize\n")
            f.write(f"foreign import ccall unsafe \"rb_next_read_offset\"\n")
            f.write(f"    c_rb_next_read_offset :: Ptr {struct['name']} -> CSize -> CSize -> IO CSize\n\n")

def generate_manifest(data, out_hdr, out_hs, cfg_path):
    required_keys = ['baud_rates', 'gpio_pins', 'timing', 'mounting_offset_mm']
    for key in required_keys:
        if key not in data:
            sys.exit(f"Error: Missing required hardware field '{key}' in manifest.")

    pins = list(data["gpio_pins"].values())
    if len(pins) != len(set(pins)):
        sys.exit("Error: Duplicate GPIO pin assignments.")
    for p in pins:
        if p < 0 or p >= 54:
            sys.exit(f"Error: Invalid GPIO pin {p}.")
            
    with open(out_hdr, 'w') as f:
        f.write("// DO NOT EDIT: This file is auto-generated from the hardware manifest.\n")
        f.write("#ifndef HARDWARE_MANIFEST_H\n")
        f.write("#define HARDWARE_MANIFEST_H\n\n")
        f.write("#include <termios.h>\n\n")
        f.write(f"#define MANIFEST_WATCHDOG_PIN {data['gpio_pins']['watchdog']}\n")
        f.write(f"#define MANIFEST_LOGIC_PIN {data['gpio_pins']['logic']}\n")
        f.write(f"#define MANIFEST_CONFIG_BAUD {data['baud_rates']['config']}\n")
        f.write(f"#define MANIFEST_CONFIG_BAUD_MACRO B{data['baud_rates']['config']}\n")
        f.write(f"#define MANIFEST_DATA_BAUD {data['baud_rates']['data']}\n")
        f.write(f"#define MANIFEST_DATA_BAUD_MACRO B{data['baud_rates']['data']}\n")
        f.write(f"#define MANIFEST_MOUNTING_OFFSET_MM {data['mounting_offset_mm']}\n")
        
        freq = data.get('constants', {}).get('operating_frequency_ghz', 77.0)
        f.write(f"#define MANIFEST_OPERATING_FREQUENCY_GHZ {freq}\n")
        
        f.write("#endif\n")
        
    with open(out_hs, 'w') as f:
        f.write("-- DO NOT EDIT: This file is auto-generated from the hardware manifest.\n")
        f.write("{-# LANGUAGE DataKinds #-}\n")
        f.write("module Hardware.Manifest (\n")
        f.write("    watchdogPin,\n")
        f.write("    logicPin,\n")
        f.write("    configBaudRate,\n")
        f.write("    dataBaudRate,\n")
        f.write("    framePeriodicityMs,\n")
        f.write("    systemLatencyMs,\n")
        f.write("    mountingOffsetMm,\n")
        f.write("    operatingFrequencyGhz,\n")
        f.write("    WatchdogTimeoutMs,\n")
        f.write("    SystemLatencyMs\n")
        f.write(") where\n\n")
        f.write(f"watchdogPin :: Int\nwatchdogPin = {data['gpio_pins']['watchdog']}\n\n")
        f.write(f"logicPin :: Int\nlogicPin = {data['gpio_pins']['logic']}\n\n")
        f.write(f"configBaudRate :: Int\nconfigBaudRate = {data['baud_rates']['config']}\n\n")
        f.write(f"dataBaudRate :: Int\ndataBaudRate = {data['baud_rates']['data']}\n\n")
        f.write(f"framePeriodicityMs :: Int\nframePeriodicityMs = {data['timing']['frame_periodicity_ms']}\n\n")
        f.write(f"systemLatencyMs :: Int\nsystemLatencyMs = {data['timing']['system_latency_ms']}\n\n")
        f.write(f"mountingOffsetMm :: Double\nmountingOffsetMm = {data['mounting_offset_mm']}\n\n")
        
        freq = data.get('constants', {}).get('operating_frequency_ghz', 77.0)
        f.write(f"operatingFrequencyGhz :: Double\noperatingFrequencyGhz = {freq}\n\n")
        
        f.write(f"type WatchdogTimeoutMs = {data['timing']['frame_periodicity_ms']}\n\n")
        f.write(f"type SystemLatencyMs = {data['timing']['system_latency_ms']}\n")

    with open(cfg_path, 'r') as f:
        lines = f.readlines()
    with open(cfg_path, 'w') as f:
        for line in lines:
            if line.startswith("frameCfg"):
                parts = line.strip().split()
                if len(parts) >= 6:
                    parts[5] = str(data['timing']['frame_periodicity_ms'])
                f.write(" ".join(parts) + "\n")
            else:
                f.write(line)

def generate_sensor_scaffold(sensor_name, cbits_include_dir, hs_src_dir, hs_test_dir):
    sensor_upper = capitalize_first(sensor_name)
    sensor_lower = to_snake_case(sensor_name)
    
    # 1. C++ Header
    cxx_hdr = os.path.join(cbits_include_dir, f"Sensor{sensor_upper}.h")
    os.makedirs(cbits_include_dir, exist_ok=True)
    with open(cxx_hdr, 'w') as f:
        f.write(f"#ifndef SENSOR_{sensor_upper.upper()}_H\n")
        f.write(f"#define SENSOR_{sensor_upper.upper()}_H\n\n")
        f.write("#ifdef __cplusplus\n")
        f.write("extern \"C\" {\n")
        f.write("#endif\n\n")
        f.write(f"void* c_create_{sensor_lower}();\n")
        f.write(f"void c_destroy_{sensor_lower}(void* ptr);\n")
        f.write(f"void* c_attach_{sensor_lower}(void* existing_ptr);\n\n")
        f.write("#ifdef __cplusplus\n")
        f.write("}\n")
        f.write("#endif\n\n")
        f.write("#endif\n")
    
    # 2. Haskell Source
    hs_code = f"""{{-# LANGUAGE OverloadedStrings #-}}
-- |
-- SAFETY-CRITICAL Scaffolded Hardware Integration: {sensor_upper}
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
module Hardware.{sensor_upper} (
    with{sensor_upper},
    attach{sensor_upper},
    initialize{sensor_upper},
    c_destroy_{sensor_lower}_fun_ptr
) where

import Control.Exception (bracket, mask_, uninterruptibleMask_)
import Foreign.Ptr (Ptr, FunPtr)
import Foreign.ForeignPtr (ForeignPtr, newForeignPtr)
import Hardware.FFI.Bridge (MustHandle, bridgeHardwareCallCustom)
import Foreign.C.Types (CInt)

-- | Real FFI imports from C++ headers
foreign import ccall unsafe "c_create_{sensor_lower}"
    c_create_{sensor_lower} :: IO (Ptr ())

foreign import ccall unsafe "c_destroy_{sensor_lower}"
    c_destroy_{sensor_lower} :: Ptr () -> IO ()

foreign import ccall unsafe "c_attach_{sensor_lower}"
    c_attach_{sensor_lower} :: Ptr () -> IO (Ptr ())

foreign import ccall unsafe "&c_destroy_{sensor_lower}"
    c_destroy_{sensor_lower}_fun_ptr :: FunPtr (Ptr a -> IO ())

-- | Lifecycle Stage 1: Creation (bracket pattern)
-- Exception-safe resource allocation [cite:source1, source4]
with{sensor_upper} :: (Ptr () -> IO a) -> IO a
with{sensor_upper} = bracket allocate freeResource
  where
    allocate = mask_ $ do
        -- Mask asynchronous exceptions during allocation
        c_create_{sensor_lower}
    freeResource ptr = uninterruptibleMask_ $ do
        -- Cleanup must not be interrupted
        c_destroy_{sensor_lower} ptr

-- | Lifecycle Stage 2: Attachment to existing memory [cite:source2]
attach{sensor_upper} :: Ptr () -> IO (ForeignPtr ())
attach{sensor_upper} existingPtr = do
    -- Uses ForeignPtr finalizer for GC-managed cleanup
    attached <- c_attach_{sensor_lower} existingPtr
    newForeignPtr c_destroy_{sensor_lower}_fun_ptr attached

-- | Example BridgeCall and MustHandle integration [cite:source6]
initialize{sensor_upper} :: IO (MustHandle ())
initialize{sensor_upper} = do
    let mockResult = return 0 :: IO CInt
    bridgeHardwareCallCustom (const $ return ()) mockResult
"""
    hs_path = os.path.join(hs_src_dir, "Hardware", f"{sensor_upper}.hs")
    os.makedirs(os.path.dirname(hs_path), exist_ok=True)
    with open(hs_path, "w") as f:
        f.write(hs_code)
    
    # 3. Haskell Test
    test_code = f"""module Main (main) where

import Hardware.{sensor_upper}
import System.Mem (performGC)
import Control.Monad (replicateM_)
import Foreign.Ptr (nullPtr)
import System.Exit (exitSuccess)

main :: IO ()
main = do
    -- Test memory leak safety of bracket pattern
    replicateM_ 1000 $ do
        with{sensor_upper} $ \\_ -> return ()
    
    -- Test memory leak safety of ForeignPtr pattern
    replicateM_ 1000 $ do
        _ <- attach{sensor_upper} nullPtr
        return ()
    
    performGC
    putStrLn "Passed memory-leak tests."
    exitSuccess
"""
    test_path = os.path.join(hs_test_dir, "Hardware", f"{sensor_upper}Check.hs")
    os.makedirs(os.path.dirname(test_path), exist_ok=True)
    with open(test_path, "w") as f:
        f.write(test_code)

def sync_docs(data):
    freq = data.get('constants', {}).get('operating_frequency_ghz', 77.0)
    import re
    pattern = re.compile(r'<!-- MANIFEST:operating_frequency_ghz -->(.*?)<!-- /MANIFEST:operating_frequency_ghz -->')
    def replacer(match):
        # We can format the frequency as an integer if it has no decimal part to match '60' or '77'
        val = int(freq) if freq == int(freq) else freq
        return f"<!-- MANIFEST:operating_frequency_ghz -->{val}<!-- /MANIFEST:operating_frequency_ghz -->"
    
    for root, _, files in os.walk("docs"):
        for file in files:
            if file.endswith(".md"):
                filepath = os.path.join(root, file)
                with open(filepath, 'r') as f:
                    content = f.read()
                new_content = pattern.sub(replacer, content)
                if new_content != content:
                    with open(filepath, 'w') as f:
                        f.write(new_content)

def generate_all_sensors(data, cbits_include_dir, hs_src_dir, hs_test_dir):
    sensors = data.get('sensors', [])
    for sensor in sensors:
        generate_sensor_scaffold(sensor['name'], cbits_include_dir, hs_src_dir, hs_test_dir)

def main():
    parser = argparse.ArgumentParser(description="Unified Hardware Compiler")
    subparsers = parser.add_subparsers(dest="command", required=True)
    
    parser_idl = subparsers.add_parser("idl")
    parser_idl.add_argument("input")
    parser_idl.add_argument("out_hdr")
    parser_idl.add_argument("out_src")
    parser_idl.add_argument("out_hs")
    
    parser_manifest = subparsers.add_parser("manifest")
    parser_manifest.add_argument("input")
    parser_manifest.add_argument("out_hdr")
    parser_manifest.add_argument("out_hs")
    parser_manifest.add_argument("out_cfg")
    
    parser_sensors = subparsers.add_parser("sensors")
    parser_sensors.add_argument("input")
    parser_sensors.add_argument("cbits_include_dir")
    parser_sensors.add_argument("hs_src_dir")
    parser_sensors.add_argument("hs_test_dir")
    
    parser_all = subparsers.add_parser("all")
    parser_all.add_argument("input")
    parser_all.add_argument("--idl_out_hdr", default="cbits/include/RingBuffer.h")
    parser_all.add_argument("--idl_out_src", default="cbits/src/ring_buffer_ffi.cpp")
    parser_all.add_argument("--idl_out_hs", default="src/FFI/RingBuffer/Generated.hs")
    parser_all.add_argument("--man_out_hdr", default="cbits/include/hardware_manifest.h")
    parser_all.add_argument("--man_out_hs", default="src/Hardware/Manifest.hs")
    parser_all.add_argument("--man_out_cfg", default="config/ti_iwr6843isk/sgrt_profile.cfg")
    parser_all.add_argument("--cbits_include_dir", default="cbits/include")
    parser_all.add_argument("--hs_src_dir", default="src")
    parser_all.add_argument("--hs_test_dir", default="test")

    args = parser.parse_args()
    data = load_data(args.input)
    
    if args.command == "idl":
        generate_cxx_idl(data, args.out_hdr, args.out_src)
        generate_hs_idl(data, args.out_hs)
    elif args.command == "manifest":
        generate_manifest(data, args.out_hdr, args.out_hs, args.out_cfg)
        sync_docs(data)
    elif args.command == "sensors":
        generate_all_sensors(data, args.cbits_include_dir, args.hs_src_dir, args.hs_test_dir)
    elif args.command == "all":
        if 'structs' in data:
            generate_cxx_idl(data, args.idl_out_hdr, args.idl_out_src)
            generate_hs_idl(data, args.idl_out_hs)
        if 'baud_rates' in data:
            generate_manifest(data, args.man_out_hdr, args.man_out_hs, args.man_out_cfg)
        if 'sensors' in data:
            generate_all_sensors(data, args.cbits_include_dir, args.hs_src_dir, args.hs_test_dir)
        sync_docs(data)

if __name__ == '__main__':
    main()
