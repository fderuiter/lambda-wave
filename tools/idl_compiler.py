#!/usr/bin/env python3
import sys
import json
import os

def generate_cxx(data, out_hdr, out_src):
    with open(out_hdr, 'w') as f:
        f.write("#ifndef RING_BUFFER_CONTROL_H\n")
        f.write("#define RING_BUFFER_CONTROL_H\n\n")
        f.write("#include <atomic>\n")
        f.write("#include <cstddef>\n")
        f.write("#include <cstdint>\n\n")
        
        if 'constants' in data:
            for k, v in data['constants'].items():
                f.write(f"#define {k} {v}\n")
            f.write("\n")

        for struct in data['structs']:
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
            
            f.write(f"static_assert(alignof({struct['name']}) == {struct.get('alignas', 64)},\n")
            f.write(f"              \"{struct['name']} alignment expected to be {struct.get('alignas', 64)}\");\n")
            f.write(f"static_assert(sizeof({struct['name']}) == {struct.get('alignas', 64)},\n")
            f.write(f"              \"{struct['name']} size expected to be {struct.get('alignas', 64)}\");\n\n")
        
        f.write('extern "C" {\n')
        for struct in data['structs']:
            for field in struct['fields']:
                if field['type'] == 'atomic_size_t':
                    f.write(f"size_t get_{field['name']}({struct['name']}* handle);\n")
                    f.write(f"void set_{field['name']}({struct['name']}* handle, size_t val);\n")
        
        f.write("size_t calculate_available_read_bytes(size_t read_offset, size_t write_offset, size_t buffer_size);\n")
        f.write("size_t calculate_next_read_offset(size_t read_offset, size_t consumed, size_t buffer_size);\n")
        
        f.write('}\n')
        f.write("#endif\n")

    with open(out_src, 'w') as f:
        f.write(f'#include "{os.path.basename(out_hdr)}"\n\n')
        f.write('extern "C" {\n')
        for struct in data['structs']:
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
        
        f.write("size_t calculate_available_read_bytes(size_t read_offset, size_t write_offset, size_t buffer_size) {\n")
        f.write("    if (write_offset >= read_offset) {\n")
        f.write("        return write_offset - read_offset;\n")
        f.write("    } else {\n")
        f.write("        return buffer_size - read_offset + write_offset;\n")
        f.write("    }\n")
        f.write("}\n\n")

        f.write("size_t calculate_next_read_offset(size_t read_offset, size_t consumed, size_t buffer_size) {\n")
        f.write("    return (read_offset + consumed) % buffer_size;\n")
        f.write("}\n")
        
        f.write('}\n')

def generate_haskell(data, out_hs):
    with open(out_hs, 'w') as f:
        exports = []
        if 'constants' in data:
            for k in data['constants'].keys():
                # Convert BUFFER_GAP to bufferGap
                words = k.lower().split('_')
                hs_name = words[0] + ''.join(w.capitalize() for w in words[1:])
                exports.append(hs_name)
        
        for struct in data['structs']:
            exports.append(f"{struct['name']}(..)")
            for field in struct['fields']:
                if field['type'] == 'atomic_size_t':
                    exports.append(f"c_get_{field['name']}")
                    exports.append(f"c_set_{field['name']}")
                    
        exports.append("c_calculate_available_read_bytes")
        exports.append("c_calculate_next_read_offset")
        
        f.write(f"module FFI.RingBuffer.Generated ({', '.join(exports)}) where\n\n")
        f.write("import Foreign.C.Types\n")
        f.write("import Foreign.Ptr\n\n")
        
        if 'constants' in data:
            for k, v in data['constants'].items():
                words = k.lower().split('_')
                hs_name = words[0] + ''.join(w.capitalize() for w in words[1:])
                f.write(f"{hs_name} :: Int\n")
                f.write(f"{hs_name} = {v}\n\n")
        
        for struct in data['structs']:
            f.write(f"data {struct['name']} = {struct['name']}\n")
            f.write("    { ")
            first = True
            for field in struct['fields']:
                hs_type = "CSize" if "size_t" in field['type'] else field['type']
                name = "".join([w.capitalize() if i > 0 else w for i, w in enumerate(field['name'].split('_'))])
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

        f.write("foreign import ccall unsafe \"calculate_available_read_bytes\"\n")
        f.write("    c_calculate_available_read_bytes :: CSize -> CSize -> CSize -> IO CSize\n\n")
        
        f.write("foreign import ccall unsafe \"calculate_next_read_offset\"\n")
        f.write("    c_calculate_next_read_offset :: CSize -> CSize -> CSize -> IO CSize\n\n")

if __name__ == '__main__':
    with open(sys.argv[1]) as f:
        data = json.load(f)
    if 'structs' in data:
        generate_cxx(data, sys.argv[2], sys.argv[3])
        generate_haskell(data, sys.argv[4])
    elif 'baud_rates' in data:
        # Validation for required fields
        required_keys = ['baud_rates', 'gpio_pins', 'timing', 'mounting_offset_mm']
        for key in required_keys:
            if key not in data:
                sys.exit(f"Error: Missing required hardware field '{key}' in manifest.")

        # Hardware manifest path
        out_hdr = sys.argv[2]
        out_hs = sys.argv[3]
        cfg_path = sys.argv[4]
        
        # Validation
        pins = list(data["gpio_pins"].values())
        if len(pins) != len(set(pins)):
            sys.exit("Error: Duplicate GPIO pin assignments.")
        for p in pins:
            if p < 0 or p >= 54:
                sys.exit(f"Error: Invalid GPIO pin {p}.")
                
        # Generate C++ header
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
            f.write("#endif\n")
            
        # Generate Haskell module
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
            f.write("    WatchdogTimeoutMs,\n")
            f.write("    SystemLatencyMs\n")
            f.write(") where\n\n")
            f.write("import GHC.TypeLits (Nat)\n\n")
            f.write(f"watchdogPin :: Int\nwatchdogPin = {data['gpio_pins']['watchdog']}\n\n")
            f.write(f"logicPin :: Int\nlogicPin = {data['gpio_pins']['logic']}\n\n")
            f.write(f"configBaudRate :: Int\nconfigBaudRate = {data['baud_rates']['config']}\n\n")
            f.write(f"dataBaudRate :: Int\ndataBaudRate = {data['baud_rates']['data']}\n\n")
            f.write(f"framePeriodicityMs :: Int\nframePeriodicityMs = {data['timing']['frame_periodicity_ms']}\n\n")
            f.write(f"systemLatencyMs :: Int\nsystemLatencyMs = {data['timing']['system_latency_ms']}\n\n")
            f.write(f"mountingOffsetMm :: Double\nmountingOffsetMm = {data['mounting_offset_mm']}\n\n")
            f.write(f"type WatchdogTimeoutMs = {data['timing']['frame_periodicity_ms']}\n\n")
            f.write(f"type SystemLatencyMs = {data['timing']['system_latency_ms']}\n")

        # Update radar cfg file
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

