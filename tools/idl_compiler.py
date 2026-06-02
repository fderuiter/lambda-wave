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
        f.write('}\n')

def generate_haskell(data, out_hs):
    with open(out_hs, 'w') as f:
        exports = []
        for struct in data['structs']:
            exports.append(f"{struct['name']}(..)")
            for field in struct['fields']:
                if field['type'] == 'atomic_size_t':
                    exports.append(f"c_get_{field['name']}")
                    exports.append(f"c_set_{field['name']}")
                    
        f.write(f"module FFI.RingBuffer.Generated ({', '.join(exports)}) where\n\n")
        f.write("import Foreign.C.Types\n")
        f.write("import Foreign.Ptr\n\n")
        
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

if __name__ == '__main__':
    with open(sys.argv[1]) as f:
        data = json.load(f)
    generate_cxx(data, sys.argv[2], sys.argv[3])
    generate_haskell(data, sys.argv[4])
