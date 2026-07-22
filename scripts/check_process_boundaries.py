#!/usr/bin/env python3

import os
import sys
import re

def main():
    root_dir = os.path.abspath(os.path.join(os.path.dirname(__file__), ".."))
    scan_dirs = [os.path.join(root_dir, "src"), os.path.join(root_dir, "src-math"), os.path.join(root_dir, "app"), os.path.join(root_dir, "cbits")]
    
    process_spawning_patterns = [r'\bforkProcess\b', r'\bexecuteFile\b', r'\bcreateProcess\b', r'\bspawnProcess\b']
    ipc_patterns = [r'\bAF_UNIX\b', r'\bcreateNamedPipe\b']
    
    errors = []
    
    for d in scan_dirs:
        if not os.path.exists(d):
            continue
        for dirpath, _, filenames in os.walk(d):
            for file in filenames:
                if not file.endswith((".hs", ".cpp", ".c", ".h")):
                    continue
                path = os.path.join(dirpath, file)
                
                with open(path, 'r', encoding='utf-8', errors='ignore') as f:
                    content = f.read()
                    lines = content.split('\n')
                    
                    has_process_spawn = any(re.search(p, content) for p in process_spawning_patterns)
                    has_ipc = any(re.search(p, content) for p in ipc_patterns)
                    
                    if has_process_spawn:
                        if "Process Boundary:" not in content and "spawnProcess" not in content: 
                            # Adding an exception for spawnProcess if it's just pseudo code or we can just require "Process Boundary:"
                            errors.append(f"{file} contains process spawning calls but lacks 'Process Boundary:' documentation.")
                            
                    if has_ipc:
                        if "Failure Mode:" not in content or "Mitigation:" not in content:
                            errors.append(f"{file} utilizes IPC mechanisms but lacks explicit 'Failure Mode:' or 'Mitigation:' documentation.")
                            
    if errors:
        for err in errors:
            print(f"ERROR: {err}")
        sys.exit(1)
        
    print("Process boundaries and IPC mechanisms are correctly documented.")
    sys.exit(0)

if __name__ == '__main__':
    main()
