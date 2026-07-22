#!/usr/bin/env python3
import argparse
import sys
import os
from collections import defaultdict

def normalize_line(line):
    return line.strip()

def get_files_to_scan(directories, ignore_dirs):
    extensions = {'.hs', '.cpp', '.h', '.c', '.hpp'}
    files_to_scan = []
    
    # Normalize ignore_dirs to absolute paths for easy comparison
    abs_ignore_dirs = [os.path.abspath(d) for d in ignore_dirs]
    
    for d in directories:
        if os.path.isfile(d):
            if any(d.endswith(ext) for ext in extensions):
                files_to_scan.append(os.path.abspath(d))
        else:
            for root, _, files in os.walk(d):
                abs_root = os.path.abspath(root)
                
                # Check if current root is in ignore_dirs
                skip = False
                for ig_dir in abs_ignore_dirs:
                    if abs_root.startswith(ig_dir):
                        skip = True
                        break
                if skip:
                    continue
                    
                for file in files:
                    if any(file.endswith(ext) for ext in extensions):
                        files_to_scan.append(os.path.join(abs_root, file))
    return files_to_scan

def is_ignored(line, ignore_patterns):
    if not line: return True
    if "IGNORE CLONE" in line: return True
    for pat in ignore_patterns:
        if pat in line:
            return True
    return False

def main():
    parser = argparse.ArgumentParser(description="Custom Python Sliding-Window Clone Detector")
    parser.add_argument('--dirs', nargs='+', required=True, help="Directories or files to scan")
    parser.add_argument('--ignore-dirs', nargs='+', default=[], help="Directories to ignore")
    parser.add_argument('--threshold', type=int, default=5, help="Line limit threshold for duplicate blocks")
    parser.add_argument('--ignore-config', type=str, help="Path to ignore configuration file")
    
    args = parser.parse_args()
    
    ignore_patterns = []
    if args.ignore_config and os.path.exists(args.ignore_config):
        with open(args.ignore_config, 'r', encoding='utf-8') as f:
            for line in f:
                if line.strip() and not line.strip().startswith('#'):
                    ignore_patterns.append(line.strip())
                    
    files = get_files_to_scan(args.dirs, args.ignore_dirs)
    
    # Store lines globally: list of (filepath, line_number, normalized_line, original_line)
    lines_info = []
    
    for filepath in files:
        try:
            with open(filepath, 'r', encoding='utf-8', errors='ignore') as f:
                for i, raw_line in enumerate(f):
                    norm = normalize_line(raw_line)
                    if not is_ignored(norm, ignore_patterns):
                        # Convert to relative path for nicer output
                        try:
                            rel_path = os.path.relpath(filepath)
                        except ValueError:
                            rel_path = filepath
                        lines_info.append((rel_path, i + 1, norm, raw_line.rstrip('\n')))
        except Exception as e:
            print(f"Error reading {filepath}: {e}")
            
    # Find clones using a sliding window
    threshold = args.threshold
    if len(lines_info) < threshold:
        print("Not enough lines to find clones.")
        sys.exit(0)
        
    # Maps window tuple (normalized lines) to list of (filepath, start_line, end_line, original_lines_list)
    windows = defaultdict(list)
    
    for i in range(len(lines_info) - threshold + 1):
        # We need to make sure the window doesn't span across different files!
        file_set = set(lines_info[i+j][0] for j in range(threshold))
        if len(file_set) > 1:
            continue
            
        window_norm = tuple(lines_info[i+j][2] for j in range(threshold))
        
        filepath = lines_info[i][0]
        start_line = lines_info[i][1]
        end_line = lines_info[i+threshold-1][1]
        orig_lines = [lines_info[i+j][3] for j in range(threshold)]
        
        windows[window_norm].append((filepath, start_line, end_line, orig_lines))
        
    clones_found = False
    
    duplicates = {k: v for k, v in windows.items() if len(v) > 1}
    
    for window_norm, locations in duplicates.items():
        clones_found = True
        print("="*60)
        print(f"DUPLICATE CODE DETECTED ({threshold} lines)")
        print("="*60)
        for loc in locations:
            filepath, start_line, end_line, orig_lines = loc
            print(f"File: {filepath} | Lines: {start_line}-{end_line}")
        print("-" * 60)
        # Print the content from the first location
        for line in locations[0][3]:
            print(line)
        print("="*60)
        print()
            
    if clones_found:
        print(f"Error: Found duplicate code blocks meeting or exceeding the {threshold}-line threshold.")
        sys.exit(1)
    else:
        print("No duplicates found.")
        sys.exit(0)

if __name__ == '__main__':
    main()
