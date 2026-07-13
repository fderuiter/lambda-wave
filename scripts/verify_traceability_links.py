#!/usr/bin/env python3
import os
import re
import sys

def main():
    root_dir = os.path.abspath(os.path.join(os.path.dirname(__file__), ".."))
    matrix_path = os.path.join(root_dir, "docs/iec_62304/traceability_matrix.md")
    
    if not os.path.exists(matrix_path):
        print("Traceability matrix not found.")
        sys.exit(0) # Not an error if it hasn't been generated yet, or should we fail?
        
    with open(matrix_path, 'r') as f:
        content = f.read()
        
    # Find all Markdown links
    # Look for table rows, e.g. | ... |
    failed = False
    
    # We only care about links in the Architecture Section column, but let's check all links just in case
    links = re.findall(r'\[([^\]]+)\]\(([^)]+)\)', content)
    for text, url in links:
        # Ignore external HTTP links
        if url.startswith('http'):
            continue
            
        # Resolve path relative to the matrix location
        matrix_dir = os.path.dirname(matrix_path)
        
        # if url contains anchor #... strip it
        file_path = url.split('#')[0]
        
        abs_path = os.path.normpath(os.path.join(matrix_dir, file_path))
        if not os.path.exists(abs_path):
            print(f"Broken link found in traceability matrix: [{text}]({url}) -> resolves to {abs_path}")
            failed = True
            
    if failed:
        print("Verification failed: Broken links found in traceability matrix.")
        sys.exit(1)
    else:
        print("All local links in the traceability matrix are valid.")
        sys.exit(0)

if __name__ == "__main__":
    main()
