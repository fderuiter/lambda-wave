#!/usr/bin/env python3
import sys
import os
import glob

def main():
    if len(sys.argv) > 1:
        version_tag = sys.argv[1]
    else:
        version_tag = "LATEST"
        
    ccr_files = glob.glob('docs/ccr/*.md')
    matched_ccrs = []
    
    for fpath in ccr_files:
        if 'template' in os.path.basename(fpath).lower():
            continue
            
        with open(fpath, 'r') as f:
            content = f.read()
            
        if version_tag == "LATEST" or version_tag in fpath or version_tag in content:
            matched_ccrs.append((fpath, content))
            
    # Always generate the report even if empty, as CI expects it
    report = f"# Safety Audit Report - {version_tag}\n\n"
    
    if not matched_ccrs:
        report += "No Change Control Records found for this release.\n"
    else:
        for fpath, content in matched_ccrs:
            report += f"## Record: {os.path.basename(fpath)}\n\n"
            report += content + "\n\n---\n\n"
            
    with open('safety_audit_report.txt', 'w') as f:
        f.write(report)
        
    print(f"Generated safety_audit_report.txt with {len(matched_ccrs)} records.")

if __name__ == '__main__':
    main()
