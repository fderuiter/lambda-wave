#!/usr/bin/env python3
import sys
import os
import glob
import re

def main():
    if len(sys.argv) < 2:
        print("Usage: scripts/ccr_validation.py <modified_files.txt>")
        sys.exit(1)
        
    with open(sys.argv[1], 'r') as f:
        modified_files = [line.strip() for line in f if line.strip()]
        
    # Requirement 3: Identify "Class C" changes based on file paths
    # We consider anything in src/Safety/ or src-math/Safety/ as a Class C safety-critical file
    safety_modified = any((f.startswith('src/Safety/') or f.startswith('src-math/Safety/')) for f in modified_files)
    
    # Check for manual overrides of automated Traceability Matrix
    if 'docs/iec_62304/traceability_matrix.md' in modified_files:
        print("AUDIT EXCEPTION: Manual override of the automated Traceability Matrix detected.")
    
    # Check if a CCR file was added or updated
    ccr_files_modified = [f for f in modified_files if f.startswith('docs/ccr/') and f.endswith('.md') and os.path.basename(f) != 'template.md']
    rmf_modified = any(f == 'rmf.yaml' for f in modified_files)

    # Automated check within the CI pipeline that fails if a safety-critical change is detected without an updated risk analysis
    if safety_modified and not rmf_modified:
        print("ERROR: Class C changes detected in 'src/Safety/' or 'src-math/Safety/' but no updated risk analysis (rmf.yaml) found.")
        print("Please update rmf.yaml with new or updated hazard analysis for this safety-critical change.")
        sys.exit(1)

    # Requirement 4: Fail if Class C change is detected without a corresponding CCR
    if safety_modified and not ccr_files_modified:
        print("ERROR: Class C changes detected in 'src/Safety/' or 'src-math/Safety/' but no CCR file was added or updated in 'docs/ccr/'.")
        print("Please create a Change Control Record (CCR) using docs/ccr/template.md.")
        sys.exit(1)
        
    if not safety_modified and not ccr_files_modified:
        print("No Class C (Safety) files modified and no CCRs modified. Skipping CCR validation.")
        sys.exit(0)
        
    valid_hazards = get_valid_hazards()
    
    all_valid = True
    # If CCRs were modified, validate them even if no safety code was touched in this exact commit
    for ccr in ccr_files_modified:
        if not os.path.exists(ccr):
            continue
        if not validate_ccr(ccr, valid_hazards):
            all_valid = False
            
    if not all_valid:
        print("ERROR: CCR validation failed.")
        sys.exit(1)
        
    print("CCR validation passed successfully.")

def get_valid_hazards():
    hazards = set()
    
    # Parse rmf.yaml for Hazard IDs
    rmf_path = 'rmf.yaml'
    if os.path.exists(rmf_path):
        import yaml
        with open(rmf_path, 'r') as f:
            data = yaml.safe_load(f)
            if data and 'hazards' in data:
                for h in data['hazards']:
                    if 'id' in h:
                        hazards.add(h['id'])
                        
    # The prompt explicitly mentions "Signal Noise" and "Watchdog Timeouts"
    hazards.add("Signal Noise")
    hazards.add("Watchdog Timeouts")
    hazards.add("Signal noise")
    hazards.add("Watchdog timer")
    
    return hazards

def validate_ccr(ccr_path, valid_hazards):
    print(f"Validating {ccr_path}...")
    with open(ccr_path, 'r') as f:
        content = f.read()
        
    required_sections = ['Problem Description', 'Proposed Change', 'Impact on Hazards', 'Verification Strategy', 'Quality Policy Origin']
    for sec in required_sections:
        if sec not in content:
            print(f"  ERROR: Missing required section '{sec}'")
            return False
            
    # Extract "Impact on Hazards" section
    parts = content.split('Impact on Hazards')
    if len(parts) < 2:
        print("  ERROR: Could not parse 'Impact on Hazards' section")
        return False
        
    impact_section = parts[1].split('##')[0]
    
    found_valid = False
    
    # Requirement 5: Check for valid Hazard ID references
    # Check for formal IDs (H-*)
    formal_ids = re.findall(r'H-[A-Z0-9\-]+', impact_section)
    for fid in formal_ids:
        if fid not in valid_hazards:
            print(f"  ERROR: Non-existent Hazard ID referenced: '{fid}'")
            return False
        else:
            found_valid = True
            
    # Check for informal category mentions if no formal IDs used
    if not found_valid:
        for h in valid_hazards:
            if h.lower() in impact_section.lower():
                found_valid = True
                break
                
    if not found_valid:
        print(f"  ERROR: No valid Hazard IDs or categories found in the Impact on Hazards section.")
        return False
        
    print(f"  PASS: {ccr_path} is valid.")
    return True

if __name__ == '__main__':
    main()
