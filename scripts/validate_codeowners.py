#!/usr/bin/env python3
import sys
import os

def validate_codeowners(filepath):
    if not os.path.exists(filepath):
        print(f"Error: CODEOWNERS file not found at {filepath}")
        return False
    
    with open(filepath, 'r') as f:
        lines = f.readlines()
        
    valid = True
    users = set()
    
    for line_num, line in enumerate(lines, 1):
        # Remove inline comments and strip whitespace
        line = line.split('#')[0].strip()
        
        # Skip empty lines
        if not line:
            continue
            
        parts = line.split()
        if len(parts) < 2:
            print(f"Error (line {line_num}): Missing owners for path '{parts[0]}'")
            valid = False
            continue
            
        path = parts[0]
        owners = parts[1:]
        
        for owner in owners:
            if not owner.startswith('@') and '@' not in owner:
                # Typically valid formats are @username, @org/team, or email
                # Based on requirements: "including the mandatory user prefix"
                print(f"Error (line {line_num}): Owner '{owner}' is missing the mandatory '@' prefix.")
                valid = False
            users.add(owner)
            
    # Check for required roles: Lead Developer and QA/Safety Officer
    required_users = {"@fderuiter", "@isthebeamon"}
    missing = required_users - users
    
    if missing:
        print(f"Error: Missing required development plan roles in CODEOWNERS: {', '.join(missing)}")
        print("The Lead Developer (@fderuiter) and QA/Safety Officer (@isthebeamon) must be assigned.")
        valid = False
        
    return valid

if __name__ == "__main__":
    # Allow passing a specific path, default to standard location
    if len(sys.argv) > 1:
        codeowners_path = sys.argv[1]
    else:
        # Get absolute path from repo root
        repo_root = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
        codeowners_path = os.path.join(repo_root, ".github", "CODEOWNERS")
        
    if not validate_codeowners(codeowners_path):
        sys.exit(1)
    else:
        print("CODEOWNERS validation passed successfully.")
        sys.exit(0)
