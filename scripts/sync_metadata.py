#!/usr/bin/env python3
import json
import os
import re
import sys

def load_registry(path="project_metadata.json"):
    with open(path, 'r') as f:
        return json.load(f)

def process_file(filepath, registry, check_only=False):
    with open(filepath, 'r') as f:
        content = f.read()

    def replacer(match):
        key = match.group(1)
        if key in registry:
            return f"<!-- METADATA:{key} -->{registry[key]}<!-- /METADATA:{key} -->"
        return match.group(0)

    pattern = re.compile(r'<!-- METADATA:([a-zA-Z0-9_]+) -->(.*?)<!-- /METADATA:\1 -->', re.DOTALL)
    new_content = pattern.sub(replacer, content)

    if new_content != content:
        if check_only:
            print(f"ERROR: File {filepath} has drifted from registry.")
            return False
        else:
            with open(filepath, 'w') as f:
                f.write(new_content)
            print(f"Updated {filepath}")
            return True
    return True

def main():
    check_only = "--check" in sys.argv
    try:
        registry = load_registry()
    except FileNotFoundError:
        print("ERROR: project_metadata.json not found.")
        sys.exit(1)
    
    success = True

    for root, dirs, files in os.walk("."):
        if ".git" in root or ".github" in root:
            continue
        for file in files:
            if file.endswith(".md"):
                filepath = os.path.join(root, file)
                if not process_file(filepath, registry, check_only):
                    success = False
    
    if not success:
        sys.exit(1)
    else:
        if check_only:
            print("All documentation is in sync with the registry.")

if __name__ == "__main__":
    main()
