#!/usr/bin/env python3
import sys
import hashlib
import json
import os
from datetime import datetime
import argparse

AUDIT_LOG = "docs/qms/audit_log.json"

def sign_document(filepath, user_id, status="approved"):
    if not os.path.exists(filepath):
        print(f"Error: Document {filepath} does not exist.")
        sys.exit(1)
        
    with open(filepath, 'rb') as f:
        content = f.read()
        
    doc_hash = hashlib.sha256(content).hexdigest()
    
    entry = {
        "timestamp": datetime.now().isoformat() + "Z",
        "document": filepath,
        "hash": doc_hash,
        "user": user_id,
        "status": status,
        "algorithm": "SHA-256"
    }
    
    logs = []
    if os.path.exists(AUDIT_LOG):
        try:
            with open(AUDIT_LOG, 'r') as f:
                logs = json.load(f)
        except json.JSONDecodeError:
            pass
            
    logs.append(entry)
    
    os.makedirs(os.path.dirname(AUDIT_LOG), exist_ok=True)
    with open(AUDIT_LOG, 'w') as f:
        json.dump(logs, f, indent=2)
        
    print(f"Document signed successfully: {doc_hash}")

def main():
    parser = argparse.ArgumentParser(description="Sign a controlled document for 21 CFR Part 11 compliance.")
    parser.add_argument("file", help="Path to the document to sign")
    parser.add_argument("--user", required=True, help="User ID or username")
    parser.add_argument("--status", default="approved", help="Status (e.g., approved, pending)")
    
    args = parser.parse_args()
    sign_document(args.file, args.user, args.status)

if __name__ == "__main__":
    main()
