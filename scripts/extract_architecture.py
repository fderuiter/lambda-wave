#!/usr/bin/env python3
import os
import sys
import re
import argparse

def extract_docstrings(directory):
    docstrings = []
    for root, _, files in os.walk(directory):
        for file in files:
            if not file.endswith('.hs'):
                continue
            path = os.path.join(root, file)
            with open(path, 'r') as f:
                content = f.read()
            
            # Simple parser for Haddock block comments starting with '-- |'
            lines = content.splitlines()
            in_doc = False
            current_doc = []
            
            for line in lines:
                if line.lstrip().startswith('-- |'):
                    if in_doc:
                        docstrings.append((os.path.relpath(path, '/app'), '\n'.join(current_doc)))
                    in_doc = True
                    # Remove '-- |'
                    text = line.lstrip()[4:]
                    if text.startswith(' '):
                        text = text[1:]
                    current_doc = [text]
                elif in_doc and line.lstrip().startswith('--'):
                    # Remove '--'
                    text = line.lstrip()[2:]
                    if text.startswith(' '):
                        text = text[1:]
                    current_doc.append(text)
                else:
                    if in_doc:
                        docstrings.append((os.path.relpath(path, '/app'), '\n'.join(current_doc)))
                        in_doc = False
                        current_doc = []
            
            if in_doc:
                docstrings.append((os.path.relpath(path, '/app'), '\n'.join(current_doc)))
                
    return docstrings

def validate_and_filter_docstrings(docstrings):
    valid_docs = []
    for path, doc in docstrings:
        has_failure_modes = re.search(r'Failure Modes:', doc, re.IGNORECASE)
        has_mitigations = re.search(r'Mitigations:', doc, re.IGNORECASE)
        has_traceability = re.search(r'Traceability:', doc, re.IGNORECASE)
        
        # Determine if it's an architectural docstring
        if has_failure_modes or has_mitigations or has_traceability:
            if not (has_failure_modes and has_mitigations and has_traceability):
                print(f"Error: Validation failed in {path}. Architectural docstrings must include 'Failure Modes:', 'Mitigations:', and 'Traceability:' sections.", file=sys.stderr)
                sys.exit(1)
            valid_docs.append((path, doc))
            
    return valid_docs

def generate_markdown(valid_docs):
    md = []
    for path, doc in valid_docs:
        md.append(f"### Extracted from `{path}`")
        md.append("")
        md.append(doc)
        md.append("")
    return "\n".join(md)

def check_for_missing_docstring_updates():
    import subprocess
    try:
        base_ref = os.environ.get('GITHUB_BASE_REF', '')
        if base_ref:
            cmd = ['git', 'diff', f'origin/{base_ref}...HEAD', '--name-only']
        else:
            # Fallback to checking the last commit
            cmd = ['git', 'show', '--name-only', '--format=']
        
        modified_files = subprocess.check_output(cmd, text=True).splitlines()
        
        safety_dirs = ('src/Numeric', 'src/SignalProcessing', 'src/FFI')
        safety_modified = [f for f in modified_files if f.startswith(safety_dirs) and f.endswith('.hs')]
        
        for sf in safety_modified:
            if base_ref:
                diff_cmd = ['git', 'diff', f'origin/{base_ref}...HEAD', '--', sf]
            else:
                diff_cmd = ['git', 'show', '--', sf]
            
            diff_out = subprocess.check_output(diff_cmd, text=True)
            doc_changed = False
            for line in diff_out.splitlines():
                if line.startswith('+') and not line.startswith('+++'):
                    content = line[1:].lstrip()
                    if content.startswith('-- |') or content.startswith('--'):
                        doc_changed = True
                        break
                elif line.startswith('-') and not line.startswith('---'):
                    content = line[1:].lstrip()
                    if content.startswith('-- |') or content.startswith('--'):
                        doc_changed = True
                        break
            
            if not doc_changed:
                print(f"Error: Safety-critical file {sf} was modified but its architectural docstrings were not updated.", file=sys.stderr)
                sys.exit(1)
                
    except subprocess.CalledProcessError:
        pass # Ignore git errors if not in a git repo or ref not found

def update_document(doc_path, new_content, check_only=False):
    with open(doc_path, 'r') as f:
        content = f.read()
        
    start_marker = "<!-- ARCHITECTURE-START -->"
    end_marker = "<!-- ARCHITECTURE-END -->"
    
    if start_marker not in content or end_marker not in content:
        if check_only:
            print("Error: Markers not found in document. Run without --check to apply.", file=sys.stderr)
            sys.exit(1)
        else:
            content += f"\n\n## Auto-Generated Architecture\n{start_marker}\n{new_content}\n{end_marker}\n"
            with open(doc_path, 'w') as f:
                f.write(content)
            print("Appended markers and content.")
            return

    start_idx = content.find(start_marker) + len(start_marker)
    end_idx = content.find(end_marker)
    
    current_extracted = content[start_idx:end_idx].strip()
    new_content_stripped = new_content.strip()
    
    if current_extracted == new_content_stripped:
        print("Document is in sync.")
        sys.exit(0)
    
    if check_only:
        print("Error: Document is out of sync. Architectural documentation drift detected.", file=sys.stderr)
        sys.exit(1)
        
    new_doc_content = content[:start_idx] + "\n" + new_content_stripped + "\n" + content[end_idx:]
    with open(doc_path, 'w') as f:
        f.write(new_doc_content)
    print("Document updated successfully.")

def main():
    parser = argparse.ArgumentParser(description="Extract architectural docstrings.")
    parser.add_argument('--check', action='store_true', help="Check for drift without modifying.")
    args = parser.parse_args()
    
    if args.check:
        check_for_missing_docstring_updates()
    
    dirs_to_scan = ['src/Numeric', 'src/SignalProcessing', 'src/FFI']
    all_docstrings = []
    for d in dirs_to_scan:
        path = os.path.join('/app', d)
        if os.path.exists(path):
            all_docstrings.extend(extract_docstrings(path))
            
    valid_docs = validate_and_filter_docstrings(all_docstrings)
    
    if not valid_docs:
        print("Warning: No valid architectural docstrings found.")
        
    new_content = generate_markdown(valid_docs)
    
    doc_path = '/app/Haskell Radar SGRT System Development.md'
    update_document(doc_path, new_content, check_only=args.check)

if __name__ == "__main__":
    main()
