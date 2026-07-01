#!/usr/bin/env python3
import os
import re
import sys

def check_file(filepath):
    with open(filepath, 'r') as f:
        content = f.read()

    # Split by double newline to get paragraphs
    paragraphs = content.split('\n\n')
    
    issues = []
    
    for i, p in enumerate(paragraphs):
        p = p.strip()
        if not p:
            continue
            
        # Ignore headers, lists, math, code blocks, tables
        if p.startswith('#') or p.startswith('*') or p.startswith('-') or p.startswith('$$') or p.startswith('```') or p.startswith('|'):
            continue
            
        # Ignore very short lines (e.g., single formulas or small notes)
        if len(p) < 150 and p.count('.') <= 2:
            continue

        # If it's a regular paragraph and it's long, flag it
        sentences = len(re.split(r'(?<!\w\.\w.)(?<![A-Z][a-z]\.)(?<=\.|\?)\s', p))
        if len(p) >= 200 or sentences >= 3:
            issues.append(f"Paragraph {i+1} appears to be narrative text (length: {len(p)} chars, {sentences} sentences):\n{p[:50]}...")
            
    return issues

def main():
    ref_dir = os.path.join(os.path.dirname(__file__), '..', 'docs', 'reference')
    if not os.path.exists(ref_dir):
        print(f"Directory {ref_dir} does not exist.")
        sys.exit(1)
        
    has_error = False
    
    for root, _, files in os.walk(ref_dir):
        for file in files:
            if file.endswith('.md'):
                filepath = os.path.join(root, file)
                issues = check_file(filepath)
                if issues:
                    has_error = True
                    print(f"Error: Non-reference narrative found in {filepath}:")
                    for issue in issues:
                        print(f"  - {issue}")
                        
    if has_error:
        sys.exit(1)
    else:
        print("Validation passed: No narrative content found in reference docs.")
        sys.exit(0)

if __name__ == '__main__':
    main()
