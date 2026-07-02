#!/usr/bin/env python3

import os
import sys
import yaml
import re
import argparse

def main():
    parser = argparse.ArgumentParser(description="Compliance Engine")
    parser.add_argument('--pdf', action='store_true', help="Generate PDF documentation")
    args = parser.parse_args()

    root_dir = os.path.abspath(os.path.join(os.path.dirname(__file__), "../.."))
    req_file = os.path.join(root_dir, "requirements.yaml")
    
    with open(req_file, 'r') as f:
        data = yaml.safe_load(f)
    
    requirements = data.get("requirements", [])
    
    errors = []
    
    for req in requirements:
        req_id = req['id']
        status = req['status']
        
        if status == 'Complete':
            modules = req.get('module', '')
            if modules == 'None':
                errors.append(f"Requirement {req_id} marked Complete but has no module.")
            
            tests = req.get('test', '')
            if tests == 'None':
                errors.append(f"Requirement {req_id} marked Complete but has no matching test file.")
            
            module_found = search_tag_in_dirs(root_dir, ["src", "app", "cbits"], req_id)
            test_found = search_tag_in_dirs(root_dir, ["test", "bench"], req_id)
            
            if tests == 'Visual Inspection':
                test_found = True
                
            if not test_found:
                errors.append(f"Requirement {req_id} marked Complete but lacks a matching test file (no tag found in tests).")
                
    if errors:
        for err in errors:
            print(f"ERROR: {err}")
        sys.exit(1)
        
    print("All requirements synchronized and verified.")
    generate_markdown(requirements, root_dir)
    
    if args.pdf:
        print("PDF generation requested (stub).")
        
    sys.exit(0)

def search_tag_in_dirs(root, dirs, tag):
    for d in dirs:
        dir_path = os.path.join(root, d)
        if not os.path.exists(dir_path):
            continue
        for root_dir, _, files in os.walk(dir_path):
            for file in files:
                if file.endswith((".hs", ".cpp", ".c", ".h")):
                    path = os.path.join(root_dir, file)
                    with open(path, 'r', encoding='utf-8', errors='ignore') as f:
                        content = f.read()
                        if tag in content:
                            return True
    return False

def get_architecture_links(req_id, arch_doc_path):
    if not os.path.exists(arch_doc_path):
        return "N/A"
    
    with open(arch_doc_path, 'r', encoding='utf-8') as f:
        content = f.read()
        
    start_marker = "<!-- ARCHITECTURE-START -->"
    end_marker = "<!-- ARCHITECTURE-END -->"
    
    start_idx = content.find(start_marker)
    end_idx = content.find(end_marker)
    
    if start_idx == -1 or end_idx == -1:
        return "N/A"
        
    arch_content = content[start_idx:end_idx]
    
    sections = arch_content.split('### Extracted from `')
    links = []
    for section in sections[1:]:
        title = section.split('`', 1)[0]
        if req_id in section:
            links.append(f"[`{title}`](../../Haskell Radar SGRT System Development.md)")
            
    if links:
        return "<br>".join(links)
    return "N/A"

def generate_markdown(requirements, root_dir):
    md_path = os.path.join(root_dir, "docs/iec_62304/traceability_matrix.md")
    
    frs = [r for r in requirements if r['id'].startswith('FR')]
    srs = [r for r in requirements if r['id'].startswith('SR')]
    prs = [r for r in requirements if r['id'].startswith('PR')]
    mrs = [r for r in requirements if r['id'].startswith('MR')]
    
    lines = [
        "# IEC 62304 Traceability Matrix\n",
        "**Note:** For reporting security vulnerabilities, refer to our [Security Policy](../../SECURITY.md).\n",
        "This document provides end-to-end traceability between Functional Requirements (FR), Safety Requirements (SR), Design Elements (Modules), and Verification (Tests) as required by IEC 62304 for Class C medical software.\n"
    ]
    
    sections = [
        ("Functional Requirements (FR)", frs),
        ("Safety Requirements (SR)", srs),
        ("Performance Requirements (PR)", prs),
        ("Mathematical Requirements (MR)", mrs)
    ]
    
    arch_doc_path = os.path.join(root_dir, "Haskell Radar SGRT System Development.md")
    
    for title, reqs in sections:
        if not reqs:
            continue
        lines.append(f"## {title}\n")
        lines.append("| Req ID | Quality Policy Origin | Description | Source Phase | Module | Architecture Section | Verification Test | Status |")
        lines.append("|---|---|---|---|---|---|---|---|")
        for req in reqs:
            arch_links = get_architecture_links(req['id'], arch_doc_path)
            status_str = f"✅ {req['status']}" if req['status'] == 'Complete' else f"❌ {req['status']}"
            lines.append(f"| {req['id']} | {req['policy']} | {req['description']} | {req['phase']} | {req['module']} | {arch_links} | {req['test']} | {status_str} |")
        lines.append("")
        
    with open(md_path, 'w', encoding='utf-8') as f:
        f.write("\n".join(lines))
    print(f"Generated {md_path}")

if __name__ == "__main__":
    main()
