#!/usr/bin/env python3

import os
import sys
import yaml
import re
import argparse

def resolve_file(item, root_dir):
    if "." in item and not item.endswith(".hs") and not item.endswith(".cpp") and not item.endswith(".h") and not item.endswith(".md") and not item.endswith(".cabal"):
        # it's a module
        rel_path = item.replace(".", "/") + ".hs"
        for d in ["src", "app", "src-math"]:
            p = os.path.join(root_dir, d, rel_path)
            if os.path.exists(p):
                return p
    else:
        # try as is
        p = os.path.join(root_dir, item)
        if os.path.exists(p):
            return p
        # try in search_dirs
        for d in ["test", "bench"]:
            p = os.path.join(root_dir, d, item)
            if os.path.exists(p):
                return p
    return None

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
    audited_files = set()
    
    for req in requirements:
        req_id = req['id']
        modules_str = req.get('module', '')
        tests_str = req.get('test', '')
        
        modules = re.findall(r'`([^`]+)`', modules_str) if modules_str != 'None' else []
        tests = re.findall(r'`([^`]+)`', tests_str) if tests_str not in ['None', 'Visual Inspection'] else []
        
        paths = []
        
        for m in modules:
            res = resolve_file(m, root_dir)
            if res is None:
                errors.append(f"Requirement {req_id} references a non-existent module file: {m}")
            else:
                paths.append(res)
                
        for t in tests:
            res = resolve_file(t, root_dir)
            if res is None:
                errors.append(f"Requirement {req_id} references a non-existent test file: {t}")
            else:
                paths.append(res)
                
        for p in paths:
            audited_files.add(p)
            with open(p, 'r', encoding='utf-8', errors='ignore') as f:
                content = f.read()
                if req_id not in content:
                    errors.append(f"Requirement {req_id} tag missing in file: {os.path.relpath(p, root_dir)}")
                    
    print("--- Validation Report ---")
    print(f"Total Unique Files Audited: {len(audited_files)}")
    for f in sorted(list(audited_files)):
        print(f" - {os.path.relpath(f, root_dir)}")
    print("-------------------------\n")
    
    if errors:
        print("ERRORS FOUND:")
        for err in errors:
            print(f"ERROR: {err}")
        sys.exit(1)
        
    print("All requirements synchronized and verified.")
    generate_markdown(requirements, root_dir)
    
    if args.pdf:
        print("PDF generation requested (stub).")
        
    sys.exit(0)

def get_architecture_links(req_id, arch_doc_path):
    root_dir = os.path.abspath(os.path.join(os.path.dirname(arch_doc_path), "."))
    arch_dir = os.path.join(root_dir, "docs/architecture")
    links = []

    if os.path.exists(arch_dir):
        for file in sorted(os.listdir(arch_dir)):
            if file.endswith('.md'):
                file_path = os.path.join(arch_dir, file)
                with open(file_path, 'r', encoding='utf-8') as f:
                    content = f.read()
                    if req_id in content:
                        title = file.replace('.md', '').replace('_', ' ').title()
                        links.append(f"[`{title}`](../../docs/architecture/{file})")
    
    if os.path.exists(arch_doc_path):
        with open(arch_doc_path, 'r', encoding='utf-8') as f:
            content = f.read()
        start_marker = "<!-- ARCHITECTURE-START -->"
        end_marker = "<!-- ARCHITECTURE-END -->"
        start_idx = content.find(start_marker)
        end_idx = content.find(end_marker)
        if start_idx != -1 and end_idx != -1:
            arch_content = content[start_idx:end_idx]
            sections = arch_content.split('### Extracted from `')
            for section in sections[1:]:
                sect_title = section.split('`', 1)[0]
                if req_id in section:
                    links.append(f"[`{sect_title}`](../../Haskell Radar SGRT System Development.md)")
                    
    if links:
        seen = set()
        unique_links = []
        for l in links:
            if l not in seen:
                unique_links.append(l)
                seen.add(l)
        return "<br>".join(unique_links)
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
        
    os.makedirs(os.path.dirname(md_path), exist_ok=True)
    with open(md_path, 'w', encoding='utf-8') as f:
        f.write("\n".join(lines))
    print(f"Generated {md_path}")

if __name__ == "__main__":
    main()
