#!/usr/bin/env python3
import sys
import os
import yaml
import re
import argparse
from pathlib import Path

RMF_PATH = Path('/app/rmf.yaml')
SRC_DIRS = ['/app/src', '/app/src-math', '/app/test', '/app/app', '/app/cbits']

def load_rmf():
    if not RMF_PATH.exists():
        print(f"Error: {RMF_PATH} not found.")
        sys.exit(1)
    with open(RMF_PATH, 'r') as f:
        data = yaml.safe_load(f)
    return data.get('hazards', [])

def save_rmf(hazards):
    with open(RMF_PATH, 'w') as f:
        yaml.dump({'hazards': hazards}, f, sort_keys=False)

def calc_rpn(h):
    return h.get('severity', 1) * h.get('occurrence', 1) * h.get('detection', 1)

def cmd_add(args):
    hazards = load_rmf()
    new_h = {
        'id': args.id,
        'description': args.description,
        'cause': args.cause,
        'effect': args.effect,
        'mitigation': args.mitigation,
        'severity': args.severity,
        'occurrence': args.occurrence,
        'detection': args.detection,
    }
    new_h['rpn'] = calc_rpn(new_h)
    hazards.append(new_h)
    save_rmf(hazards)
    print(f"Added hazard {args.id} with RPN {new_h['rpn']}")

def cmd_update(args):
    hazards = load_rmf()
    found = False
    for h in hazards:
        if h['id'] == args.id:
            h['description'] = args.description or h['description']
            h['cause'] = args.cause or h['cause']
            h['effect'] = args.effect or h['effect']
            h['mitigation'] = args.mitigation or h['mitigation']
            if args.severity: h['severity'] = args.severity
            if args.occurrence: h['occurrence'] = args.occurrence
            if args.detection: h['detection'] = args.detection
            h['rpn'] = calc_rpn(h)
            found = True
            print(f"Updated hazard {args.id} with new RPN {h['rpn']}")
            break
    if not found:
        print(f"Hazard {args.id} not found.")
        sys.exit(1)
    save_rmf(hazards)

def cmd_list(args):
    hazards = load_rmf()
    for h in hazards:
        print(f"[{h['id']}] RPN: {h.get('rpn', calc_rpn(h))} - {h['description']}")

def cmd_report(args):
    from fpdf.enums import XPos, YPos
    hazards = load_rmf()
    pdf = FPDF()
    pdf.add_page()
    pdf.set_font("helvetica", 'B', 16)
    pdf.cell(190, 10, text="Risk Management Report", new_x=XPos.LMARGIN, new_y=YPos.NEXT, align='C')
    pdf.ln(10)

    pdf.set_font("helvetica", size=10)
    for h in hazards:
        pdf.set_font("helvetica", 'B', 12)
        pdf.cell(190, 10, text=f"Hazard ID: {h['id']}", new_x=XPos.LMARGIN, new_y=YPos.NEXT)
        pdf.set_font("helvetica", size=10)
        pdf.multi_cell(190, 10, text=f"Description: {h['description']}")
        pdf.multi_cell(190, 10, text=f"Cause: {h.get('cause', 'N/A')}")
        pdf.multi_cell(190, 10, text=f"Effect: {h.get('effect', 'N/A')}")
        pdf.multi_cell(190, 10, text=f"Mitigation: {h.get('mitigation', 'N/A')}")
        
        pdf.set_font("helvetica", 'B', 10)
        s = h.get('severity', 1)
        o = h.get('occurrence', 1)
        d = h.get('detection', 1)
        rpn = h.get('rpn', s * o * d)
        pdf.cell(190, 10, text=f"Severity: {s} | Occurrence: {o} | Detection: {d} | RPN: {rpn}", new_x=XPos.LMARGIN, new_y=YPos.NEXT)
        pdf.ln(5)
    
    out_file = args.output or "Risk_Management_Report.pdf"
    pdf.output(out_file)
    print(f"Report generated at {out_file}")

def get_codebase_hazards():
    hazard_refs = set()
    regex = re.compile(r'Hazard\s+(H-[A-Z0-9\-]+)')
    for d in SRC_DIRS:
        for root, dirs, files in os.walk(d):
            for file in files:
                if file.endswith(('.hs', '.cpp', '.h')):
                    path = os.path.join(root, file)
                    with open(path, 'r', encoding='utf-8', errors='ignore') as f:
                        for line in f:
                            match = regex.search(line)
                            if match:
                                hazard_refs.add(match.group(1))
    return hazard_refs

def cmd_verify(args):
    hazards = load_rmf()
    refs = get_codebase_hazards()
    missing = []
    for h in hazards:
        if h['id'] not in refs:
            missing.append(h['id'])
    
    if missing:
        print("ERROR: The following hazards lack verified mitigation evidence in the codebase:")
        for m in missing:
            print(f" - {m}")
        sys.exit(1)
    else:
        print("Verification PASSED: All hazards are referenced by at least one software component.")

def cmd_gap(args):
    hazards = load_rmf()
    refs = get_codebase_hazards()
    missing = [h['id'] for h in hazards if h['id'] not in refs]
    if missing:
        print("GAP ANALYSIS REPORT:")
        print("The following hazards lack codebase references:")
        for m in missing:
            print(f" - {m}")
    else:
        print("GAP ANALYSIS REPORT: No gaps found. All hazards are mitigated in code.")

def cmd_check_docs(args):
    print("Checking safety-critical modules for mandatory documentation sections...")
    req_path = Path('/app/requirements.yaml')
    valid_ids = set()
    if req_path.exists():
        with open(req_path, 'r') as f:
            data = yaml.safe_load(f)
            reqs = data.get('requirements', [])
            for r in reqs:
                valid_ids.add(r['id'])
                
    hazards = load_rmf()
    for h in hazards:
        valid_ids.add(h['id'])

    failed = False

    # Check Master Spec
    spec_path = Path('/app/docs/ffi_master_spec.md')
    if not spec_path.exists():
        print("Error: ffi_master_spec.md not found.")
        failed = True
    else:
        with open(spec_path, 'r') as f:
            spec_content = f.read()
        match = re.search(r'```yaml\n(.*?)\n```', spec_content, re.DOTALL)
        if match:
            master_spec = yaml.safe_load(match.group(1)).get('ffi_functions', {})
            # Read bridge layer mapping
            common_hs_path = '/app/src/Hardware/FFI/Common.hs'
            if os.path.exists(common_hs_path):
                with open(common_hs_path, 'r') as f:
                    common_hs = f.read()
                
                for func, data in master_spec.items():
                    for ret_code, ret_info in data.get('return_codes', {}).items():
                        # Verify audit event is present
                        if not ret_info.get('audit_event'):
                            print(f"Error: Missing audit_event for {func} return code {ret_code}")
                            failed = True
                        
                        # Verify mapping exists in Common.hs (simplified check)
                        hr = ret_info.get('hardware_result')
                        # It should map in Common.hs, either in toHardwareResult or toRingBufferResult
                        if hr and hr not in common_hs:
                            print(f"Error: Documented return code {ret_code} mapping {hr} not found in bridge layer.")
                            failed = True
            else:
                print("Error: /app/src/Hardware/FFI/Common.hs not found.")
                failed = True
        else:
            print("Error: No YAML block found in master spec.")
            failed = True
    
    req_regex = re.compile(r'\b((?:FR|SR|PR|MR)(?:-[A-Z0-9]+)*-\d+)\b')
    hazard_regex = re.compile(r'\b(H-[A-Z0-9\-]+)\b')
    
    for d in SRC_DIRS:
        for root, dirs, files in os.walk(d):
            for file in files:
                if file.endswith(('.hs', '.cpp', '.h', '.c', '.hpp', '.cc', '.cxx')):
                    fpath = os.path.join(root, file)
                    with open(fpath, 'r', encoding='utf-8', errors='ignore') as f:
                        content = f.read()
                        
                    is_ffi_module = 'Hardware/FFI' in fpath and file.endswith('.hs')
                    is_safety_critical = "SAFETY-CRITICAL" in content
                    
                    if is_safety_critical or is_ffi_module:
                        print(f"Checking {fpath}...")
                        if not re.search(r'=\s*Failure Mode', content):
                            print(f"Error: Missing '= Failure Mode' in {fpath}")
                            failed = True
                        if not re.search(r'=\s*Mitigation', content):
                            print(f"Error: Missing '= Mitigation' in {fpath}")
                            failed = True
                        if is_ffi_module and not re.search(r'=\s*Audit Events', content):
                            print(f"Error: Missing '= Audit Events' in {fpath}")
                            failed = True
                            
                        reqs_found = req_regex.findall(content)
                        hazards_found = hazard_regex.findall(content)
                        
                        for req in reqs_found:
                            if req not in valid_ids:
                                print(f"Error: Non-existent requirement ID referenced: {req} in {fpath}")
                                failed = True
                        for haz in hazards_found:
                            if haz not in valid_ids:
                                print(f"Error: Non-existent hazard ID referenced: {haz} in {fpath}")
                                failed = True

    if failed:
        print("Documentation check failed: Mandatory safety sections missing or invalid IDs.")
        sys.exit(1)
        
    print("Documentation check passed: All safety-critical modules have required sections and valid IDs.")

def cmd_check_ffi_layouts(args):
    print("Verifying FFI Memory Layouts between markdown and Haskell source...")
    
    spec_path = Path('/app/docs/ffi_master_spec.md')
    if not spec_path.exists():
        print(f"Error: {spec_path} not found.")
        sys.exit(1)
        
    with open(spec_path, 'r', encoding='utf-8') as f:
        lines = f.readlines()
        
    md_layouts = {}
    current_layout = None
    in_ffi_section = False
    
    for i, line in enumerate(lines):
        line_no = i + 1
        if line.startswith('## FFI Memory Layouts'):
            in_ffi_section = True
            continue
        elif line.startswith('## ') and in_ffi_section:
            in_ffi_section = False
            
        if in_ffi_section:
            m = re.match(r'^###\s+(\w+)', line)
            if m:
                current_layout = m.group(1)
                md_layouts[current_layout] = []
                continue
                
            if current_layout:
                field_match = re.search(r'^-\s+`([^`]+)`', line.strip())
                if field_match:
                    declaration = field_match.group(1).strip()
                    parts = declaration.split()
                    if len(parts) >= 2:
                        field_name = parts[-1]
                        field_type = " ".join(parts[:-1])
                        md_layouts[current_layout].append({
                            'name': field_name,
                            'type': field_type,
                            'line_text': line.strip(),
                            'line_no': line_no
                        })
                        
    md_layouts = {k: v for k, v in md_layouts.items() if len(v) > 0}
    
    hs_records = {}
    record_pattern = re.compile(r'data\s+([A-Z][A-Za-z0-9_]*)\s*=\s*[A-Z][A-Za-z0-9_]*\s*\{([^}]+)\}')
    field_pattern = re.compile(r'([a-z][A-Za-z0-9_]*)\s*::\s*!?([A-Z][A-Za-z0-9_]*)')

    for d in SRC_DIRS:
        for root, dirs, files in os.walk(d):
            for file in files:
                if file.endswith('.hs'):
                    path = os.path.join(root, file)
                    with open(path, 'r', encoding='utf-8') as f:
                        content = f.read()
                    
                    for match in record_pattern.finditer(content):
                        record_name = match.group(1)
                        fields_str = match.group(2)
                        
                        fields = []
                        for fmatch in field_pattern.finditer(fields_str):
                            fields.append({
                                'name': fmatch.group(1),
                                'type': fmatch.group(2)
                            })
                        if fields:
                            hs_records[record_name] = {'fields': fields, 'file': path}
                            
    def snake_to_camel(name):
        parts = name.split('_')
        return parts[0] + ''.join(p.capitalize() for p in parts[1:])

    failed = False
    
    for layout_name, md_fields in md_layouts.items():
        if layout_name not in hs_records:
            print(f"Error: Documented layout '{layout_name}' not found in any Haskell FFI source records.")
            failed = True
            continue
            
        hs_info = hs_records[layout_name]
        hs_fields = hs_info['fields']
        hs_file = hs_info['file']
        
        if len(md_fields) != len(hs_fields):
            print(f"Error: Structural mismatch in '{layout_name}'. Markdown has {len(md_fields)} fields, Haskell has {len(hs_fields)} fields.")
            print(f"File: {hs_file}")
            failed = True
            continue
            
        for i, (md_f, hs_f) in enumerate(zip(md_fields, hs_fields)):
            expected_name = snake_to_camel(md_f['name'])
            if expected_name != hs_f['name']:
                print(f"Error: Structural mismatch in '{layout_name}' at field {i+1}.")
                print(f"Spec File: {spec_path}")
                print(f"Header: ### {layout_name}")
                print(f"Mismatched line ({md_f['line_no']}): {md_f['line_text']}")
                print(f"Expected field name '{expected_name}' but found '{hs_f['name']}' in {hs_file}.")
                failed = True
                break
                
            type_mapping = {
                'std::atomic<size_t>': 'CSize',
                'size_t': 'CSize',
                'uint32_t': 'Word32',
                'int': 'CInt',
            }
            mapped_type = type_mapping.get(md_f['type'])
            if mapped_type and mapped_type != hs_f['type']:
                print(f"Error: Type mismatch in '{layout_name}' at field {i+1}.")
                print(f"Spec File: {spec_path}")
                print(f"Header: ### {layout_name}")
                print(f"Mismatched line ({md_f['line_no']}): {md_f['line_text']}")
                print(f"Expected type '{mapped_type}' but found '{hs_f['type']}' in {hs_file}.")
                failed = True
                break

    if failed:
        sys.exit(1)
        
    print("Verification PASSED: Documented FFI memory layouts perfectly match Haskell source records.")

def main():
    parser = argparse.ArgumentParser(description="Integrated Safety & Risk Suite")
    subparsers = parser.add_subparsers(dest="command", required=True)

    add_p = subparsers.add_parser("add", help="Add a new hazard")
    add_p.add_argument("--id", required=True)
    add_p.add_argument("--description", required=True)
    add_p.add_argument("--cause", required=True)
    add_p.add_argument("--effect", required=True)
    add_p.add_argument("--mitigation", required=True)
    add_p.add_argument("--severity", type=int, required=True)
    add_p.add_argument("--occurrence", type=int, required=True)
    add_p.add_argument("--detection", type=int, required=True)

    up_p = subparsers.add_parser("update", help="Update a hazard")
    up_p.add_argument("--id", required=True)
    up_p.add_argument("--description")
    up_p.add_argument("--cause")
    up_p.add_argument("--effect")
    up_p.add_argument("--mitigation")
    up_p.add_argument("--severity", type=int)
    up_p.add_argument("--occurrence", type=int)
    up_p.add_argument("--detection", type=int)

    subparsers.add_parser("list", help="List hazards")
    
    rep_p = subparsers.add_parser("report", help="Generate PDF report")
    rep_p.add_argument("--output", "-o", help="Output PDF path")

    subparsers.add_parser("verify", help="Verify all hazards are referenced in codebase")
    subparsers.add_parser("gap", help="Gap analysis for missing codebase references")
    subparsers.add_parser("check-docs", help="Check documentation structure in safety-critical files")
    subparsers.add_parser("check-ffi-layouts", help="Verify FFI Memory Layouts between markdown and Haskell source")

    args = parser.parse_args()
    if args.command == "add":
        cmd_add(args)
    elif args.command == "update":
        cmd_update(args)
    elif args.command == "list":
        cmd_list(args)
    elif args.command == "report":
        cmd_report(args)
    elif args.command == "verify":
        cmd_verify(args)
    elif args.command == "gap":
        cmd_gap(args)
    elif args.command == "check-docs":
        cmd_check_docs(args)
    elif args.command == "check-ffi-layouts":
        cmd_check_ffi_layouts(args)

if __name__ == "__main__":
    main()
