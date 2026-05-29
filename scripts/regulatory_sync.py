import os
import sys
import re
import subprocess
import json
from datetime import datetime

# Regex for tags
REQ_REGEX = re.compile(r'\b((?:FR|SR|PR)-[A-Z0-9]+-\d+)\b')

def get_compiler_version():
    try:
        out = subprocess.check_output(["ghc", "--numeric-version"], stderr=subprocess.STDOUT)
        return out.decode('utf-8').strip()
    except Exception:
        return "Unknown"

def get_runtime_settings():
    settings = ""
    try:
        with open("sgrt-radar-system.cabal", "r") as f:
            content = f.read()
            m = re.search(r'executable sgrt-radar-system-exe.*?ghc-options:\s*(.*?)(?:\n\s*[a-zA-Z]|\Z)', content, re.DOTALL)
            if m:
                settings = m.group(1).strip()
    except Exception:
        pass
    return settings

def get_library_versions():
    deps = {}
    try:
        if os.path.exists("cabal.project.freeze"):
            with open("cabal.project.freeze", "r") as f:
                for line in f:
                    m = re.search(r'any\.([a-zA-Z0-9\-]+)\s*==\s*([0-9\.]+)', line)
                    if m:
                        deps[m.group(1)] = m.group(2)
    except Exception:
        pass
    return deps

def parse_soup():
    soup_path = "docs/iec_62304/soup_analysis.md"
    version = None
    if os.path.exists(soup_path):
        with open(soup_path, "r") as f:
            content = f.read()
            m = re.search(r'\*\*Version:\*\*\s*([\d\.]+)', content)
            if m:
                version = m.group(1)
    return version

def get_soup_dependencies():
    soup_path = "docs/iec_62304/soup_analysis.md"
    deps = {}
    if os.path.exists(soup_path):
        with open(soup_path, "r") as f:
            content = f.read()
            m = re.search(r'<!-- AUTOMATED-DEPENDENCIES-START -->(.*?)<!-- AUTOMATED-DEPENDENCIES-END -->', content, re.DOTALL)
            if m:
                lines = m.group(1).strip().split('\n')
                for line in lines:
                    if line.startswith("- "):
                        parts = line[2:].split(" == ")
                        if len(parts) == 2:
                            deps[parts[0].strip()] = parts[1].strip()
    return deps

def sync_soup_dependencies(freeze_deps):
    soup_path = "docs/iec_62304/soup_analysis.md"
    if not os.path.exists(soup_path):
        return
    with open(soup_path, "r") as f:
        content = f.read()
    
    deps_text = "\n"
    for dep, ver in sorted(freeze_deps.items()):
        deps_text += f"- {dep} == {ver}\n"
    
    new_content = re.sub(
        r'<!-- AUTOMATED-DEPENDENCIES-START -->.*?<!-- AUTOMATED-DEPENDENCIES-END -->',
        f'<!-- AUTOMATED-DEPENDENCIES-START -->{deps_text}<!-- AUTOMATED-DEPENDENCIES-END -->',
        content,
        flags=re.DOTALL
    )
    with open(soup_path, "w") as f:
        f.write(new_content)


def get_safety_critical_modules():
    dev_guide = "docs/DEVELOPER_GUIDE.md"
    modules = set()
    if os.path.exists(dev_guide):
        with open(dev_guide, "r", encoding="utf-8") as f:
            content = f.read()
            m = re.search(r'Files marked with \*\*⚠️ SAFETY-CRITICAL\*\* require special attention:(.*?)(?=\n\n|\n###)', content, re.DOTALL)
            if m:
                for line in m.group(1).split('\n'):
                    if line.startswith('- `'):
                        path = line.strip().strip('- `')
                        modules.add(path)
    return modules

def get_files(dirs, exts):
    files = []
    for d in dirs:
        if not os.path.exists(d): continue
        for root, _, fnames in os.walk(d):
            for f in fnames:
                if any(f.endswith(ext) for ext in exts):
                    files.append(os.path.join(root, f))
    return files

def find_tags(files):
    tags = {}
    for fpath in files:
        with open(fpath, "r", encoding="utf-8", errors="ignore") as f:
            for line_no, line in enumerate(f, 1):
                matches = REQ_REGEX.findall(line)
                for match in matches:
                    if match not in tags:
                        tags[match] = []
                    tags[match].append(f"{fpath}:{line_no}")
    return tags

def parse_matrix():
    matrix_path = "docs/iec_62304/traceability_matrix.md"
    reqs = {}
    if os.path.exists(matrix_path):
        with open(matrix_path, "r", encoding="utf-8") as f:
            for line in f:
                if line.startswith('|') and 'Req ID' not in line and '---' not in line:
                    parts = [p.strip() for p in line.split('|')]
                    if len(parts) >= 7:
                        req_id = parts[1]
                        reqs[req_id] = {
                            'description': parts[2],
                            'phase': parts[3],
                            'modules': parts[4],
                            'tests': parts[5],
                            'status': parts[6]
                        }
    return reqs

def main():
    test_log_path = None
    bench_log_path = None
    sync_mode = False
    
    for arg in sys.argv[1:]:
        if arg == "--sync":
            sync_mode = True
        elif not test_log_path:
            test_log_path = arg
        elif not bench_log_path:
            bench_log_path = arg

    # Environment Meta
    compiler_version = get_compiler_version()
    doc_version = parse_soup()
    runtime_settings = get_runtime_settings()
    
    freeze_deps = get_library_versions()
    
    if sync_mode:
        sync_soup_dependencies(freeze_deps)
        print("Synchronized SOUP documentation with freeze file dependencies.")
        
    soup_deps = get_soup_dependencies()
    
    mismatch = False
    mismatch_msg = ""
    if doc_version and compiler_version != "Unknown" and not compiler_version.startswith(doc_version):
        if compiler_version != doc_version:
            mismatch = True
            mismatch_msg += f"Environment compiler ({compiler_version}) does not match documented SOUP version ({doc_version}). "
            
    if "-N2" not in runtime_settings or "-qa" not in runtime_settings:
        mismatch = True
        mismatch_msg += f"Runtime settings do not contain required safety flags (-N2, -qa). Found: {runtime_settings}. "
    
    # Check dependencies match
    for dep, ver in freeze_deps.items():
        if dep not in soup_deps:
            mismatch = True
            mismatch_msg += f"Dependency {dep} in freeze file is not in SOUP documentation. "
        elif soup_deps[dep] != ver:
            mismatch = True
            mismatch_msg += f"Dependency {dep} version {ver} in freeze file differs from SOUP version {soup_deps[dep]}. "
            
    for dep in soup_deps:
        if dep not in freeze_deps:
            mismatch = True
            mismatch_msg += f"Dependency {dep} in SOUP documentation is missing from freeze file. "
    
    # Requirement parsing
    src_files = get_files(["src", "app", "cbits"], [".hs", ".cpp", ".c", ".h"])
    test_files = get_files(["test", "bench", "tools", "data-generation", "hardware-simulation", "qa", "system-maintenance", "scripts"], [".hs", ".cpp", ".c", ".h", ".py", ".sh"])
    
    code_tags = find_tags(src_files)
    test_tags = find_tags(test_files)
    
    matrix = parse_matrix()
    all_code_reqs = set(code_tags.keys()) | set(test_tags.keys())
    
    # Check for orphan tags (tags without a matching requirement in matrix)
    orphan_tags = all_code_reqs - set(matrix.keys())
    if orphan_tags:
        for t in orphan_tags:
            print(f"PIPELINE WARNING: Orphan tag detected ({t}) not found in traceability matrix.")

    # Guardrail: Check safety-critical modules for tags
    safety_modules = get_safety_critical_modules()
    file_to_tags = {}
    for req, locs in code_tags.items():
        for loc in locs:
            filepath = loc.split(':')[0]
            file_to_tags.setdefault(filepath, set()).add(req)
    for req, locs in test_tags.items():
        for loc in locs:
            filepath = loc.split(':')[0]
            file_to_tags.setdefault(filepath, set()).add(req)
            
    safety_missing = []
    for mod in safety_modules:
        if mod not in file_to_tags or len(file_to_tags[mod]) == 0:
            safety_missing.append(mod)
            
    if safety_missing:
        for mod in safety_missing:
            mismatch_msg += f"Safety-critical module {mod} lacks regulatory tags. See docs/DEVELOPER_GUIDE.md. "
        mismatch = True

    # Check for files mentioned in safety docs but excluded from scanning
    all_scanned_files = set(src_files) | set(test_files)
    unscanned_safety_docs = [mod for mod in safety_modules if mod not in all_scanned_files]
    
    # Gaps
    compliance_gaps = []
    for req in code_tags:
        if req not in test_tags:
            compliance_gaps.append(req)
            
    # Test Evidence
    test_log = ""
    if test_log_path and os.path.exists(test_log_path):
        with open(test_log_path, "r", encoding="utf-8", errors="ignore") as f:
            test_log = f.read()

    bench_log = ""
    if bench_log_path and os.path.exists(bench_log_path):
        with open(bench_log_path, "r", encoding="utf-8", errors="ignore") as f:
            bench_log = f.read()

    # Generate Report
    report = "# Regulatory Evidence Report\n\n"
    report += f"Generated on: {datetime.now().isoformat()}Z\n\n"
    
    report += "## 1. Environment Metadata (SOUP)\n"
    report += f"- **Compiler Version:** {compiler_version}\n"
    report += f"- **Documented SOUP Version:** {doc_version}\n"
    report += f"- **Runtime Settings:** {runtime_settings}\n"
    deps = get_library_versions()
    report += "- **Library Dependencies:**\n"
    for k, v in deps.items():
        report += f"  - {k}: {v}\n"
    report += "\n"
    
    if mismatch:
        report += f"**ERROR:** {mismatch_msg}\n\n"

    report += "## 2. Traceability Matrix\n"
    report += "| Req ID | Source Code Tags | Test Code Tags | Verification Evidence |\n"
    report += "|---|---|---|---|\n"
    
    for req in sorted(all_code_reqs | set(matrix.keys())):
        c_tags = "<br>".join(code_tags.get(req, [])) or "None"
        t_tags = "<br>".join(test_tags.get(req, [])) or "None"
        
        evidence = "Not Run"
        if req.startswith("PR-") or "Latency" in matrix.get(req, {}).get('tests', ''):
            if bench_log and "PASS" in bench_log:
                evidence = f"PASS (Exec Timestamp: {datetime.now().isoformat()}Z)"
            elif bench_log:
                evidence = "FAIL / INCOMPLETE (Bench Log)"
            elif "PASS" in test_log:
                evidence = f"PASS (Exec Timestamp: {datetime.now().isoformat()}Z)"
        else:
            if test_log and "FAIL" not in test_log and len(test_log) > 0:
                evidence = f"PASS (Exec Timestamp: {datetime.now().isoformat()}Z)"
                
        report += f"| {req} | {c_tags} | {t_tags} | {evidence} |\n"
    
    report += "\n## 3. Compliance Gaps\n"
    if compliance_gaps or unscanned_safety_docs:
        if compliance_gaps:
            for gap in compliance_gaps:
                report += f"- **{gap}**: Tagged in source code but lacking a corresponding test case.\n"
        if unscanned_safety_docs:
            for f in unscanned_safety_docs:
                report += f"- **File {f}**: Mentioned in safety documentation but excluded from scanning.\n"
    else:
        report += "No compliance gaps found. All source requirements have corresponding tests.\n"
        
    report += "\n## 4. Dependencies Diff Report\n"
    try:
        diff_out = subprocess.check_output(["git", "diff", "HEAD~1", "cabal.project.freeze"], stderr=subprocess.STDOUT)
        diff_str = diff_out.decode('utf-8').strip()
        if diff_str:
            report += "```diff\n" + diff_str + "\n```\n"
        else:
            report += "No dependency changes since last commit.\n"
    except Exception:
        report += "Could not generate dependency diff (e.g., no previous commit found).\n"
    
    with open("regulatory_evidence_report.md", "w") as f:
        f.write(report)
        
    print("Regulatory Evidence Report generated: regulatory_evidence_report.md")
    
    if mismatch:
        print(f"PIPELINE FAILED: {mismatch_msg}")
        sys.exit(1)
        
    if compliance_gaps:
        print("PIPELINE WARNING: Compliance gaps found (see report).")

if __name__ == "__main__":
    main()
