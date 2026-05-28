import re
import sys
import os

MATRIX_PATH = "docs/iec_62304/traceability_matrix.md"
SRC_DIRS = ["src", "app", "cbits"]
TEST_DIRS = ["test", "bench"]
REQ_REGEX = re.compile(r'\b((?:FR|SR|PR)-[A-Z0-9]+-\d+)\b')
SUSPICIOUS_REGEX = re.compile(r'\b((?!FR|SR|PR)[A-Z]{2,4}-[A-Z0-9]+-\d+)\b')

def get_files(dirs, exts):
    files = []
    for d in dirs:
        for root, _, fnames in os.walk(d):
            for f in fnames:
                if any(f.endswith(ext) for ext in exts):
                    files.append(os.path.join(root, f))
    return files

def find_tags_in_files(files):
    tags = {}
    suspicious = {}
    for fpath in files:
        with open(fpath, "r", encoding="utf-8", errors="ignore") as f:
            for line_no, line in enumerate(f, 1):
                matches = REQ_REGEX.findall(line)
                for match in matches:
                    if match not in tags:
                        tags[match] = []
                    tags[match].append(f"{fpath}:{line_no}")
                
                # Check for suspicious non-conforming IDs
                if 'Requirement' in line or 'Req' in line:
                    susp_matches = SUSPICIOUS_REGEX.findall(line)
                    for match in susp_matches:
                        if match not in suspicious:
                            suspicious[match] = []
                        suspicious[match].append(f"{fpath}:{line_no}")
    return tags, suspicious

def main():
    test_log_path = sys.argv[1] if len(sys.argv) > 1 else None
    
    with open(MATRIX_PATH, "r") as f:
        matrix_content = f.read()
    
    matrix_reqs = {}
    for line in matrix_content.split('\n'):
        if line.startswith('|') and 'Req ID' not in line and '---' not in line:
            parts = [p.strip() for p in line.split('|')]
            if len(parts) >= 7:
                req_id = parts[1]
                modules = parts[4]
                tests = parts[5]
                status = parts[6]
                if req_id:
                    matrix_reqs[req_id] = {
                        'modules': modules,
                        'tests': tests,
                        'status': status
                    }
    
    src_files = get_files(SRC_DIRS, ['.hs', '.cpp', '.c', '.h'])
    test_files = get_files(TEST_DIRS, ['.hs', '.cpp', '.c', '.h'])
    
    code_tags, code_susp = find_tags_in_files(src_files)
    test_tags, test_susp = find_tags_in_files(test_files)
    
    all_code_tags = set(code_tags.keys()) | set(test_tags.keys())
    all_susp = set(code_susp.keys()) | set(test_susp.keys())
    matrix_ids = set(matrix_reqs.keys())
    
    errors = []
    
    # 0. Non-conforming IDs
    for req in all_susp:
        errors.append(f"Non-conforming Requirement ID found: {req}")
    
    # 1. Missing in implementation code
    for req in matrix_ids:
        if req not in code_tags:
            errors.append(f"Missing in code: {req} is in matrix but not tagged in any implementation file.")
            
    # 2. Missing in test code
    for req, info in matrix_reqs.items():
        if req not in test_tags:
            if 'Visual Inspection' not in info['tests']:
                errors.append(f"Missing test coverage: {req} lacks test evidence tag.")
            
    # 3. Missing in matrix
    for req in all_code_tags:
        if req not in matrix_ids:
            errors.append(f"Undocumented: {req} is tagged in code/test but not in traceability matrix.")
            
    # 3. Test verification correlation
    test_log_content = ""
    if test_log_path and os.path.exists(test_log_path):
        with open(test_log_path, "r", encoding="utf-8", errors="ignore") as f:
            test_log_content = f.read()
            
        for req, info in matrix_reqs.items():
            if 'Complete' in info['status'] or 'Verified' in info['status']:
                if 'Visual Inspection' in info['tests']:
                    continue
                # If there is a test log, check that we have some test output
                if "Running" not in test_log_content and "Test suite" not in test_log_content:
                    if len(test_log_content) == 0:
                        err = f"Test Correlation Failed: test log is empty for {req}."
                        if err not in errors:
                            errors.append(err)
                
                if "FAIL" in test_log_content or "Failures:" in test_log_content:
                    err = f"Test Correlation Failed: test log indicates failures, cannot verify {req}."
                    if err not in errors:
                        errors.append(err)
    
    if errors:
        report = "COMPLIANCE GATE FAILED!\n\n"
        for err in errors:
            report += f"- {err}\n"
        print(report)
        with open("compliance_report.txt", "w") as f:
            f.write(report)
        sys.exit(1)
    else:
        report = "COMPLIANCE GATE PASSED! All requirements are in sync.\n"
        print(report)
        with open("compliance_report.txt", "w") as f:
            f.write(report)
        sys.exit(0)

if __name__ == "__main__":
    main()
