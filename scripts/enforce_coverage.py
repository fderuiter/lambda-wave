import sys
import re

def parse_coverage(file_path):
    with open(file_path, 'r') as f:
        text = f.read()

    # Look for percentages
    # Example: " 85% expressions used (100/117)"
    # We want to check all reported coverage metrics.
    
    threshold = 90
    failed = False
    
    # Often branch coverage correlates with "alternatives used" and "boolean coverage"
    # We will enforce 90% on expressions and alternatives
    
    expr_match = re.search(r'(\d+)%\s+expressions used', text)
    alt_match = re.search(r'(\d+)%\s+alternatives used', text)
    
    if not expr_match and not alt_match:
        print("Could not find coverage metrics in report.")
        sys.exit(1)
        
    if expr_match:
        expr_cov = int(expr_match.group(1))
        if expr_cov < threshold:
            print(f"FAIL: Expression coverage is {expr_cov}%, below {threshold}%.")
            failed = True
        else:
            print(f"PASS: Expression coverage is {expr_cov}%.")
            
    if alt_match:
        alt_cov = int(alt_match.group(1))
        if alt_cov < threshold:
            print(f"FAIL: Branch/Alternatives coverage is {alt_cov}%, below {threshold}%.")
            failed = True
        else:
            print(f"PASS: Branch/Alternatives coverage is {alt_cov}%.")

    if failed:
        sys.exit(1)
    else:
        print("Coverage requirements met.")
        sys.exit(0)

if __name__ == "__main__":
    if len(sys.argv) < 2:
        print("Usage: python3 enforce_coverage.py <report.txt>")
        sys.exit(1)
    parse_coverage(sys.argv[1])
