import sys
import re
import os

def load_config(file_path):
    config = {'default': {'expressions': 90.0, 'branches': 90.0}, 'modules': {}}
    if not os.path.exists(file_path):
        return config

    with open(file_path, 'r') as f:
        lines = f.readlines()

    current_section = None
    current_module = None

    for line in lines:
        stripped = line.strip()
        if not stripped or stripped.startswith('#'):
            continue
        
        indent = len(line) - len(line.lstrip())

        if indent == 0 and stripped.endswith(':'):
            current_section = stripped[:-1]
            current_module = None
            continue
            
        if current_section == 'modules':
            if indent > 0:
                if stripped.endswith(':'):
                    current_module = stripped[:-1].strip()
                    config['modules'][current_module] = {}
                elif ':' in stripped and current_module:
                    key, val = [x.strip() for x in stripped.split(':', 1)]
                    config['modules'][current_module][key] = float(val)

        elif current_section == 'default':
            if indent > 0 and ':' in stripped:
                key, val = [x.strip() for x in stripped.split(':', 1)]
                config['default'][key] = float(val)

    return config

def parse_report_blocks(text):
    blocks = {}
    lines = text.splitlines()
    current_module = None
    
    for i, line in enumerate(lines):
        is_header = False
        if i + 1 < len(lines):
            next_line = lines[i+1].strip()
            if next_line.startswith('~~~') and len(next_line) >= 3:
                is_header = True
                
        if line.strip().endswith(':'):
            potential_name = line.strip()[:-1]
            if re.match(r'^[A-Za-z0-9_.]+$', potential_name):
                current_module = potential_name
                if current_module not in blocks:
                    blocks[current_module] = {'expressions': None, 'branches': None}
                continue
            else:
                current_module = None
                
        if is_header:
            potential_name = line.strip()
            if re.match(r'^[A-Za-z0-9_.]+$', potential_name):
                current_module = potential_name
                if current_module not in blocks:
                    blocks[current_module] = {'expressions': None, 'branches': None}
                continue
            else:
                current_module = None
                
        if current_module:
            expr_match = re.search(r'(\d+(?:\.\d+)?)%\s+expressions used', line)
            if expr_match:
                blocks[current_module]['expressions'] = float(expr_match.group(1))
            
            alt_match = re.search(r'(\d+(?:\.\d+)?)%\s+alternatives used', line)
            if alt_match:
                blocks[current_module]['branches'] = float(alt_match.group(1))
                
    return blocks

def parse_coverage(file_path):
    config_path = os.path.join(os.path.dirname(os.path.dirname(os.path.abspath(__file__))), 'coverage.yaml')
    config = load_config(config_path)
    
    with open(file_path, 'r') as f:
        text = f.read()

    blocks = parse_report_blocks(text)
    
    # We should still support checking global summary if no modules are found,
    # but the requirement is to evaluate module rules.
    if not blocks:
        # Fallback to checking the report as a whole, to not fail entirely
        # if the report format has no modules (though unexpected).
        expr_match = re.search(r'(\d+(?:\.\d+)?)%\s+expressions used', text)
        alt_match = re.search(r'(\d+(?:\.\d+)?)%\s+alternatives used', text)
        if expr_match or alt_match:
            blocks['Global'] = {}
            if expr_match:
                blocks['Global']['expressions'] = float(expr_match.group(1))
            if alt_match:
                blocks['Global']['branches'] = float(alt_match.group(1))
        else:
            print("Could not find coverage metrics in report.")
            sys.exit(1)
        
    failed = False
    
    for module, metrics in blocks.items():
        target_expr = config['default'].get('expressions', 90.0)
        target_branch = config['default'].get('branches', 90.0)
        
        if module in config.get('modules', {}):
            mod_conf = config['modules'][module]
            target_expr = mod_conf.get('expressions', target_expr)
            target_branch = mod_conf.get('branches', target_branch)
            
        expr_cov = metrics.get('expressions')
        branch_cov = metrics.get('branches')
        
        if expr_cov is not None:
            if expr_cov < target_expr:
                print(f"FAIL: {module} expression coverage is {expr_cov}%, below {target_expr}%.")
                failed = True
            else:
                print(f"PASS: {module} expression coverage is {expr_cov}%.")
                
        if branch_cov is not None:
            if branch_cov < target_branch:
                print(f"FAIL: {module} branch/alternatives coverage is {branch_cov}%, below {target_branch}%.")
                failed = True
            else:
                print(f"PASS: {module} branch/alternatives coverage is {branch_cov}%.")
                
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
