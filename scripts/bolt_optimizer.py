#!/usr/bin/env python3
import sys
import re

def optimize_bolt(content):
    # Transform standard high-level math calls into safety-compliant, zero-SOUP 'Bolt' implementations
    # Replace sum (zipWith (*) v1 v2) with dot v1 v2
    content = re.sub(r'sum\s*\(\s*zipWith\s*\(\*\)\s+([a-zA-Z0-9_]+)\s+([a-zA-Z0-9_]+)\s*\)', r'dot \1 \2', content)
    content = re.sub(r'sum\s*\$\s*zipWith\s*\(\*\)\s+([a-zA-Z0-9_]+)\s+([a-zA-Z0-9_]+)', r'dot \1 \2', content)
    
    # matrix multiplication
    # Replace A <> B with multiply A B
    content = re.sub(r'([a-zA-Z0-9_]+)\s*<>\s*([a-zA-Z0-9_]+)', r'multiply \1 \2', content)
    
    # mat-vec mult
    content = re.sub(r'([a-zA-Z0-9_]+)\s*#>\s*([a-zA-Z0-9_]+)', r'matVecMult \1 \2', content)

    return content

if __name__ == "__main__":
    if len(sys.argv) < 2:
        print("Usage: bolt_optimizer.py <file>")
        sys.exit(1)
        
    for filepath in sys.argv[1:]:
        with open(filepath, 'r') as f:
            content = f.read()
            
        optimized = optimize_bolt(content)
        
        with open(filepath, 'w') as f:
            f.write(optimized)
            
    print("Bolt optimizations applied.")
