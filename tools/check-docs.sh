#!/bin/bash
set -e

echo "Checking safety-critical modules for mandatory documentation sections..."

# Find all Haskell source files containing the SAFETY-CRITICAL tag
SAFETY_FILES=$(grep -rl "⚠️ SAFETY-CRITICAL" src/ || true)

FAILED=0

for file in $SAFETY_FILES; do
    echo "Checking $file..."
    
    HAS_FAILURE_MODE=$(grep -E "= Failure Mode" "$file" || true)
    HAS_MITIGATION=$(grep -E "= Mitigation" "$file" || true)
    
    if [ -z "$HAS_FAILURE_MODE" ]; then
        echo "Error: Missing '= Failure Mode' in $file"
        FAILED=1
    fi
    
    if [ -z "$HAS_MITIGATION" ]; then
        echo "Error: Missing '= Mitigation' in $file"
        FAILED=1
    fi
done

if [ $FAILED -ne 0 ]; then
    echo "Documentation check failed: Mandatory safety sections missing."
    exit 1
fi

echo "Documentation check passed: All safety-critical modules have required sections."
