#!/bin/bash
set -e

echo "Running centralized Python compliance engine..."
python3 tools/safety_risk_suite.py check-docs

echo "Validating diagram syntax..."
python3 tools/validate_diagrams.py
