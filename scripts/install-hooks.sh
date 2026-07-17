#!/bin/bash
# Install the git hooks using the pre-commit framework

if command -v pre-commit >/dev/null 2>&1; then
    pre-commit install
else
    echo "pre-commit framework is not installed."
    echo "Please run 'pip install pre-commit' first."
    exit 1
fi
echo "Git hooks successfully initialized."
