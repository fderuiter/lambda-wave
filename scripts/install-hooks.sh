#!/usr/bin/env bash
set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(dirname "$SCRIPT_DIR")"

echo "Installing git pre-commit hook..."
cd "$REPO_ROOT"
if ! command -v pre-commit >/dev/null 2>&1; then
    echo "Installing pre-commit..."
    pip install --break-system-packages pre-commit
fi
pre-commit install
echo "Done!"
