#!/usr/bin/env bash
set -e

REGEX="scratchpad|\.todo$|^todo\.txt$|^tasks\.md$|^scratchpad\.md$|~$|\.bak$|\.swp$|^\.vscode/|^\.idea/|^cabal\.project\.local$|^\.cabal-sandbox/"
FORBIDDEN=$(git ls-files | grep -E "$REGEX" || true)
if [ -n "$FORBIDDEN" ]; then
    echo "Error: Found forbidden files in repository:"
    echo "$FORBIDDEN"
    exit 1
fi
exit 0
