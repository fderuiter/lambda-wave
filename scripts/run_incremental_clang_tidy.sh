#!/usr/bin/env bash
set -e

# Requirement FR-QC-001, FR-QC-002, FR-QC-003
# Generates compilation database and runs clang-tidy on changed C++ files

# 1. Generate compilation database
./scripts/generate_compile_commands.py

# 2. Get list of changed C++ files, ignoring deleted files
if [ -n "$GITHUB_BASE_REF" ]; then
    CHANGED_FILES=$(git diff --name-only origin/$GITHUB_BASE_REF...HEAD | grep '\.cpp$' || true)
else
    # Fallback to local diff for testing
    CHANGED_FILES=$(git diff --name-only main...HEAD | grep '\.cpp$' || true)
fi

if [ -z "$CHANGED_FILES" ]; then
    echo "No C++ files modified in this PR. Skipping clang-tidy."
    exit 0
fi

# Filter out third-party (ImGui)
FILES_TO_CHECK=""
for f in $CHANGED_FILES; do
    if [[ ! "$f" =~ "imgui/" ]] && [[ -f "$f" ]]; then
        FILES_TO_CHECK="$FILES_TO_CHECK $f"
    fi
done

if [ -z "$FILES_TO_CHECK" ]; then
    echo "No relevant internal C++ files modified. Skipping clang-tidy."
    exit 0
fi

echo "Running clang-tidy on: $FILES_TO_CHECK"

# We enable specific rules for redundant checks, unused variables, and performance
CHECKS="-*,performance-*,readability-redundant-*,misc-unused-*,bugprone-redundant-branch-condition,clang-analyzer-deadcode.DeadStores"
FAILED=0
for f in $FILES_TO_CHECK; do
    echo "Checking $f"
    clang-tidy -p . -checks="$CHECKS" --warnings-as-errors="*" "$f" || FAILED=1
done

if [ $FAILED -ne 0 ]; then
    echo "clang-tidy found issues."
    exit 1
fi

echo "clang-tidy passed successfully."
