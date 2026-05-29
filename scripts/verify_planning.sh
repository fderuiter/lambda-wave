#!/usr/bin/env bash
set -e

echo "Running IEC 62304 Planning Document Verification..."

SDP_FILE="docs/iec_62304/sdp.md"
SCMP_FILE="docs/iec_62304/scmp.md"

if [ ! -f "$SDP_FILE" ]; then
    echo "ERROR: SDP file missing at $SDP_FILE"
    exit 1
fi

if [ ! -f "$SCMP_FILE" ]; then
    echo "ERROR: SCMP file missing at $SCMP_FILE"
    exit 1
fi

# Verify SDP
echo "Verifying SDP..."
if ! grep -iq "^##.*SDLC Model" "$SDP_FILE"; then
    echo "ERROR: SDP is missing 'SDLC Model' section"
    exit 1
fi

if ! grep -iq "^##.*Milestones" "$SDP_FILE"; then
    echo "ERROR: SDP is missing 'Milestones' section"
    exit 1
fi

if ! grep -iq "^##.*RACI Matrix" "$SDP_FILE"; then
    echo "ERROR: SDP is missing 'RACI Matrix' section"
    exit 1
fi

if ! grep -iq "Lead Developer" "$SDP_FILE"; then
    echo "ERROR: SDP RACI Matrix is missing 'Lead Developer' role"
    exit 1
fi

if ! grep -iq "QA/Safety Officer" "$SDP_FILE"; then
    echo "ERROR: SDP RACI Matrix is missing 'QA/Safety Officer' role"
    exit 1
fi

# Verify SCMP
echo "Verifying SCMP..."
if ! grep -iq "^##.*Configuration Management" "$SCMP_FILE"; then
    echo "ERROR: SCMP is missing 'Configuration Management' section"
    exit 1
fi

if ! grep -iq "^##.*Technical Constraints" "$SCMP_FILE"; then
    echo "ERROR: SCMP is missing 'Technical Constraints' section"
    exit 1
fi

if ! grep -iq "90% test coverage" "$SCMP_FILE"; then
    echo "ERROR: SCMP is missing the 90% test coverage requirement"
    exit 1
fi

if ! grep -iq "Compiler Safety Flags" "$SCMP_FILE"; then
    echo "ERROR: SCMP is missing Compiler Safety Flags constraint"
    exit 1
fi

# Verify roadmap names
ROADMAP_FILE="roadmap.md"
if [ -f "$ROADMAP_FILE" ]; then
    echo "Verifying milestone names in $ROADMAP_FILE..."
    if grep -iqE "\*\*([1-9])\.([1-9])\." "$ROADMAP_FILE"; then
        echo "ERROR: Milestone names in roadmap.md may collide with IEC 62304 clauses."
        echo "Found potentially colliding names:"
        grep -iE "\*\*([1-9])\.([1-9])\." "$ROADMAP_FILE"
        exit 1
    fi
fi

echo "Planning verification passed."
exit 0
