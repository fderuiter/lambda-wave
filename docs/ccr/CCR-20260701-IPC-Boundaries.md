# Change Control Record

**CCR ID:** CCR-20260701-IPC-Boundaries
**Version:** HEAD

## Problem Description
The architecture relies on separate processes for the safety watchdog and visualizer to ensure isolation. However, inter-process communication (IPC) failures (e.g., stale sockets or hung parent processes) can compromise safety and lead to uncontrolled states.

## Proposed Change
Document process boundaries and enforce strict IPC mechanisms. Introduce automated CI checks to ensure process spawning is documented and IPC mechanisms have identified failure modes and mitigations. Safety comments and documentation were added to `Watchdog.hs`.

## Impact on Hazards
Impacts H-SYS-010. This change mitigates the risk of IPC failures by identifying failure modes (like stale sockets preventing the safety daemon from binding, or lost heartbeats) and ensuring mitigations (like removing sockets before bind and strict receive timeouts).

## Quality Policy Origin
ISO 13485 Clause 7.3

## Verification Strategy
Automated CI scripts (`check_process_boundaries.py`) check for documented process boundaries. System tests verify that stale sockets are removed and heartbeat timeouts force a hardware shutdown.
