# Change Control Record

**CCR ID:** CCR-20260624-FFIBRIDGE
**Version:** PR 313

## Problem Description
Current hardware communication lacks a unified diagnostic layer, leading to fragmented audit trails and the potential for developers to accidentally ignore critical return codes, which is unacceptable for IEC 62304 compliance.

## Proposed Change
Implement a centralized High-Assurance FFI Bridge that wraps FFI calls with `MustHandle` and automatically routes all outcomes to the encrypted safety audit system. Update `Watchdog.hs` and `Gating.hs` to use the bridge.

## Impact on Hazards
Impacts H-SYS-008. By routing all calls through the bridge and enforcing explicit error handling, we mitigate the risk of unlogged hardware errors and ignored safety-critical return values. Also mitigates H-SYS-002 (Sensor disconnection) by improving hardware error capture.

## Quality Policy Origin
ISO 13485 Clause 7.3 (Design and Development)

## Verification Strategy
Automated tests will verify that hardware faults are routed through the audit system and properly logged.
