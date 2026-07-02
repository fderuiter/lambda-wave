# Change Control Record

**CCR ID:** CCR-20260702-CPP-HUD
**Version:** 4f3aeae7f78a068be99ea295a4d9f89fffeaf10b

## Problem Description
The previous dual-path system relied on a complex Haskell web stack and various JavaScript libraries (SOUP), which complicated regulatory certification processes and added latency to the real-time respiratory traces.

## Proposed Change
We consolidated the management and visualization interfaces into a native C++ application using Dear ImGui. We completely removed the Haskell-based web server and associated JavaScript assets, as well as the cryptographic functions `encryptWebsocket` and `decryptWebsocket` from `Safety.Crypto`.

## Impact on Hazards
The removal of the web stack mitigates H-SYS-011. By eliminating the network-based delivery system, it also removes the risk of network-induced jitter and SOUP vulnerabilities.

## Quality Policy Origin
ISO 13485 Clause 7.3.3 Design and Development Outputs (reducing SOUP).

## Verification Strategy
Verified by confirming the removal of the web server dependencies, observing UI performance at 60 FPS in the native C++ HUD, and automated tests for the remaining safe cryptographic functions.
