## 2024-05-26 - Timing Attack in WebSocket Authentication
**Vulnerability:** Used non-constant time string matching (`isInfixOf`) for session token verification.
**Learning:** Standard library string matchers fail early on mismatch, causing observable timing discrepancies that leak secret tokens over many requests.
**Prevention:** Always extract exact secret values from headers and use constant-time byte comparisons (`xor` folding) for all authentication checks.
