# SOUP Analysis (GHC RTS)

**Software of Unknown Provenance (SOUP) Evaluation for IEC 62304**

**Note:** For vulnerabilities in SOUP, please refer to our [Security Policy](../../SECURITY.md).

**SOUP Item:** Glasgow Haskell Compiler Runtime System (GHC RTS)
**Version:** <!-- METADATA:ghc_version -->9.6.7<!-- /METADATA:ghc_version -->
**Purpose:** Provides memory management (Garbage Collection), thread scheduling, and runtime services for the Lambda-Wave application.

## 1. Description and Purpose
The GHC RTS is a core component required to execute compiled Haskell code. It handles:
- Memory allocation and Garbage Collection (GC)
- Green thread (lightweight thread) scheduling
- Foreign Function Interface (FFI) boundaries
- Exception handling and software transactional memory (STM)

## 2. Intended Use Context
Lambda-Wave is an IEC 62304 Class C Medical Software system. The GHC RTS runs in the critical path of the radar data ingestion, signal processing, and gating control loop. The loop has a hard real-time latency constraint of <15ms.

## 3. Potential Hazards & Risk Control Measures

*Note: For the complete, structured Risk Management File containing full quantitative FMEA metrics (Severity, Occurrence, Detection, RPN) and traceability to software components, please refer to `rmf.yaml` in the repository root. This is managed via the `tools/safety_risk_suite.py` CLI.*

## 4. Anomaly List Assessment
GHC <!-- METADATA:ghc_version -->9.6.7<!-- /METADATA:ghc_version --> is a mature release. Known issues in the GHC issue tracker have been reviewed. No open bugs related to bounded GC or core thread scheduling were identified that affect the specific subset of features used by Lambda-Wave.

Additionally, the base Debian 11.11 image (`haskell:<!-- METADATA:ghc_version -->9.6.7<!-- /METADATA:ghc_version -->-slim-bullseye`) contains a known vulnerability in `libssh2-1` (CVE-2026-55200) and kernel vulnerabilities in `linux-libc-dev` (CVE-2026-46064, CVE-2026-46068, CVE-2026-46069, CVE-2026-46075, CVE-2026-46122). These vulnerabilities are deemed acceptable and ignored by our security scans because the medical device operates in an air-gapped network segment without running an SSH server or client, and the kernel vulnerabilities in headers do not affect the statically compiled binaries or the running system since the system kernel is managed externally by the OS layer, making the vulnerable paths unreachable.

## 5. Justification for Acceptability
The GHC RTS is deemed acceptable for use in this Class C system because:
1. The project architecture relies on a minimal subset of RTS features.
2. The "hot path" processing avoids heap allocations, neutralizing the primary source of RTS non-determinism (Garbage Collection).
3. The system implements an independent, external safety watchdog to detect and mitigate any complete RTS failures or lockups.
4. GHC <!-- METADATA:ghc_version -->9.6.7<!-- /METADATA:ghc_version --> has been heavily battle-tested in industrial contexts, and the specific RTS flags (`-N2 -qa -l`) provide predictable affinity behavior.

## 6. Pinned Library Dependencies
<!-- AUTOMATED-DEPENDENCIES-START -->
- Glob == 0.10.2
- HUnit == 1.6.2.0
- OneTuple == 0.4.3
- Only == 0.1
- QuickCheck == 2.18.0.0
- StateVar == 1.2.2
- aeson == 2.2.5.0
- ansi-terminal == 1.1.5
- ansi-terminal-types == 1.1.3
- array == 0.5.8.0
- assoc == 1.1.1
- async == 2.2.6
- attoparsec == 0.14.4
- base == 4.18.3.0
- base-compat == 0.15.0
- base-compat-batteries == 0.15.0
- base-orphans == 0.9.4
- base16 == 1.0
- basement == 0.0.16
- bifunctors == 5.6.3
- binary == 0.8.9.1
- binary-orphans == 1.0.6
- bitvec == 1.1.6.0
- bytestring == 0.11.5.3
- call-stack == 0.4.0
- cassava == 0.5.4.1
- character-ps == 0.1
- code-page == 0.2.1
- colour == 2.3.7
- comonad == 5.0.10
- containers == 0.6.7
- contravariant == 1.5.6
- criterion == 1.6.5.0
- criterion-measurement == 0.2.4.0
- crypton == 1.1.3
- data-default == 0.8.0.2
- data-default-class == 0.2.0.0
- data-fix == 0.3.4
- deepseq == 1.4.8.1
- dense-linear-algebra == 0.1.0.0
- directory == 1.3.10.1
- distributive == 0.6.3
- dlist == 1.0
- exceptions == 0.10.7
- file-io == 0.1.6
- filepath == 1.4.301.0
- ghc-bignum == 1.3
- ghc-boot-th == <!-- METADATA:ghc_version -->9.6.7<!-- /METADATA:ghc_version -->
- ghc-prim == 0.10.0
- hashable == 1.5.1.0
- haskell-lexer == 1.2.1
- hsc2hs == 0.68.10
- hspec == 2.11.17
- hspec-core == 2.11.17
- hspec-discover == 2.11.17
- hspec-expectations == 0.8.4
- indexed-traversable == 0.1.5
- indexed-traversable-instances == 0.1.2.1
- integer-conversion == 0.1.1
- integer-gmp == 1.1
- integer-logarithms == 1.0.5
- js-chart == 2.9.4.1
- math-functions == 0.3.4.4
- memory == 0.18.0
- microstache == 1.0.3.1
- mtl == 2.3.1
- mwc-random == 0.15.3.0
- network == 3.2.8.0
- network-uri == 2.6.4.2
- optparse-applicative == 0.19.0.0
- os-string == 2.0.10
- parallel == 3.3.0.0
- parsec == 3.1.18.0
- pretty == 1.1.3.6
- prettyprinter == 1.7.2
- prettyprinter-ansi-terminal == 1.1.4
- primitive == 0.9.1.0
- process == 1.6.29.0
- quickcheck-io == 0.2.0
- ram == 0.22.0
- random == 1.3.1
- rts == 1.0.2
- scientific == 0.3.8.1
- semialign == 1.4
- semigroupoids == 6.0.2
- splitmix == 0.1.3.2
- statistics == 0.16.5.0
- stm == 2.5.1.0
- strict == 0.5.1
- system-cxx-std-lib == 1.0
- tagged == 0.8.10
- tasty == 1.5.4
- template-haskell == 2.20.0.0
- text == 2.1.4
- text-iso8601 == 0.1.1.1
- text-short == 0.1.6.1
- th-abstraction == 0.7.2.0
- th-compat == 0.1.7
- these == 1.2.1
- time == 1.12.2
- time-compat == 1.9.9
- transformers == 0.6.1.0
- transformers-compat == 0.8
- unix == 2.8.8.0
- unordered-containers == 0.2.21
- uuid-types == 1.0.6.1
- vector == 0.13.2.0
- vector-algorithms == 0.9.1.0
- vector-binary-instances == 0.2.5.2
- vector-stream == 0.1.0.1
- vector-th-unbox == 0.2.2
- witherable == 0.5
<!-- AUTOMATED-DEPENDENCIES-END -->

## 7. Supplier Records

| Supplier | Component | Last Risk Assessment Date | Notes |
|----------|-----------|---------------------------|-------|
| Haskell Foundation | GHC RTS | 2025-10-15 | Core compiler |
| GitHub | Code Hosting | 2025-11-20 | CI/CD |

## 8. Archived Components

| Component | Version | Date Removed | Justification for Removal |
|-----------|---------|--------------|---------------------------|
| `uuid` / `uuid-types` | 1.0.6.1 | 2026-06-24 | Removed to reduce SOUP footprint and supply chain risk. Replaced with native token generation utility leveraging existing system entropy. |
