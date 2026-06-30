# SOUP Analysis (Bolt Optimizer)

**Software of Unknown Provenance (SOUP) Evaluation for IEC 62304**

**SOUP Item:** Bolt Optimizer Tool
**Version:** 1.0.0
**Purpose:** Automates AST-aware transformations and optimizations on Haskell code for safety-critical signal processing.

## 1. Description and Purpose
The Bolt Optimizer parses Haskell source code, verifies module names, and applies AST-aware pattern matching to rewrite constructs such as `sum . zipWith (*)` into highly optimized `dot` functions. This avoids manual, string-based matching risks.

## 2. Intended Use Context
Lambda-Wave is an IEC 62304 Class C Medical Software system. The Bolt Optimizer is used within the GitHub Actions continuous integration pipeline as a mandatory build step.

## 3. Potential Hazards & Risk Control Measures
- **Hazard:** Silent defects introduced by incorrect code modification.
- **Control Measure:** The tool now uses AST-aware tokenization and explicitly validates module names to ensure non-numeric modules (e.g., `Data.I18n`) are never affected. Verified via differential testing in CI.

## 4. Anomaly List Assessment
No open anomalies exist for the Bolt Optimizer. It has been reviewed and tested against property-based tests for Kalman and FMCW modules to prove functional equivalence.

## 5. Justification for Acceptability
The tool is acceptable for use in the Lambda-Wave pipeline due to its strict guardrails restricting modifications to explicitly allowed numeric modules and automated CI verification guaranteeing correctness.
