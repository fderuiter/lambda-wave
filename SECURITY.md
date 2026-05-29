# Security Policy

## Supported Versions

The following versions of the project are currently supported with security updates.

| Version | Supported          |
| ------- | ------------------ |
| Main    | :white_check_mark: |
| v1.x    | :white_check_mark: |
| < v1.0  | :x:                |

## Reporting a Vulnerability

We take the security of this medical device software seriously. If you identify a potential flaw in the safety-critical logic, please report it to us privately to ensure patient safety and regulatory compliance.

### Private Reporting Interface

To report a vulnerability, please use **GitHub Security Advisories** to submit a private vulnerability report. This ensures that the details remain private during the remediation phase without alerting malicious actors.

1. Navigate to the [Security tab](../../security/advisories) in this repository.
2. Click on "Report a vulnerability".
3. Provide a detailed description of the flaw, including steps to reproduce.

Alternatively, you can email the maintainer directly at security@example.com (using PGP encryption if possible).

### Triage and Remediation Process

All vulnerability reports will be triaged following medical safety protocols and IEC 62304 Class C requirements.

1. **Initial Assessment:** The designated maintainer will acknowledge receipt of the report within 48 hours.
2. **Medical Safety Triage:** The report will be evaluated for its impact on safety-critical medical logic.
3. **Four-Eyes Review:** All remediation patches must undergo a strict "four-eyes" review (at least two independent reviewers) in accordance with IEC 62304 Class C compliance.
4. **Disclosure:** Vulnerability details will remain private during the remediation phase. Once a patch is available and deployed, the vulnerability will be disclosed via a structured GitHub Security Advisory to maintain transparency.

Thank you for helping us keep our safety-critical medical logic secure.
