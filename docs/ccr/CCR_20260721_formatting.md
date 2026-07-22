# Change Control Record

**CCR ID:** CCR-20260721-174000
**Version:** 8a120d04722e7f0cdd3bff94417c1e108788482c

## Problem Description
Code formatting differences and style drift in generated FFI files cause noisy git diffs, reducing the effectiveness of peer reviews for safety-critical logic in `src/Safety/`.

## Proposed Change
Implement strict CI formatting validation using `pre-commit` and `.clang-format`. Automate formatting of generated FFI files during the build process to ensure consistent styling.

## Impact on Hazards
Addresses H-SYS-016 by reducing review noise, thereby decreasing the likelihood of missing logical errors during code review.

## Quality Policy Origin
ISO 13485 Clause 7.3.3 Design and Development Outputs (consistency of code).

## Verification Strategy
Verified by CI pipeline `lint.yml` executing `pre-commit run --all-files` successfully and failing when code is improperly formatted.
