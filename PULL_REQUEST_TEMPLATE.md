## Related Issue
[Issue Link]

## Summary
[Brief description of changes]

## Verification Steps
[How to verify the fix/feature]

## Definition of Done Checklist
- [ ] Code Linting Passed
- [ ] Build Successful
- [ ] Unit/Property Tests Passed
- [ ] Safety/Compliance Review (if applicable)
- [ ] Hardware FFI checks: All new C calls are wrapped in `BridgeCall` / `bridgeHardwareCall`
- [ ] Hardware FFI checks: All results use `MustHandle` and are correctly eliminated
