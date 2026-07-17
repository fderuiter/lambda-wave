## Related Issue
[Issue Link]

## Summary
[Brief description of changes]

## Verification Steps
[How to verify the fix/feature]

## Definition of Done Checklist
- [ ] Local pre-commit hooks passed successfully (`git commit` didn't block due to formatting or test failures)
- [ ] Code Linting Passed
- [ ] Build Successful
- [ ] Unit/Property Tests Passed
- [ ] Safety/Compliance Review (if applicable)
- [ ] Hardware FFI checks: All new C calls are wrapped in `BridgeCall` / `bridgeHardwareCall`
- [ ] Hardware FFI checks: All results use `MustHandle` and are correctly eliminated
