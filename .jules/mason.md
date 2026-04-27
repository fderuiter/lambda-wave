## 2026-04-27 - [Exported Core Utilities from Numeric.Simple]
**Context:** To comply with Class C requirements and satisfy Hspec test scopes, `Numeric.Simple` needs its utility functions exported and tested.
**Decision:** Added `dot`, `at`, `isRectangular`, `updateAt`, and `gaussJordan` to the export list, documented them with Haddock complexity and safety guarantees, and added relevant assertions to `NumericCheck.hs`.
**Compliance Impact:** Satisfies the requirement for testable, total functions with documented safety constraints in a Class C module without introducing SOUP dependencies.
