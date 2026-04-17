## 2026-04-17 - [List Comprehension Stream Fusion]
**Learning:** In GHC Haskell, replacing simple list comprehensions without guards with explicit `map` calls does not improve performance. GHC already desugars them directly into `map` and applies identical stream fusion optimization rules.
**Action:** Avoid replacing list comprehensions with `map` for performance reasons. Focus on larger algorithmic optimizations or avoiding partial functions in hot paths instead.
