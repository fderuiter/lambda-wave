## 2024-05-26 - Device-Aware Interaction Hints
**Learning:** Displaying keyboard-specific interaction hints (like "Press Space") on mobile/touch devices causes cognitive friction, as the user lacks the physical hardware to follow the instruction. It makes the UI feel broken or non-native to the device.
**Action:** Always use `@media (hover: none) and (pointer: coarse)` to visually hide `<kbd>` elements on touch devices, and use `window.matchMedia` in JavaScript to conditionally swap verbs (e.g., "Click" -> "Tap") in dynamic text/tooltips for a native feel.
