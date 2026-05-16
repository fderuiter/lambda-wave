## 2026-05-16 - Make canvas interactions discoverable
**Learning:** When providing interactive canvas elements (like a click-to-pause graph), the `cursor: pointer` CSS alone is insufficient for discoverability.
**Action:** Always add a descriptive `title` attribute (e.g., 'Click to pause trace') and explicitly render visual hints (e.g., 'Click or press Space to resume') on the canvas during paused/empty states to ensure users are aware of alternative interaction methods.
