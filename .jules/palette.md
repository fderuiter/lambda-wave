## 2024-05-18 - Canvas Interaction Discoverability
**Learning:** Adding `cursor: pointer` to interactive canvas elements is insufficient for discoverability. Without descriptive `title` attributes and clear visual hints directly on the canvas, users (especially those relying on tooltips) may not realize the element is clickable or how to interact with it.
**Action:** Always add dynamic `title` attributes to interactive canvases and render explicit visual instructions (e.g., 'Click or press Space to resume') during paused or empty states.

## 2024-05-19 - Custom Interactive Elements & Keyboard Operability
**Learning:** When making non-button elements (like `<canvas>`) visually interactive and focusable (`tabindex="0"`), relying on global Spacebar shortcuts is insufficient. Screen reader and keyboard users expect the 'Enter' key to activate the currently focused interactive element. Without handling Enter, the element fails standard keyboard operability expectations.
**Action:** Always bind both 'Enter' and 'Space' when creating custom interactive elements to ensure complete, predictable keyboard accessibility matching native `<button>` behavior.
