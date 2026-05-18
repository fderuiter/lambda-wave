## 2024-05-18 - Canvas Interaction Discoverability
**Learning:** Adding `cursor: pointer` to interactive canvas elements is insufficient for discoverability. Without descriptive `title` attributes and clear visual hints directly on the canvas, users (especially those relying on tooltips) may not realize the element is clickable or how to interact with it.
**Action:** Always add dynamic `title` attributes to interactive canvases and render explicit visual instructions (e.g., 'Click or press Space to resume') during paused or empty states.
