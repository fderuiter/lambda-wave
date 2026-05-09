## 2024-04-24 - Throttling High-Frequency Telemetry Text
**Learning:** Displaying raw textual metrics at high frequency (e.g. 30Hz) causes rapid flickering, making it impossible for users to read and increasing cognitive load without providing actionable information. Large numbers without formatting also take longer to mentally parse.
**Action:** Always decouple textual UI updates from underlying high-frequency event loops by throttling them (e.g. to ~4Hz). Additionally, use `toLocaleString()` or similar formatting for large numbers to improve quick readability.
## 2024-05-15 - Context Labels on Real-time Graphs
**Learning:** Real-time data visualization lines (like `<canvas>` traces) lack magnitude context by themselves, forcing users to guess the scale.
**Action:** Always add visual axis labels (e.g. `+100mm`, `-100mm`, `0mm`) and baselines directly to the graph rendering to reduce cognitive load and allow instant interpretation of data scale.
## 2024-05-16 - Canvas Text Accessibility
**Learning:** Text rendered visually on `<canvas>` elements using `fillText` is invisible to screen readers, meaning critical context (like graph scales or empty state messages) is lost to visually impaired users.
**Action:** Dynamically mirror any text drawn on the canvas to the element's `aria-label` attribute, and ensure the canvas has `tabindex="0"` with clear `:focus-visible` styling so it is discoverable via keyboard navigation.

## 2026-05-01 - Keyboard Accessibility for Abbr Tooltips
**Learning:** Native `title` attributes on `<abbr>` elements are visually displayed as tooltips on mouse hover, but they are not accessible to keyboard-only users who navigate via Tab, leading to missing context.
**Action:** Always add a CSS rule (e.g., `abbr:focus-visible::after`) to visually expose the `title` attribute as a tooltip when the `abbr` element receives keyboard focus, ensuring feature parity for keyboard users.
## 2024-05-20 - Pausing Real-time Interfaces
**Learning:** Users with cognitive or vestibular conditions can struggle with rapidly updating real-time animations (like a 30Hz trace), violating WCAG 2.2.2 (Pause, Stop, Hide).
**Action:** Always provide an accessible mechanism (like a keyboard-navigable pause button) to freeze live animations while maintaining data context in the background.

## 2026-05-07 - Dynamic ARIA Labeling for Canvas States
**Learning:** While static `aria-label`s on canvases provide initial context, dynamic state changes rendered via `fillText` (like "PAUSED") remain inaccessible to screen readers unless the `aria-label` is synchronously updated.
**Action:** When a `<canvas>` element's visual state changes dynamically (e.g., displaying a "PAUSED" overlay), mirror the new context to the `aria-label` attribute and restore the original label when the state reverts.
## 2024-05-24 - Accessible Keyboard Shortcut Hints
**Learning:** Adding a `<kbd>` element for visual hints is great for sighted users, but screen readers may announce the key names out of context or redundantly if the element is nested within a button label. The `aria-keyshortcuts` attribute should be added to the button itself for semantic accessibility.
**Action:** When adding keyboard shortcut hints, add `aria-keyshortcuts` to the interactive element, and use `aria-hidden="true"` on the visual `<kbd>` hint nested within it to prevent redundant screen reader announcements while providing semantic shortcut info.
