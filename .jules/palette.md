## 2026-01-28 - Accessible Tooltips for Keyboard Focus on Disabled Elements
**Learning:** While using `aria-disabled="true"` allows a disabled button to remain in the tab order and retain its `title` attribute, browsers only natively display the `title` tooltip on mouse hover. Keyboard users navigating via Tab receive focus but no visual explanation of why the control is disabled.
**Action:** Always combine `aria-disabled="true"` with a CSS `::after` pseudo-element triggered by `:focus-visible` (e.g., `[aria-disabled="true"][title]:focus-visible::after { content: attr(title); }`) to explicitly expose the tooltip text to sighted keyboard users.
## 2026-01-28 - Focus Visible Tooltips for Aria-Disabled Controls
**Learning:** Browsers natively display `title` tooltips on mouse hover, but for keyboard users navigating via Tab, a `title` on an `aria-disabled="true"` element isn't exposed visually, leaving them confused as to why the control is unavailable.
**Action:** Always combine `aria-disabled="true"` with a CSS `::after` pseudo-element triggered by `:focus-visible` (e.g., `[aria-disabled="true"][title]:focus-visible::after { content: attr(title); }`) to ensure the reason for disablement is accessible.
## 2026-01-29 - Avoid aria-pressed on dynamic text toggles
**Learning:** Using `aria-pressed` on toggle buttons that dynamically change their visible text label (e.g., "Pause" to "Resume") causes confusing screen reader announcements (e.g., "Resume toggle button pressed").
**Action:** Do not use `aria-pressed` when the button's visible text already clearly indicates its state. Use it only for buttons with persistent visual labels.
