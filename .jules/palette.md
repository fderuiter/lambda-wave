## 2024-03-26 - Prevent frozen UIs in real-time apps
**Learning:** Real-time applications relying on WebSockets can silently fail, leaving users looking at stale data without realizing the connection has dropped. This is a critical usability issue, particularly in a medical/monitoring context.
**Action:** Always provide explicit, accessible connection state feedback (Connecting, Live, Disconnected) with appropriate ARIA attributes (`aria-live="polite"`) and visual indicators (like the `Offline` class) to ensure users are aware of the system's status.
## 2024-03-29 - [Actionable Canvas Empty States]
**Learning:** Pure status messages in canvas visualizations ("Disconnected") create a dead-end UX, whereas pairing them with actionable sub-text ("Check hardware connection") significantly reduces user frustration during complex stream interruptions.
**Action:** Always provide secondary actionable guidance alongside primary status alerts in empty states, especially for hardware/connection intensive views.

## 2024-03-29 - [Human-Readable Metrics in Monitoring UIs]
**Learning:** Exposing raw machine data (like nanosecond timestamps) in real-time monitoring interfaces increases cognitive load and hides important performance context (like actual frame processing time/latency). Users don't care about the absolute timestamp, they care about the *interval* or *latency*.
**Action:** Always compute and display derived, human-readable metrics (like "33.3 ms" frame times) instead of raw system counters.

## 2024-03-31 - [Keyboard Accessible Tooltips]
**Learning:** `<abbr>` elements with `title` attributes provide helpful tooltips for mouse users but are completely invisible to keyboard users by default, breaking accessibility for technical abbreviations.
**Action:** Always add `tabindex="0"` to `<abbr>` tags or custom tooltips to ensure keyboard-only users can discover and access the definitions.
