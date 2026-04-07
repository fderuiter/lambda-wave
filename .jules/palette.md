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

## 2024-05-18 - [Critical System State Visibility in Background Tabs]
**Learning:** Users in a clinical/monitoring environment often have multiple tabs or applications open. If critical status changes (like "BEAM OFF" or "Disconnected") only occur visually within the webpage content, they are missed when the tab is in the background.
**Action:** Always reflect critical system state changes in the `document.title` so the user can monitor the system's status directly from the browser tab bar without needing the window to be actively focused.

## 2024-06-15 - [Screen Reader Accessibility for Canvas Rendered Text]
**Learning:** Text rendered visually on `<canvas>` elements using `fillText` is completely invisible to screen readers, creating a major accessibility gap for users relying on assistive technologies to understand state changes or guidance displayed on the canvas.
**Action:** Always mirror critical text drawn on `<canvas>` elements to the canvas's `aria-label` attribute dynamically to ensure screen reader users receive the same information as sighted users.
## 2024-10-24 - Screen Reader Spam from High-Frequency WebSocket Updates
**Learning:** Updating `aria-live` elements (or `document.title`) on every frame from a 30Hz WebSocket stream causes screen readers to constantly announce the status, completely overwhelming the user and rendering the interface unusable.
**Action:** Always diff against previous states (`lastState !== currentState`) before applying DOM updates to `aria-live` elements or the document title in high-frequency event loops.

## 2024-10-24 - [Semantic Landmarks and Clean Live Regions]
**Learning:** Decorative elements inside `aria-live` regions (like animated status dots) can cause confusing noise for screen reader users, and missing semantic landmarks (like `<main>`) force users to navigate through all DOM nodes sequentially.
**Action:** Wrap the core application content in a `<main>` tag to establish clear document structure, add `aria-hidden="true"` to decorative elements within status announcements, and use `aria-atomic="true"` on live regions to ensure cohesive readings.

## 2024-11-20 - [Silencing Decorative Separators]
**Learning:** Text characters used strictly for visual layout or separation (like `|` or `/`) are read aloud by screen readers ("vertical line", "slash"), causing unnecessary cognitive noise and slowing down navigation for visually impaired users.
**Action:** Always wrap non-semantic, purely visual separator text in a `<span aria-hidden="true">` to hide it from assistive technologies while preserving its visual appearance.

## 2024-11-20 - [Synchronizing Contrast Transitions]
**Learning:** When animating a component's state (like an alert or status pill) where both the background and text color change, animating only one property (e.g., `background-color`) causes the other to snap instantly. This creates a brief moment of harsh or unreadable contrast mid-transition.
**Action:** Always synchronize transitions for both `background-color` and `color` (`transition: background-color 0.3s ease, color 0.3s ease;`) to maintain readable contrast throughout the animation lifecycle.

## 2024-11-21 - [Prevent False Security in Initial States]
**Learning:** For real-time UI components (like WebSocket status monitors), hardcoding an initial "safe" state (e.g., "BEAM OFF") in the static HTML before the connection is established creates a false sense of security. If the connection fails to establish, the user might assume the system is safe when its actual state is unknown.
**Action:** Always default to a neutral, informative state like "CONNECTING..." (with an offline or pending visual indicator) until actual telemetry is confirmed from the backend.
