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

## 2024-11-21 - [Responsive Fixed-Dimension Canvases & Graceful Degradation]
**Learning:** Fixed dimension canvases break layouts on mobile devices if they exceed screen width, and missing graceful degradation leaves users with JS disabled stranded on confusing initial states.
**Action:** Always apply `max-width: 100%; height: auto;` to fixed canvases for responsiveness, and include a `<noscript>` tag for SPAs to warn users instead of displaying broken "Loading..." interfaces.

## 2024-11-25 - [Dynamic Favicons for Background State Visibility]
**Learning:** While updating `document.title` helps users monitor real-time background tabs, titles get truncated when users open many tabs or pin them, completely hiding text-based status updates (like "BEAM OFF").
**Action:** Always combine title changes with a dynamic SVG favicon that updates its color (e.g., green, red, yellow) based on critical system states. Colors are instantly recognizable even when the tab is pinned and the title is fully hidden.

## 2024-11-26 - [Prevent Visual Jitter in Real-Time Metrics]
**Learning:** In real-time monitoring UIs updating at high frequencies (e.g., 30Hz), right-aligned or center-aligned strings containing numbers of varying character lengths cause the entire layout to rapidly shift horizontally (visual jitter). This makes reading values impossible and significantly increases cognitive load.
**Action:** Always extract static text units (like "ms") outside the dynamic container, and set the numeric container to `display: inline-block; text-align: right; font-variant-numeric: tabular-nums;` with a fixed `width` (e.g., `width: 5ch;`) to guarantee the layout remains perfectly stable regardless of value changes.

## 2024-11-28 - [Respect User Motion Preferences in Live Indicators]
**Learning:** Infinite CSS animations (like pulsing status dots) can trigger vestibular disorders or discomfort for users who have requested reduced motion at the OS level.
**Action:** Always provide a `@media (prefers-reduced-motion: reduce)` fallback to disable continuous decorative animations (e.g., setting `animation: none;`) to respect system-level accessibility preferences.

## 2024-11-28 - [Accessible Status Indication for Color-Blind Users]
**Learning:** Using only color changes (e.g., green/red/yellow) in a dynamic SVG favicon to indicate critical system states (like "Live" or "Disconnected") makes the states indistinguishable for color-blind users, especially when the tab is pinned and the title is hidden.
**Action:** Always combine color indicators with distinct shape changes (e.g., circle for active, square for disconnected, triangle for warning/hold) in dynamic favicons to ensure the state is visually distinct regardless of color perception.

## 2024-11-29 - [Smooth State Transitions for Status Indicators]
**Learning:** Real-time status indicators (like connection dots) that snap instantly between states (e.g., green to red) can feel jarring or glitchy, reducing the perceived quality of the interface.
**Action:** Always apply smooth CSS transitions (e.g., `transition: background-color 0.3s ease, box-shadow 0.3s ease;`) to interactive or dynamic state indicators to maintain a fluid, polished real-time UI.

## 2024-11-29 - [Discoverable Tooltip Hover States]
**Learning:** Even if `<abbr>` tags are keyboard accessible, sighted mouse users may not realize they are interactive tooltips if they lack a clear hover state, especially when the default styling only uses a subtle dotted underline.
**Action:** Always provide a clear visual hover state for tooltips and abbreviations (e.g., `abbr { transition: color 0.3s ease; } abbr:hover { color: [accent_color]; }`) so users can easily discover the interaction.
## 2024-04-14 - Smooth State Transitions
**Learning:** Status indicators that snap instantly between colors (e.g., live vs offline) can feel jarring and break the fluid feel of real-time monitoring.
**Action:** Always apply smooth CSS transitions (`transition: background-color 0.3s ease, box-shadow 0.3s ease;`) to interactive state dots.

## 2024-04-14 - Discoverable Tooltips
**Learning:** Abbreviations with tooltips aren't easily discoverable without a clear visual hover state.
**Action:** Add clear hover states (`color: [accent]`) with smooth color transitions to `abbr` tags to indicate interactivity.

## 2024-12-05 - [Semantic Headings for Self-Evident Monitors]
**Learning:** In visually self-evident web apps (like full-screen dashboards or monitoring tools), sighted users inherently understand the context from the visual layout alone. However, screen reader users rely on a semantic heading structure (starting with an `<h1>`) to understand the purpose of the page when they first land on it. Missing this top-level heading leaves them disoriented.
**Action:** Always include an `<h1>` element, even if it must be visually hidden using a `.sr-only` utility class, to provide essential context for assistive technology users.

## 2024-12-05 - [Visual Baseline for Real-Time Traces]
**Learning:** Real-time line graphs (like breathing traces) plotted without a center or zero-reference line make it difficult for users to cognitively anchor the data. They struggle to quickly assess whether values are positive/negative or increasing/decreasing relative to the baseline.
**Action:** Always render a subtle visual baseline or reference grid (e.g., a dashed center line) in real-time graphs to reduce cognitive load and improve data interpretability.

## 2024-12-06 - [Assertive Live Regions for Safety-Critical States]
**Learning:** For medical or safety-critical monitoring systems, setting `aria-live="polite"` on status updates (like "BEAM ON" or "BEAM OFF") is insufficient. Screen readers will wait for the current task to finish before reading it, causing potentially dangerous delays in conveying critical state changes to visually impaired users.
**Action:** Always use `role="alert"` and `aria-live="assertive"` for safety-critical state changes to guarantee that assistive technologies interrupt current announcements and immediately alert the user.

## 2024-12-06 - [Shape Indication for UI Status Dots]
**Learning:** Relying solely on colors (green, red, yellow) for small UI elements like connection status dots (`.status-dot`) makes these states indistinguishable for color-blind users directly within the interface (even if favicons handle background states).
**Action:** Always combine shape changes (e.g., border-radius: 50% for circle, border-radius: 0 for square, and transform rotations for diamond) alongside color changes on UI status dots to ensure accessibility for users with color vision deficiencies.
