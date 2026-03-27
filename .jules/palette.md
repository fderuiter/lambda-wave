## 2024-03-26 - Prevent frozen UIs in real-time apps
**Learning:** Real-time applications relying on WebSockets can silently fail, leaving users looking at stale data without realizing the connection has dropped. This is a critical usability issue, particularly in a medical/monitoring context.
**Action:** Always provide explicit, accessible connection state feedback (Connecting, Live, Disconnected) with appropriate ARIA attributes (`aria-live="polite"`) and visual indicators (like the `Offline` class) to ensure users are aware of the system's status.

## 2024-05-14 - Real-time Visualization Empty States
**Learning:** Real-time data canvases (like radar plots) that start as empty black boxes cause confusion about system status during initial loading, connection delays, or disconnects. Users cannot differentiate between "broken" and "waiting for data".
**Action:** Always implement explicit empty states directly on `<canvas>` elements using `fillText()` to provide clear guidance text (e.g., "Connecting...", "Waiting for data...") during all non-streaming states.
