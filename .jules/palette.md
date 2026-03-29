## 2024-03-26 - Prevent frozen UIs in real-time apps
**Learning:** Real-time applications relying on WebSockets can silently fail, leaving users looking at stale data without realizing the connection has dropped. This is a critical usability issue, particularly in a medical/monitoring context.
**Action:** Always provide explicit, accessible connection state feedback (Connecting, Live, Disconnected) with appropriate ARIA attributes (`aria-live="polite"`) and visual indicators (like the `Offline` class) to ensure users are aware of the system's status.
## 2024-03-29 - [Actionable Canvas Empty States]
**Learning:** Pure status messages in canvas visualizations ("Disconnected") create a dead-end UX, whereas pairing them with actionable sub-text ("Check hardware connection") significantly reduces user frustration during complex stream interruptions.
**Action:** Always provide secondary actionable guidance alongside primary status alerts in empty states, especially for hardware/connection intensive views.
