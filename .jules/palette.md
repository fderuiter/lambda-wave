## 2024-05-15 - Disable UI controls when disconnected
**Learning:** Users can get confused if interactive controls (like a Pause button) remain enabled when the application is in a disconnected or connecting state, leading them to think the action was accepted when it wasn't.
**Action:** Always visually and functionally disable interactive controls when the underlying connection or service is unavailable.

## 2024-05-15 - Use aria-disabled instead of native disabled for tooltips
**Learning:** To provide accessible tooltips on disabled interactive elements (like buttons), do not use the native `disabled` attribute, as it drops mouse events and prevents keyboard focus in several browsers.
**Action:** Instead, use `aria-disabled="true"`, style it with `[aria-disabled="true"]`, and manually ignore click events in JavaScript to ensure the `title` attribute remains accessible.
