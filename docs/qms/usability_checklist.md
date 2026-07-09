# Usability Verification Checklist

This artifact provides structured criteria for manual visual inspections to verify compliance with IEC 62366 and IEC 62304 usability and accessibility requirements.

## Traceability Tags
- SR-UI-002

## 1. Multi-Modal Encoding (SR-UI-001)
- [ ] **Color**: Safety-critical states (Beam On, Beam Off, Beam Hold) use distinct, standard colors (Green, Red, Yellow/Cyan).
- [ ] **Shape**: Safety-critical states are accompanied by distinct geometric shapes (e.g., Circle, Polygon, Triangle/Square).
- [ ] **Symbol**: Status text or symbolic icons explicitly state the system status redundantly alongside color and shape.

## 2. Structural Navigation (FR-UI-003)
- [ ] **Focus cycling**: Navigating through all buttons and input fields using only Tab (forward) and Shift+Tab / Arrow keys.
- [ ] **Visible focus highlight / native focus ring**: Ensuring a highly visible visual ring or bounding highlight is displayed around the active UI control.
- [ ] **Enter key form submission**: Pressing the Enter key while focusing on the password field initiates the authentication process.

## 3. Hazard Mitigation Verification
- [ ] The redundant visual signaling successfully mitigates the hazard of state misidentification for users with visual impairments (e.g., color vision deficiency) (H-USE-003).
