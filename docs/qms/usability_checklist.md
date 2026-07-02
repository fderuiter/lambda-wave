# Usability Verification Checklist

This artifact provides structured criteria for manual visual inspections to verify compliance with IEC 62366 and IEC 62304 usability and accessibility requirements.

## Traceability Tags
- SR-UI-002

## 1. Multi-Modal Encoding (SR-UI-001)
- [ ] **Color**: Safety-critical states (Beam On, Beam Off, Beam Hold) use distinct, standard colors (Green, Red, Yellow/Cyan).
- [ ] **Shape**: Safety-critical states are accompanied by distinct geometric shapes (e.g., Circle, Polygon, Triangle/Square).
- [ ] **Symbol**: Status text or symbolic icons explicitly state the system status redundantly alongside color and shape.

## 2. Structural Navigation (FR-UI-003)
- [ ] **Skip-links**: A skip-link ("Skip to Main Content") is present and functions correctly to bypass repetitive navigation elements.
- [ ] **Assistive Hooks**: ARIA labels, roles (`role="alert"`, `role="img"`), and live regions (`aria-live`) are implemented on critical UI components (e.g., Beam Status, Canvas, Announcements).
- [ ] **Keyboard Nav**: Focus styles are visible, and all interactive elements (e.g., Pause button) are accessible via keyboard navigation.

## 3. Hazard Mitigation Verification
- [ ] The redundant visual signaling successfully mitigates the hazard of state misidentification for users with visual impairments (e.g., color vision deficiency) (H-USE-003).
