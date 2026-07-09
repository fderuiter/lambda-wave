# IEC 62304 Traceability Matrix

**Note:** For reporting security vulnerabilities, refer to our [Security Policy](../../SECURITY.md).

This document provides end-to-end traceability between Functional Requirements (FR), Safety Requirements (SR), Design Elements (Modules), and Verification (Tests) as required by IEC 62304 for Class C medical software.

## Functional Requirements (FR)

| Req ID | Quality Policy Origin | Description | Source Phase | Module | Architecture Section | Verification Test | Status |
|---|---|---|---|---|---|---|---|
| FR-DAQ-001 | ISO-13485-7.3.2 | Zero-copy data ingestion | Phase 2.1 | `FFI.RingBuffer.IO`, `cbits/src/ring_buffer.cpp` | [`src/FFI/RingBuffer/IO.hs`](../../Haskell Radar SGRT System Development.md) | `test/FFI/RingBuffer/IOSpec.hs` | ✅ Complete |
| FR-DAQ-002 | ISO-13485-7.3.2 | Sensor Configuration | Phase 2.3 | `Hardware.Control`, `Data.Config` | N/A | `test/Hardware/ControlSpec.hs` | ✅ Complete |
| FR-DAQ-003 | ISO-13485-7.3.2 | Packet parser validation | Phase 2.2 | `Hardware.Consumer` | N/A | `test/Hardware/ConsumerSpec.hs`, `ConsumerIntegrationCheck.hs` | ✅ Complete |
| FR-DAQ-004 | ISO-13485-7.3.2 | Atomic ring buffer management | Phase 2.1 | `FFI.RingBuffer.IO`, `cbits/src/ring_buffer.cpp` | [`src/FFI/RingBuffer/IO.hs`](../../Haskell Radar SGRT System Development.md) | `test/FFI/RingBuffer/IOSpec.hs` | ✅ Complete |
| FR-DSP-001 | ISO-13485-7.3.2 | Static Clutter Removal | Phase 3.1 | `SignalProcessing.FMCW` | N/A | `test/SignalProcessing/FMCWSpec.hs` | ✅ Complete |
| FR-DSP-002 | ISO-13485-7.3.2 | Phase Unwrapping | Phase 3.2 | `SignalProcessing.FMCW` | N/A | `test/SignalProcessing/FMCWSpec.hs` | ✅ Complete |
| FR-DSP-003 | ISO-13485-7.3.2 | Kalman filter for motion prediction | Phase 3.3 | `SignalProcessing.Kalman` | N/A | `test/SignalProcessing/KalmanCheck.hs`, `test/SignalProcessing/PhantomStudy.hs` | ✅ Complete |
| FR-DSP-004 | ISO-13485-7.3.2 | Phase Extraction | Phase 3.2 | `SignalProcessing.FMCW` | N/A | `test/SignalProcessing/FMCWSpec.hs` | ✅ Complete |
| FR-GAT-001 | ISO-13485-7.3.2 | Automatic beam gating | Phase 4.2 | `Control.Gating` | N/A | `test/Control/GatingCheck.hs` | ✅ Complete |
| FR-GAT-002 | ISO-13485-7.3.2 | Total latency < 50ms | Phase 4.2 | `Control.Gating`, `Main` | N/A | `bench/LatencyBench.hs` | ✅ Complete |
| FR-UI-001 | ISO-13485-7.3.2 | Real-time visualization | Phase 5.1 | `cbits/src/hud.cpp`, `cbits/include/hud.h` | N/A | `test/Control/UIMathSpec.hs` | ✅ Complete |
| FR-UI-002 | ISO-13485-7.3.2 | Visual gating feedback | Phase 5.2 | `cbits/src/hud.cpp`, `cbits/include/hud.h` | N/A | Visual Inspection | ✅ Complete |
| FR-UI-003 | IEC-62366-5.1 | Structural navigation, assistive technology hooks, and skip-links | Phase 5.3 | `app/Control/WebUI.hs` | N/A | Visual Inspection | ✅ Complete |

## Safety Requirements (SR)

| Req ID | Quality Policy Origin | Description | Source Phase | Module | Architecture Section | Verification Test | Status |
|---|---|---|---|---|---|---|---|
| SR-UI-001 | IEC-62366-5.1, IEC-62304-5.1.1 | Multi-modal encoding (color, shape, and symbol) for safety-critical states | Phase 5.3 | `app/Control/UI/Renderer.hs`, `app/Control/WebUI.hs` | N/A | Visual Inspection | ✅ Complete |
| SR-UI-002 | IEC-62366-5.1 | Formal usability verification checklist for visual inspections | Phase 5.3 | `docs/qms/usability_checklist.md` | N/A | Visual Inspection | ✅ Complete |
| SR-SOUP-001 | ISO-13485-7.1 | GHC RTS deterministic runtime (locked capabilities) | Phase 1.1 | `app/Main.hs`, `.cabal` | N/A | `test/System/RTSSpec.hs` | ✅ Complete |
| SR-WD-001 | ISO-13485-7.1 | Watchdog monitors all critical threads | Phase 4.1 | `Safety.Watchdog` | N/A | `test/Safety/WatchdogSpec.hs` | ✅ Complete |
| SR-WD-002 | ISO-13485-7.1 | Application termination on timeout (100ms) | Phase 4.1 | `Safety.Watchdog` | N/A | `test/WatchdogCheck.hs` | ✅ Complete |
| SR-AUDIT-001 | ISO-13485-7.1 | Immutable event log | Phase 4.3 | `Safety.Audit` | N/A | `test/Safety/AuditCheck.hs` | ✅ Complete |
| SR-IPC-001 | ISO-13485-7.1 | Process boundary isolation; all IPC heartbeats are monitored by the safety daemon | Phase 4.4 | `Safety.Watchdog`, `app/Main.hs`, `app/VisualizerMain.hs` | N/A | Visual Inspection | ✅ Complete |

## Performance Requirements (PR)

| Req ID | Quality Policy Origin | Description | Source Phase | Module | Architecture Section | Verification Test | Status |
|---|---|---|---|---|---|---|---|
| PR-ACC-01 | ISO-13485-7.3.6 | Correlation coefficient > 0.95 vs ground truth | Phase 6.1 | `SignalProcessing.Kalman`, `SignalProcessing.FMCW` | N/A | `test/SignalProcessing/PhantomStudy.hs` | ❌ Incomplete |

## Mathematical Requirements (MR)

| Req ID | Quality Policy Origin | Description | Source Phase | Module | Architecture Section | Verification Test | Status |
|---|---|---|---|---|---|---|---|
| MR-001 | ISO-13485-7.3.2 | FMCW Range Estimation (Equation 1) | Phase 3.1 | `SignalProcessing.FMCW` | [`src/Numeric/Kinematics.hs`](../../Haskell Radar SGRT System Development.md) | `test/SignalProcessing/FMCWSpec.hs` | ✅ Complete |
| MR-002 | ISO-13485-7.3.2 | Chirp Z-Transform (Equation 2) | Phase 3.1 | `SignalProcessing.FMCW` | N/A | `test/SignalProcessing/FMCWSpec.hs` | ✅ Complete |
| MR-003 | ISO-13485-7.3.2 | Standard DFT (Equation 3) | Phase 3.1 | None | N/A | None | ❌ Incomplete |
| MR-004 | ISO-13485-7.3.2 | Phase Extraction (Equation 4) | Phase 3.2 | `SignalProcessing.FMCW` | N/A | `test/SignalProcessing/FMCWSpec.hs` | ✅ Complete |
| MR-005 | ISO-13485-7.3.2 | Displacement Calculation (Equation 5) | Phase 3.2 | `SignalProcessing.FMCW` | [`src/Numeric/Kinematics.hs`](../../Haskell Radar SGRT System Development.md) | `test/SignalProcessing/FMCWSpec.hs` | ✅ Complete |
