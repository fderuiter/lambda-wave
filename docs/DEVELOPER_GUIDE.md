# Developer's Guide 📘

**Last Updated:** January 28, 2026  
**For:** Contributors to Lambda-Wave codebase

This guide provides developers with essential information about code structure, development workflow, and best practices for contributing to Lambda-Wave.

---

## Table of Contents

1. [Quick Reference](#quick-reference)
2. [Codebase Structure](#codebase-structure)
3. [Development Workflow](#development-workflow)
4. [User Interface Development](#user-interface-development)
5. [Coding Standards](#coding-standards)
6. [Testing Strategy](#testing-strategy)
7. [Performance Guidelines](#performance-guidelines)
8. [Safety-Critical Code](#safety-critical-code)
9. [Debugging Tips](#debugging-tips)
10. [Common Tasks](#common-tasks)

---

## Quick Reference

### Essential Commands

```bash
# Build (Core Only)
cabal build

# Build with OpenGL UI
cabal build --flags=enable-ui

# Build with Web Dashboard
cabal build --flags=enable-web-ui

# Test
cabal test

# Run
cabal run sgrt-radar-system-exe

# Lint
hlint src/ app/ test/

# Format (future)
ormolu --mode inplace $(find src app test -name '*.hs')

# Benchmarks
cabal bench

# Documentation
cabal haddock
```

### Key Files to Know

- **`app/Main.hs`** - Application entry point, thread orchestration
- **`src/Control/Gating.hs`** - Beam gating logic (safety-critical)
- **`src/SignalProcessing/FMCW.hs`** - Radar signal processing
- **`src/Safety/Watchdog.hs`** - Watchdog timer (safety-critical)
- **`cbits/src/ring_buffer.cpp`** - C++ ring buffer for data ingestion
- **`app/Control/UI/`** - OpenGL Visualization
- **`app/Control/WebUI/`** - Web Dashboard Server

---

## 📂 Codebase Structure

### `/src` - Core Logic (Haskell)

#### `Control/` - Control Plane
*   **`Gating.hs`**: **⚠️ SAFETY-CRITICAL**. Beam ON/OFF decision logic. Four-eyes review required.
*   **`Mesher.hs`**: Polynomial surface fitting for virtual mesh generation.

#### `Data/` - Data Types
*   **`Types.hs`**: Core data structures (`RadarFrame`, `Point3D`, `SystemState`)
*   **`Config.hs`**: Configuration parsing and management

#### `FFI/` - Foreign Function Interface
*   **`RingBuffer/`**: Haskell bindings to C++ ring buffer
    *   `Types.hs`: FFI type definitions
    *   `IO.hs`: **Dragon 🐉**. Handles raw UART data ingestion using Pinned Memory

#### `Hardware/` - Hardware Interaction Layer
*   **`Consumer.hs`**: TLV packet parser, converts raw bytes into `RadarFrame` structures
*   **`Control.hs`**: Sensor configuration via serial port

#### `Safety/` - Safety-Critical Monitoring
*   **`Watchdog.hs`**: **⚠️ SAFETY-CRITICAL**. Dead Man's Switch, kills application on timeout
*   **`Audit.hs`**: Immutable event logging for compliance

#### `SignalProcessing/` - Data Plane
*   **`FMCW.hs`**: FMCW radar processing (background subtraction, phase unwrapping)
*   **`Regression.hs`**: Kalman filtering and polynomial regression

### `/cbits` - Foreign Function Interface (C++)
*   **`include/`**: Header files
    *   `ring_buffer.h`: Ring buffer API
    *   `RingBuffer.h`: C++ class definition
*   **`src/`**: Implementation
    *   `ring_buffer.cpp`: **Dragon 🐉**. Circular buffer with atomic operations for Zero-Copy ingestion
    *   `RingBufferCheck.cpp`: Memory safety checks

### `/app` - Application Entry Point
*   **`Main.hs`**: Wires together Ingestion, Parser, Gating, Watchdog, and UI threads using `forkOS` and `STM`
*   **`Control/UI/`**: OpenGL Visualization (Moved from `src` for isolation)
*   **`Control/WebUI/`**: Web Dashboard Server (Isolated)

### `/test` - Test Suites
*   **`FFI/RingBuffer/`**: FFI layer tests
*   **`Hardware/`**: Hardware interaction tests
*   **`SignalProcessing/`**: DSP algorithm tests
*   **`System/`**: Runtime system tests
*   **`Spec.hs`**: Test runner (main entry point)

---

## 🔄 Development Workflow

### 1. Setup Development Environment

See [BUILD_GUIDE.md](BUILD_GUIDE.md) for detailed setup instructions.

```bash
# Clone repository
git clone https://github.com/fderuiter/lambda-wave.git
cd lambda-wave

# Install dependencies
cabal update
cabal build --only-dependencies

# Verify setup
cabal test
```

### 2. Choose a Task

Check [TODO.md](../TODO.md) for prioritized tasks. Look for:
- **P0** items if you want to work on critical path
- **P1** items for core functionality
- **Good first issues** on GitHub for beginners

### 3. Create a Branch

Follow Git-flow naming convention:

```bash
# For new features
git checkout -b feature/issue-123-add-feature

# For bug fixes
git checkout -b bugfix/issue-456-fix-bug

# Always branch from develop
git checkout develop
git pull origin develop
git checkout -b feature/your-feature
```

---

## 🎨 User Interface Development

The UI components are intentionally isolated in `app/Control` to separate them from the safety-critical library in `src/`. This separation ensures that complex dependencies (OpenGL, WebSockets) do not pollute the certified core.

### OpenGL UI (Local)

*   **Location:** `app/Control/UI/`
*   **Flag:** `enable-ui` (Default: False)
*   **Technology:** `OpenGL` (Immediate Mode) + `GLUT`
*   **Architecture:**
    *   Runs on the **Main Thread** (GLUT requirement).
    *   Polls `SystemState` TVar in `display` callback.
    *   Visualization: Points (Cyan), Target (Red Crosshair), Beam Status (Color Indicator).
*   **Verification:** Visual inspection via `cabal run --flags=enable-ui`.

### Web Dashboard (Remote)

*   **Location:** `app/Control/WebUI/`
*   **Flag:** `enable-web-ui` (Default: False)
*   **Technology:** `Warp` (HTTP) + `WebSockets` + HTML5 Canvas
*   **Architecture:**
    *   Runs in a background thread via `forkOS`.
    *   Streams JSON updates at 30Hz via WebSocket.
    *   Frontend assets embedded into binary via `Data.FileEmbed`.
*   **Verification:**
    *   **Unit:** `test/WebUI/ServerSpec.hs` checks WebSocket JSON validity.
    *   **E2E:** `test/WebUI/verify_frontend.py` checks HTML structure using Playwright.

---

## 📝 Coding Standards

### Haskell Style

#### General Principles
- Use **strict data** by default (`{-# LANGUAGE StrictData #-}`)
- Prefer **pure functions** over IO when possible
- Use **type signatures** for all top-level functions
- Keep functions **small and focused** (< 20 lines ideal)
- Use **meaningful names** (avoid abbreviations unless standard)

#### Naming Conventions
```haskell
-- Types: PascalCase
data SystemState = SystemState { ... }

-- Functions: camelCase
calculateGatingDecision :: SystemState -> BeamState

-- Constants: camelCase
defaultTimeout :: Int
```

---

## 🧪 Testing Strategy

### Test Organization

```
test/
├── Spec.hs                    # Main test runner
├── FFI/                       # FFI layer tests
├── Hardware/                  # Hardware tests
├── SignalProcessing/          # DSP tests
├── System/                    # Integration tests
└── WebUI/                     # Web Dashboard verification
```

### UI Testing

Testing UI components requires a different strategy than pure logic:

1.  **Isolation:** UI logic (e.g., coordinate transformation) should be separated into pure functions where possible and unit tested.
2.  **Visual Verification:** Use `cabal run --flags=enable-ui` to manually verify rendering.
3.  **Automated Checks:** Use `Playwright` scripts (like `test/WebUI/verify_frontend.py`) to verify web assets and structure.

---

## ⚡ Performance Guidelines

### Critical Performance Paths

1. **Data Ingestion** (< 1ms per frame target)
2. **FMCW Processing** (< 5ms per frame target)
3. **Gating Decision** (< 0.1ms target)
4. **Total Pipeline** (< 50ms end-to-end)

### Optimization Techniques

#### Avoid Allocations in Hot Paths
```haskell
-- Bad: Allocates list every call
processPoints :: [Point3D] -> Result
processPoints ps = map expensive ps

-- Good: Use vector for bulk operations
processPoints :: Vector Point3D -> Result
processPoints ps = V.map expensive ps
```

---

## ⚠️ Safety-Critical Code

### Identification

Files marked with **⚠️ SAFETY-CRITICAL** require special attention:
- `src/Control/Gating.hs`
- `src/Safety/Watchdog.hs`
- `src/Safety/Audit.hs`

### Review Requirements

1. **Four-Eyes Principle**: Minimum 2 reviewers
2. **Test Coverage**: ≥90% branch coverage
3. **Property Testing**: QuickCheck properties required
4. **Documentation**: Detailed comments explaining safety logic
5. **Traceability**: Link to requirements (FR-*, SR-*)

---

## 🔧 Common Tasks

### Adding a New Module

1. Create file in appropriate directory
2. Add to `exposed-modules` in `.cabal` file
3. Write module documentation
4. Add corresponding test file
5. Update DEVELOPER_GUIDE.md (this file)

### Adding a New Dependency

1. Add to `build-depends` in `.cabal` file
2. Run `cabal build --only-dependencies`
3. Update BUILD_GUIDE.md with any system requirements
4. Document usage in relevant module

### Updating Documentation

1. Make changes to markdown files
2. Update `docs/README.md` index if structure changes
3. Run markdown lint (if available)
4. Commit with docs: prefix

---

**Maintained by:** Development Team  
**Last Review:** January 28, 2026
