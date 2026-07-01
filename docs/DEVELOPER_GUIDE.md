# Developer's Guide 📘

**Last Updated:** January 28, 2026  
**For:** Contributors to Lambda-Wave codebase

This guide provides developers with essential information about code structure, development workflow, and best practices for contributing to Lambda-Wave.

---

## Table of Contents

1. [Quick Reference](#quick-reference)
2. [Codebase Structure](#codebase-structure)
3. [Development Workflow](#development-workflow)
4. [Coding Standards](#coding-standards)
5. [Testing Strategy](#testing-strategy)
6. [Performance Guidelines](#performance-guidelines)
7. [Safety-Critical Code](#safety-critical-code)
8. [Debugging Tips](#debugging-tips)
9. [Common Tasks](#common-tasks)
10. [Hardware Integration & FFI Safety Framework](#hardware-integration--ffi-safety-framework)

---

## Quick Reference

### Essential Commands

```bash
# Build
cabal build

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

---

## 📂 Codebase Structure

### `/src` - Core Logic (Haskell)

#### `Control/` - Control Plane
*   **`Gating.hs`**: **⚠️ SAFETY-CRITICAL**. Beam ON/OFF decision logic. Four-eyes review required.
*   **`Mesher.hs`**: Polynomial surface fitting for virtual mesh generation.
*   **`UI/`**: User interface components (OpenGL rendering)
    *   `Window.hs`: Window management
    *   `Renderer.hs`: 3D mesh visualization
    *   `Input.hs`: User input handling

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

### `/test` - Test Suites
*   **`FFI/RingBuffer/`**: FFI layer tests
*   **`Hardware/`**: Hardware interaction tests
*   **`SignalProcessing/`**: DSP algorithm tests
*   **`System/`**: Runtime system tests
*   **`Spec.hs`**: Test runner (main entry point)

### `/bench` - Performance Benchmarks
*   **`LatencyBench.hs`**: End-to-end latency measurements

### `/config` - Configuration Files
*   **`ti_iwr6843isk/sgrt_profile.cfg`**: Sensor chirp configuration

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

### 4. Make Changes

- Write code following [Coding Standards](#coding-standards)
- Add tests for new functionality
- Update documentation if needed
- Run linter and tests frequently

### 5. Test Your Changes

```bash
# Run tests
cabal test

# Run specific test
cabal test --test-options="-m YourModule"

# Run benchmarks if performance-critical
cabal bench

# Check with hlint
hlint src/ app/ test/
```

### 6. Commit and Push

```bash
# Stage changes
git add .

# Commit with descriptive message
git commit -m "feat: Add Kalman filter implementation

- Implement state vector and prediction step
- Add measurement update with Kalman gain
- Include unit tests with synthetic data
- Refs #123"

# Push to GitHub
git push origin feature/your-feature
```

### 7. Open Pull Request

1. Go to GitHub repository
2. Click "New Pull Request"
3. Fill out PR template completely
4. Request review from at least 2 people (4 eyes for safety-critical code)
5. Address review comments
6. Wait for CI to pass

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

-- Type variables: lowercase single letters
map :: (a -> b) -> [a] -> [b]
```

#### Example Good Code
```haskell
-- Good: Type signature, clear name, pure function
calculateDistance :: Point3D -> Point3D -> Double
calculateDistance p1 p2 =
    sqrt (dx^2 + dy^2 + dz^2)
  where
    dx = x p1 - x p2
    dy = y p1 - y p2
    dz = z p1 - z p2

-- Good: Guards for clarity
evaluateGating :: KalmanState -> Tolerance -> GatingDecision
evaluateGating state tol
    | displacement < tol = BeamOn
    | otherwise          = BeamOff
  where
    displacement = calculateDisplacement state
```

#### Example Bad Code
```haskell
-- Bad: No type signature, unclear name, imperative style
calc p1 p2 = do
    let d = sqrt ((x p1 - x p2)^2 + (y p1 - y p2)^2 + (z p1 - z p2)^2)
    return d  -- Unnecessary IO
```

### C++ Style (cbits/)

- Use **C++11** standard
- Follow **snake_case** for functions and variables
- Use **RAII** for resource management
- Prefer `std::atomic` over manual memory barriers
- Document thread safety assumptions

```cpp
// Good example
class RingBuffer {
private:
    std::atomic<size_t> head_;
    std::atomic<size_t> tail_;
    
public:
    // Thread-safe: Can be called from multiple threads
    bool write(const uint8_t* data, size_t len);
};
```

### Formatting

- **Indentation**: 4 spaces (Haskell), 2 spaces (C++)
- **Line length**: 80-100 characters (soft limit)
- **Future**: Will use `ormolu` for automatic formatting

---

## 🛡️ Error Handling

### Hardware Layer Error Propagation

Error handling in the hardware abstraction layer relies on explicit, typed errors defined in `Hardware.Types.HardwareError`. We avoid runtime exceptions (`error`, `undefined`) to comply with IEC 62304 Class C requirements for fail-safe operations.

#### Detailed Error Types (`HardwareError`)
- `ConnectionLost`: Serial port connection lost or unreadable.
- `ConfigurationFailed String`: Failed to apply sensor configuration.
- `ParseError String`: General parsing failure (e.g., invalid header).
- `Timeout`: Operation timed out (e.g., no response to command).
- `UnknownError String`: Catch-all for unexpected IO errors.
- `MagicWordMissing`: Failed to find the magic word syncing pattern in the stream.
- `InvalidLength`: Packet length is outside the valid range.
- `TlvError String`: TLV block parsing error.
- `DoSAttackDetected`: Potential denial of service (e.g., massive TLV blocks).

#### Retry and Recovery Logic
Transient hardware and connection failures are handled gracefully to ensure the application does not crash.
- Use `configureSensorWithRetry` when communicating with hardware endpoints. It implements automatic retries with a fixed delay (e.g., 100ms) to handle transient serial port errors.
- Parse errors skip corrupted packets rather than crashing the consumer thread. Resyncing happens automatically via `skipToMagicWord`.

#### Error Logging
Critical events are logged to the immutable `auditQueue` ensuring an exact audit trail is maintained for tracking hardware-related anomalies.
- Security-critical events (e.g., `DoSAttackDetected`) are logged with `Critical` severity.
- Recoverable synchronization and parsing errors are logged as `Warning`.

---

## 🧪 Testing Strategy

### Test Organization

```
test/
├── Spec.hs                    # Main test runner
├── FFI/                       # FFI layer tests
├── Hardware/                  # Hardware tests
├── SignalProcessing/          # DSP tests
└── System/                    # Integration tests
```

### Types of Tests

#### 1. Unit Tests (HSpec)
```haskell
spec :: Spec
spec = describe "Phase unwrapping" $ do
    it "handles single wrap" $ do
        let input = [0, pi - 0.1, pi + 0.1, 2*pi]
        let expected = [0, pi - 0.1, pi + 0.1, 2*pi]
        unwrapPhase input `shouldBe` expected
```

#### 2. Property Tests (QuickCheck)
```haskell
prop_linearityOfKalmanFilter :: KalmanState -> Property
prop_linearityOfKalmanFilter state =
    forAll arbitrary $ \scale ->
        let scaled = scaleState scale state
            result1 = kalmanPredict scaled
            result2 = scaleState scale (kalmanPredict state)
        in result1 === result2
```

#### 3. Integration Tests
```haskell
spec :: Spec
spec = describe "End-to-end pipeline" $ do
    it "processes real sensor data" $ do
        frameData <- BS.readFile "test/fixtures/sample_frame.bin"
        let parsed = parseFrame frameData
        parsed `shouldSatisfy` isRight
```

### Running Tests

```bash
# All tests
cabal test

# With coverage
cabal test --enable-coverage
cabal hpc report sgrt-radar-system-test

# Specific module
cabal test --test-options="-m FMCW"

# Verbose output
cabal test --test-show-details=direct

# With QuickCheck options
cabal test --test-options="--qc-max-success=1000"
```

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

#### Use Strict Data
```haskell
-- Enable strict fields globally
{-# LANGUAGE StrictData #-}

-- Or per-field
data SystemState = SystemState
    { currentPoints :: ![Point3D]  -- Strict list
    , !beamState :: BeamState      -- Strict field
    }
```

#### Profile Before Optimizing
```bash
# Build with profiling
cabal build --enable-profiling

# Run with profiling
cabal run sgrt-radar-system-exe -- +RTS -p -RTS

# View report
cat sgrt-radar-system-exe.prof
```

---

## ⚠️ Safety-Critical Code

**Note:** Please refer to our [Security Policy](../SECURITY.md) for vulnerability reporting and triage for safety-critical flaws.

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

### Example Safety Comment
```haskell
-- SAFETY-CRITICAL: Gating Logic
-- Requirement: FR-GAT-001
-- Failure Mode: Beam remains ON during patient motion
-- Mitigation: Hysteresis with conservative thresholds
evaluateGating :: KalmanState -> Tolerance -> GatingDecision
evaluateGating state tol
    | displacement < (tol - hysteresis) = BeamOn
    | displacement > (tol + hysteresis) = BeamOff
    | otherwise = maintainCurrentState
  where
    displacement = calculateDisplacement state
    hysteresis = 0.001  -- 1mm safety margin
```

---

## 🐛 Debugging Tips

### Common Issues

#### GC Pauses
```bash
# Check GC statistics
cabal run sgrt-radar-system-exe -- +RTS -s -RTS

# If pauses > 5ms, tune RTS
cabal run sgrt-radar-system-exe -- +RTS -A32m -n4m -RTS
```

#### FFI Memory Issues
```bash
# Run with Valgrind (requires Linux)
valgrind --leak-check=full cabal run sgrt-radar-system-exe
```

#### Thread Deadlocks
```bash
# Enable thread debugging
cabal run sgrt-radar-system-exe -- +RTS -Ds -RTS
```

### Useful GHC Options

```bash
# Verbose compilation
cabal build -v3

# Show all warnings
cabal build --ghc-options="-Wall -Wcompat"

# Optimize
cabal build --ghc-options="-O2"

# Debug symbols
cabal build --ghc-options="-g"
```

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

### Creating a Release

See [TODO.md](../TODO.md) Release Checklist section

---

---

## 10. Hardware Integration & FFI Safety Framework

When adding new hardware components or sensors, you must adhere strictly to our safety framework to prevent memory leaks and asynchronous exception hazards. The framework provides a centralized set of resource managers, result wrappers, and audit helpers.

### 10.1 Scaffold Generator

To minimize manual memory management errors, use the provided scaffold tool when integrating new sensors.
```bash
python3 tools/generate_hardware_scaffold.py <SensorName>
```
This generates a template (`src/Hardware/<SensorName>.hs`) and a memory-leak test suite (`test/Hardware/<SensorName>Check.hs`). The generated code includes built-in exception-safe resource allocations by default.

### 10.2 Resource Lifecycles & Managers

The framework enforces two primary patterns for handling C memory and lifecycle bounds:

1. **New Creation (The Bracket Pattern):**
   When allocating temporary resources, always use `Control.Exception.bracket` alongside `mask_` and `uninterruptibleMask_`. This guarantees that your cleanup logic (e.g., `free`) runs even if an asynchronous exception is raised immediately after allocation.

   ```haskell
   withSensor :: (Ptr () -> IO a) -> IO a
   withSensor = bracket allocate free
     where
       allocate = mask_ $ c_create_sensor
       free ptr = uninterruptibleMask_ $ c_destroy_sensor ptr
   ```

2. **Attachment to Existing Memory (ForeignPtr):**
   If memory is shared or needs its lifecycle managed by the Haskell Garbage Collector, attach a finalizer via `ForeignPtr`.

   ```haskell
   attachSensor :: Ptr () -> IO (ForeignPtr ())
   attachSensor existingPtr = do
       attached <- c_attach_sensor existingPtr
       newForeignPtr c_destroy_sensor_fun_ptr attached
   ```

### 10.3 Result Wrappers and the FFI Bridge

To prevent dropped return values and unhandled states, all FFI boundary calls **must** be routed through the `BridgeCall` module (`Hardware.FFI.Bridge`). The bridge transforms C-level errors into the `MustHandle` result wrapper, guaranteeing that logic cannot proceed without addressing potential failures.

- **`bridgeHardwareCall` / `bridgeHardwareCallCustom`**: Use these to execute FFI routines safely. They automatically hook into the Safety Audit log.
- **`MustHandle`**: An explicit wrapper over `Either HardwareError a`. You must use `handleHardwareResponse` to extract the value and define handlers for both the success and error branches.

### 10.4 Automated Compliance

To maintain 100% adoption and zero memory-safety violations, automated PR checklists now require confirmation of `MustHandle` and `BridgeCall` usage for all hardware FFI additions.

## 📚 Additional Resources

- **[TODO.md](../TODO.md)** - Current development tasks
- **[BUILD_GUIDE.md](BUILD_GUIDE.md)** - Detailed build instructions
- **[PURPOSE_AND_ARCHITECTURE.md](PURPOSE_AND_ARCHITECTURE.md)** - System architecture
- **[CONTRIBUTING.md](../CONTRIBUTING.md)** - Contribution process
- **Haskell Resources**:
  - [Learn You a Haskell](http://learnyouahaskell.com/)
  - [Real World Haskell](http://book.realworldhaskell.org/)
  - [Haskell Wiki](https://wiki.haskell.org/)

---

**Maintained by:** Development Team  
**Last Review:** January 28, 2026
