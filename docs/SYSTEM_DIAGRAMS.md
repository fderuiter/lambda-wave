# Lambda-Wave: System Diagrams & High-Level Documentation

This document provides a comprehensive visual and technical overview of the Lambda-Wave architecture, FFI interfaces, and subsystem details.

## 1. High-Level System Architecture

The Lambda-Wave system is organized into three primary planes and a cross-cutting safety layer, ensuring a clear separation between high-level logic, performance-critical processing, and low-level hardware interaction.

```mermaid
graph TB
    subgraph "Control Plane (Haskell)"
        GL[Gating Logic]
        UM[UI Management]
        CFG[Configuration]
    end

    subgraph "Data Plane (Haskell)"
        FMCW[FMCW Processing]
        PU[Phase Unwrapping]
        KF[Kalman Filter]
        REG[Regression Analysis]
    end

    subgraph "Hardware Interface (C++/FFI)"
        RB[Ring Buffer]
        SIO[Serial I/O]
        GPIO[GPIO Control]
    end

    subgraph "Safety Layer"
        WD[Watchdog]
        AL[Audit Logger]
    end

    %% Interactions
    RB -->|Binary Frames| FMCW
    FMCW --> PU
    PU --> KF
    KF --> REG
    REG -->|State Estimation| GL
    GL -->|Beam Command| GPIO
    CFG -->|Parameters| FMCW
    CFG -->|Serial Commands| SIO

    WD -.->|Monitors| GL
    WD -.->|Monitors| FMCW
    GL -->|Log Event| AL
    GPIO -->|Beam Status| AL

    SIO <-->|UART| Sensor[TI IWR6843ISK Sensor]
    GPIO -->|TTL| LINAC[Linear Accelerator]
```

---

## 2. Foreign Function Interface (FFI) Documentation

Lambda-Wave uses FFI to bridge Haskell's safety and high-level abstractions with C++'s performance and direct hardware access.

### 2.1 Ring Buffer (C++ & Haskell Bridge)

The Ring Buffer is the most critical FFI component, facilitating zero-copy, lock-free data transfer from the UART ingestion thread (C++) to the TLV parser (Haskell).

**FFI Mechanics:**
- **Shared Memory:** A fixed-size circular buffer is allocated in pinned memory on the C++ side.
- **Atomic Offsets:** `write_offset` and `read_offset` are `std::atomic<size_t>` fields in the `RingBufferControl` struct.
- **ABI Stability:** The Haskell `FFI.RingBuffer.Types` module defines a matching layout to ensure correct field access.

```mermaid
sequenceDiagram
    participant C as C++ (Producer)
    participant RB as Ring Buffer (Shared)
    participant H as Haskell (Consumer)

    Note over C, H: Lock-free Synchronization
    C->>RB: Write raw UART bytes
    C->>RB: Atomic update write_offset
    H->>RB: Peek write_offset (Atomic Load)
    H->>RB: Read data from [read_offset, write_offset)
    H->>RB: Atomic update read_offset
```

### 2.2 Hardware Control (GPIO & Serial)

Haskell invokes C functions for synchronous, low-latency operations that require direct system calls or specific hardware drivers.

| Interface | Haskell Module | C Function (cbits) | Purpose |
|-----------|----------------|--------------------|---------|
| **GPIO** | `Hardware.Control` | `gpio_write` | Control the physical beam gating signal (TTL). |
| **GPIO** | `Hardware.Control` | `gpio_setup_watchdog` | Initialize the hardware watchdog timer on Pin 27. |
| **Serial** | `Hardware.Control` | `configure_serial_port` | Set baud rate and terminal attributes (POSIX). |
| **UART** | `Hardware.FFI.Common` | `read_from_uart` | Perform non-blocking reads into the Ring Buffer. |

---

## 3. Low-Level Subsystem Diagrams

### 3.1 Hardware Interface: Lock-Free Ring Buffer Logic

This diagram details the producer-consumer synchronization used for high-throughput radar data ingestion.

```mermaid
graph LR
    subgraph "Producer (C++)"
        UART[UART Read] --> RB_W[Write to buffer_offset + write_offset]
        RB_W --> UPD_W[Atomic Store write_offset]
    end
    subgraph "Consumer (Haskell)"
        UPD_W -.-> PEEK_W[Atomic Load write_offset]
        PEEK_W --> RB_R[Read from buffer_offset + read_offset]
        RB_R --> UPD_R[Atomic Store read_offset]
    end
```

### 3.2 Signal Processing: Data Pipeline

The signal processing pipeline transforms raw frequency-domain data into precise spatial coordinates and filtered motion vectors.

```mermaid
flowchart TD
    Raw[Raw ADC Data] --> BS[Background Subtraction]
    BS --> CZT[Chirp Z-Transform]
    CZT --> PE[Phase Extraction]
    PE --> PU[Phase Unwrapping]
    PU --> KF[Kalman Filter]

    subgraph "Kalman Filter Step"
        KF --> Pred[Predict State]
        Pred --> Upd[Update with Measurement]
        Upd --> Out[Filtered Position/Velocity/Acceleration]
    end
```

### 3.3 Control: Gating and Safety Loops

Safety-critical logic that ensures the radiation beam is only active when the patient is within the prescribed tolerance.

```mermaid
flowchart TD
    subgraph "Gating Loop (Control.Gating)"
        Input[Filtered State] --> Tol{Within Tolerance?}
        Tol -->|Yes| B_ON[Beam ENABLE]
        Tol -->|No| B_OFF[Beam DISABLE]
        B_ON --> Hys{Hysteresis Check}
        B_OFF --> Hys
    end

    subgraph "Watchdog Loop (Safety.Watchdog)"
        Timer[10ms Timer] --> Check{Last Frame < 100ms?}
        Check -->|No| KILL[EMERGENCY SHUTDOWN]
        Check -->|Yes| Timer
    end
```

### 3.4 Numeric: Kinematics and Units

Strictly-typed kinematic relations ensure physical correctness at compile time.

```mermaid
classDiagram
    class Distance { +Double val }
    class Velocity { +Double val }
    class Acceleration { +Double val }
    class Time { +Double val }

    Distance ..> Velocity : |/| Time
    Velocity ..> Acceleration : |/| Time
    Velocity ..> Distance : |*| Time (implied)
    Acceleration ..> Velocity : |*| Time (implied)

    note for Distance "Newtype wrapped Double"
```

### 3.5 Data: State and Audit

The central system state and the immutable audit trail for compliance.

```mermaid
classDiagram
    class SystemState {
        +[Point3D] currentPoints
        +BeamState beamState
        +TimeSpec lastFrameTime
        +TBQueue AuditEvent auditQueue
    }
    class AuditEvent {
        +TimeSpec timestamp
        +Severity severity
        +String component
        +String message
    }
    SystemState "1" *-- "n" AuditEvent
```

### 3.6 UI: Presentation Layer

The visualization layer uses OpenGL to provide real-time feedback to the clinical operator.

```mermaid
graph TD
    ST[System State] -->|STM Read| RP[Render Process]
    RP -->|OpenGL Commands| GL_C[Graphics Canvas]
    GL_C -->|User Interaction| ST
```
