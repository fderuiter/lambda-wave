# **Architecture and Implementation of a Fail-Safe, Real-Time Surface Guided Radiation Therapy (SGRT) System Utilizing 60 GHz Millimeter-Wave Radar and Haskell**


## Subsystems

- [01_clinical_framework.md](docs/architecture/01_clinical_framework.md)
- [02_hardware_stack.md](docs/architecture/02_hardware_stack.md)
- [03_software_architecture.md](docs/architecture/03_software_architecture.md)
- [04_surface_mesher.md](docs/architecture/04_surface_mesher.md)
- [05_gating_logic.md](docs/architecture/05_gating_logic.md)
- [06_safety_watchdog.md](docs/architecture/06_safety_watchdog.md)
- [07_visualization.md](docs/architecture/07_visualization.md)
- [08_conclusion.md](docs/architecture/08_conclusion.md)


#### **Works cited**

1. Real-Time Non-Contact Millimeter Wave Radar-Based Vital Sign Detection \- PMC \- NIH, accessed December 15, 2025, [https://pmc.ncbi.nlm.nih.gov/articles/PMC9573470/](https://pmc.ncbi.nlm.nih.gov/articles/PMC9573470/)  
2. IWR6843ISK-ODS: Chirp config example for enabled 1 Tx to 1 Rx antenna for 3D point cloud demo \- Sensors forum \- TI E2E, accessed December 15, 2025, [https://e2e.ti.com/support/sensors-group/sensors/f/sensors-forum/926591/iwr6843isk-ods-chirp-config-example-for-enabled-1-tx-to-1-rx-antenna-for-3d-point-cloud-demo](https://e2e.ti.com/support/sensors-group/sensors/f/sensors-forum/926591/iwr6843isk-ods-chirp-config-example-for-enabled-1-tx-to-1-rx-antenna-for-3d-point-cloud-demo)  
3. IWR6843ISK: Waveform for multiple TX antennas \- Sensors forum \- TI E2E, accessed December 15, 2025, [https://e2e.ti.com/support/sensors-group/sensors/f/sensors-forum/863558/iwr6843isk-waveform-for-multiple-tx-antennas](https://e2e.ti.com/support/sensors-group/sensors/f/sensors-forum/863558/iwr6843isk-waveform-for-multiple-tx-antennas)  
4. IWR6843ISK: Chirp configuration \- Sensors forum \- TI E2E, accessed December 15, 2025, [https://e2e.ti.com/support/sensors-group/sensors/f/sensors-forum/818322/iwr6843isk-chirp-configuration](https://e2e.ti.com/support/sensors-group/sensors/f/sensors-forum/818322/iwr6843isk-chirp-configuration)  
5. Haskell FFI: Correct way to pass in and return ByteStrings \- Stack Overflow, accessed December 15, 2025, [https://stackoverflow.com/questions/50481610/haskell-ffi-correct-way-to-pass-in-and-return-bytestrings](https://stackoverflow.com/questions/50481610/haskell-ffi-correct-way-to-pass-in-and-return-bytestrings)  
6. Foreign.ForeignPtr, accessed December 15, 2025, [https://www.cis.upenn.edu/\~bcpierce/courses/552-2008/resources/base/Foreign.ForeignPtr.html](https://www.cis.upenn.edu/~bcpierce/courses/552-2008/resources/base/Foreign.ForeignPtr.html)  
7. System.Posix.IO, accessed December 15, 2025, [http://lambda.inf.elte.hu/haskell/doc/libraries/unix-2.7.2.2/System-Posix-IO.html](http://lambda.inf.elte.hu/haskell/doc/libraries/unix-2.7.2.2/System-Posix-IO.html)  
8. System.Posix.IO.PosixString \- Haskell.org Downloads, accessed December 15, 2025, [https://downloads.haskell.org/ghc/latest/docs/libraries/unix-2.8.6.0-7cbc/System-Posix-IO-PosixString.html](https://downloads.haskell.org/ghc/latest/docs/libraries/unix-2.8.6.0-7cbc/System-Posix-IO-PosixString.html)  
9. 6.17. Foreign function interface (FFI) \- Haskell, accessed December 15, 2025, [https://ghc.gitlab.haskell.org/ghc/doc/users\_guide/exts/ffi.html](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/ffi.html)  
10. Understanding the Out of Box Demo Data Output Frame Header \- TI E2E, accessed December 15, 2025, [https://e2e.ti.com/cfs-file/\_\_key/communityserver-discussions-components-files/1023/understand\_5F00\_OOB\_5F00\_output.pdf](https://e2e.ti.com/cfs-file/__key/communityserver-discussions-components-files/1023/understand_5F00_OOB_5F00_output.pdf)  
11. haskell/attoparsec: A fast Haskell library for parsing ByteStrings \- GitHub, accessed December 15, 2025, [https://github.com/haskell/attoparsec](https://github.com/haskell/attoparsec)  
12. attoparsec or parsec in haskell \- Stack Overflow, accessed December 15, 2025, [https://stackoverflow.com/questions/19208231/attoparsec-or-parsec-in-haskell](https://stackoverflow.com/questions/19208231/attoparsec-or-parsec-in-haskell)


## Auto-Generated Architecture
<!-- ARCHITECTURE-START -->
### Extracted from `src/FFI/Hud/Types.hs`

Types for HUD FFI.

Failure Modes: Memory corruption if C struct layout mismatches.
Mitigations: Explicit Storable instances with fixed byte offsets.
Traceability: REQ-HUD-001

### Extracted from `src/FFI/RingBuffer/IO.hs`

High-assurance FFI Bridge Logic for RingBuffer.

The FFI bridge guarantees memory safety when interfacing between Haskell and C++ drivers.

Failure Modes:
* Buffer overflow if consumer falls behind producer.
* FFI boundary corruption during context switch.

Mitigations:
* Strict read/write offset tracking using atomic memory operations.
* Hard boundary bounds-checking enforced by `enforce_bounds.py`.
* Minimal language extensions used for stability.
* Uses non-blocking best-effort audit logging to prevent ingestion thread suspension.

Traceability: FR-DAQ-001, FR-DAQ-004

### Extracted from `cbits/src/serial_config.cpp`

Hardware Serial Configuration Driver

Configures the raw UART settings for the safety-critical radar sensor connection.

Failure Modes:
* Silent data corruption due to parity or framing errors in noisy environments.
* Port lockup due to incorrect flow control or canonical mode settings.

Mitigations:
* Enforces raw 8N1 transmission with no software flow control.
* Disables all special character handling (ECHO, ISIG) to prevent parsing bugs.

Traceability:
* Requirement FR-DAQ-003: Robust sensor telemetry
* Hazard H-SOUP-002: Malformed serial input

### Extracted from `cbits/src/ring_buffer.cpp`

High-Performance Ring Buffer Memory Manager

Implements a lock-free, zero-copy shared memory ring buffer for inter-process communication.

Failure Modes:
* Race conditions during multi-producer/multi-consumer access causing memory corruption.
* Memory leaks if shared memory segments are not unlinked on abnormal termination.

Mitigations:
* Uses std::atomic for read/write offset management ensuring memory ordering.
* Employs RAII and strict lifecycle control to cleanup /dev/shm artifacts.

Traceability:
* Requirement FR-DAQ-004: Low-latency IPC
* Hazard H-SOUP-003: FFI Memory Leaks

### Extracted from `cbits/src/gpio_driver.cpp`

Hardware GPIO Driver

Manages the low-level physical pin mapping and watchdog interlocks for the SGRT hardware.

Failure Modes:
* Unexpected physical pin state transitions leading to hardware damage.
* Watchdog failure due to software lockup or memory map corruption.

Mitigations:
* Atomic pin state tracking and hardware interlocks for the watchdog.
* Automatic safe-state transition on fatal signals (SIGTERM, SIGSEGV).

Traceability:
* Requirement FR-DAQ-002: Hardware safety interlocks
* Hazard H-HW-001: Uncontrolled pin state
<!-- ARCHITECTURE-END -->
