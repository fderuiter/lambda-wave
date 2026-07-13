## 6\. Layer 5: The Safety Watchdog In a Class II/III medical device, software must be designed with the assumption that it \*will\* fail. The system must fail safely (i.e., Beam Off).

### 6.1 The "Dead Man's Switch" We implement a dedicated, high-priority \*\*Watchdog Thread\*\*. \* \*\*Mechanism\*\*: The Gating Logic thread is required to "kick" the watchdog by updating a shared timestamp \`TVar\` (\`LastKickTime\`) every time it successfully processes a frame and generates a gating decision. \* \*\*Timeout Logic\*\*: The Watchdog wakes up periodically (e.g., every 10ms) and checks the system clock. \`\`\`haskell currentTime \<- System.Clock.getTime Monotonic if (currentTime \- lastKickTime) \> 100 \* ms then forceBeamOff \`\`\` \* \*\*Failure Coverage\*\*: \* \*\*USB Disconnect\*\*: The Ingestion thread blocks/fails, no new data reaches the Parser, the Gating Logic starves, the Watchdog trips. \* \*\*GC Freeze\*\*: If a massive GC pause halts the Gating thread for \>100ms, the Watchdog (if scheduled) or the hardware failsafe triggers. \* \*\*Infinite Loop\*\*: If the polynomial fitting diverges or enters a loop, the Watchdog trips.

### 6.2 Precision Timing Standard Haskell \`UTCTime\` or \`getClockTime\` is insufficient due to system clock drift (NTP updates). We utilize the \*\*\`clock\`\*\* library to access the OS \`CLOCK\_MONOTONIC\` (Linux) or \`QueryPerformanceCounter\` (Windows) for nanosecond-precision timing that is immune to system time changes.\[17, 18\] \---


## Explicit Software Unit Interfaces and Failure Boundaries

### SR-WD-001: Watchdog monitors all critical threads
- **Module:** `Safety.Watchdog`
- **Interfaces:** The unit exposes typed functional interfaces corresponding to Watchdog monitors all critical threads, completely decoupled from upstream IO.
- **Failure Boundaries:** Invalid state transitions will trigger the watchdog. Errors are bounded to the local STM transaction.

### SR-WD-002: Application termination on timeout (100ms)
- **Module:** `Safety.Watchdog`
- **Interfaces:** The unit exposes typed functional interfaces corresponding to Application termination on timeout (100ms), completely decoupled from upstream IO.
- **Failure Boundaries:** Invalid state transitions will trigger the watchdog. Errors are bounded to the local STM transaction.

### SR-AUDIT-001: Immutable event log
- **Module:** `Safety.Audit`
- **Interfaces:** The unit exposes typed functional interfaces corresponding to Immutable event log, completely decoupled from upstream IO.
- **Failure Boundaries:** Invalid state transitions will trigger the watchdog. Errors are bounded to the local STM transaction.

### SR-IPC-001: Process boundary isolation; all IPC heartbeats are monitored by the safety daemon
- **Module:** `Safety.Watchdog`, `app/Main.hs`, `app/VisualizerMain.hs`
- **Interfaces:** The unit exposes typed functional interfaces corresponding to Process boundary isolation; all IPC heartbeats are monitored by the safety daemon, completely decoupled from upstream IO.
- **Failure Boundaries:** Invalid state transitions will trigger the watchdog. Errors are bounded to the local STM transaction.
