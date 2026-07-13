**3\. The Software Architecture: Haskell Implementation**

The software system is designed as a pipeline of isolated, concurrent actors. This architecture leverages Haskell's strong static typing to prevent valid-but-incorrect states and employs Software Transactional Memory (STM) to manage concurrency without the deadlock risks associated with traditional mutex locking.

### **3.1 Layer 1: The Ingestion Engine ("The Danger Zone")**

The interface between the real-time Operating System (OS) and the managed runtime of a high-level language is arguably the most fragile component of any medical device software. In Haskell, the primary threat to real-time stability is **Garbage Collection (GC) latency**.

#### **3.1.1 The GC Latency Problem**

Haskell's GHC runtime utilizes a generational, copying garbage collector. While highly efficient for throughput, it is "stop-the-world" by default. During a major GC sweep, all Haskell threads are paused. If the application is blocked on a GC pause for 50ms—a plausible scenario under heap pressure—the OS kernel continues to receive UART interrupts from the sensor. The limited FIFO buffer on the UART controller (or the OS kernel buffer) will fill rapidly. Once full, new bytes are dropped.  
In a packetized stream like the IWR6843's TLV protocol, dropping a sequence of bytes corrupts the frame structure. The parser will lose synchronization, necessitating a "scan for magic word" reset. This results in a gap in the breathing data. In a DIBH treatment, a 200ms gap could mask a patient's cough, potentially leading to radiation delivery during a geographic miss.

#### **3.1.2 The Solution: Pinned Memory Ring Buffer**

To mitigate this, the architecture decouples the OS data ingestion from the Haskell heap entirely using Pinned Memory.  
Pinned memory (mallocPlainForeignPtrBytes) is allocated outside the Haskell GC's nursery and is guaranteed not to move.5 This allows the application to pass a stable pointer to the OS read syscall, enabling a zero-copy transfer from kernel space to user space.  
**Architectural Implementation**:

1. **The Buffer**: A large circular ring buffer (e.g., 10 MB) is allocated at startup using Foreign.ForeignPtr. This buffer resides in the "C-heap" but is managed by a Haskell ForeignPtr for finalization.  
2. **The Ingestion Thread**: A dedicated, unbound Haskell thread (spawned via forkOS) acts as the producer.  
   * It enters a tight loop calling the POSIX fdReadBuf (or Windows equivalent).7  
   * Crucially, this thread reads *directly* into the Ptr Word8 derived from the ring buffer's ForeignPtr.  
   * Because it performs minimal allocation (only updating the Write Pointer index), it rarely triggers the Garbage Collector. Even if the main Haskell processing threads are paused for GC, this lightweight thread (which may be treated as a foreign call) can continue to service the OS buffers, provided the GHC runtime scheduler allows the FFI call to complete.9  
3. **Synchronization**: The communication between the Ingestion Thread and the Parser is mediated by an STM variable: bufferState :: TVar (Int, Int), representing the (ReadOffset, WriteOffset).  
   * The Ingestion Thread updates WriteOffset atomically after each successful read.  
   * The Parser Thread "blocks" (retries) on this TVar until WriteOffset advances past ReadOffset.

This design creates a **Zero-Copy Ingestion** pipeline. The data is copied only once: from the UART controller to the Pinned Memory. The GC never scans this large binary blob because it is primitive data, reducing GC pressure and ensuring that the high-bandwidth stream (921.6 kbps) is ingested reliably regardless of the main application's state.

### **3.2 Layer 2: The Parser & ROI Filter**

Once the raw bytes are safely resident in the Pinned Memory Ring Buffer, the **Parser Actor** is responsible for extracting semantic meaning.

#### **3.2.1 Parsing Strategy: binary vs. attoparsec**

The IWR6843 outputs data in a hierarchical TLV format.

* **Frame Header (44 bytes)**: Contains the Sync Pattern (0x0102030405060708), Frame Number, and TLV Count.10  
* **TLV Payloads**: Dynamic length segments containing point clouds, range profiles, or side information.

While attoparsec is renowned for performance in network protocols 11, the binary library (specifically Data.Binary.Get) offers precise control over fixed-width binary words (Word16LE, FloatLE) which mirrors the C-struct layout of the radar firmware.  
Given the Ring Buffer architecture, the Parser utilizes a "pointer walking" strategy. It does not load the entire ring buffer into a Haskell ByteString. Instead, it peeks at the ReadOffset.

1. **Synchronization**: The parser scans the byte stream for the 8-byte Magic Word. This is critical for recovering from connection glitches. If the Magic Word is found at offset $N$, the parser attempts to decode the header at $N$.  
2. **Validation**: The header contains the TotalPacketLen. The parser checks if WriteOffset \- ReadOffset \>= TotalPacketLen. If not, it yields and waits for more data (STM retry).  
3. **Extraction**: Once a complete frame is available, the parser extracts the **Type 1 (Detected Points)** TLV. Each point is a 16-byte struct: x (float), y (float), z (float), velocity (float).

#### **3.2.2 Coordinate Transformation**

The point cloud emerging from the parser is in the **Sensor Coordinate System**.

* $X$: Azimuth.  
* $Y$: Elevation (depending on antenna orientation).  
* $Z$: Depth (Range).

For SGRT, these must be transformed into the Patient Support System (PSS) or Room Coordinate System (IEC 61217). The radar is typically mounted on the LINAC gantry or a ceiling mount. If mounted on the gantry, the sensor rotates with the machine.  
The Parser applies a rigid body transformation matrix $M\_{Gantry}$ to every point $P\_{sensor}$:

$$P\_{room} \= R\_{gantry}(\\theta) \\cdot P\_{sensor} \+ T\_{gantry}(\\theta)$$

This transformation aligns the radar data with the treatment planning CT scan, ensuring that "Up" in the visualization corresponds to Anterior-Posterior movement of the patient.

#### **3.2.3 Region of Interest (ROI) Clipping**

Radar is omnidirectional; it detects reflections from the floor, the gantry, the therapists, and the walls. This "clutter" must be removed before surface fitting.  
We define a virtual Bounding Box around the treatment couch isocenter.

Haskell

\-- Conceptual Haskell Logic for ROI Filter  
inROI :: Point3D \-\> Bool  
inROI p \=   
    x p \> \-0.3 && x p \< 0.3 && \-- 60cm width (Torso width)  
    y p \>  0.0 && y p \< 0.5 && \-- 50cm depth (Torso thickness)  
    z p \>  0.5 && z p \< 1.5    \-- 0.5m to 1.5m range from radar

This filter acts as a spatial gate, discarding multipath ghosts (late reflections that appear far away) and environmental static. Only points strictly within the "patient volume" are passed to Layer 3\.1

## ---


## Explicit Software Unit Interfaces and Failure Boundaries

### FR-DAQ-003: Packet parser validation
- **Module:** `Hardware.Consumer`
- **Interfaces:** The unit exposes typed functional interfaces corresponding to Packet parser validation, completely decoupled from upstream IO.
- **Failure Boundaries:** Invalid state transitions will trigger the watchdog. Errors are bounded to the local STM transaction.

### SR-SOUP-001: GHC RTS deterministic runtime (locked capabilities)
- **Module:** `app/Main.hs`, `.cabal`
- **Interfaces:** The unit exposes typed functional interfaces corresponding to GHC RTS deterministic runtime (locked capabilities), completely decoupled from upstream IO.
- **Failure Boundaries:** Invalid state transitions will trigger the watchdog. Errors are bounded to the local STM transaction.
