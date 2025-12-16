# **Architecture and Implementation of a Fail-Safe, Real-Time Surface Guided Radiation Therapy (SGRT) System Utilizing 60 GHz Millimeter-Wave Radar and Haskell**

## **1\. Clinical and Theoretical Framework: The Imperative for Radar SGRT**

### **1.1 The Evolution of Patient Positioning in Radiation Oncology**

The discipline of radiation oncology is currently undergoing a significant paradigm shift, transitioning from internal surrogates and static setup markers toward real-time, continuous surface guidance. Historically, patient positioning relied on three-point tattoo setups aligned with room lasers. While this aligns the skin, it fails to account for internal anatomical shifts, respiratory motion, or intrafraction patient relaxation. Surface Guided Radiation Therapy (SGRT) addresses this by monitoring the patient's external surface topology in real-time, ensuring that the radiation beam is delivered only when the patient is in the exact planned position.

The most clinically critical application of SGRT is the management of respiratory motion, particularly for left-sided breast cancer and thoracic malignancies. In these scenarios, the Deep Inspiration Breath Hold (DIBH) technique is employed to expand the lung volume, which physically distances the heart from the chest wall and the high-dose radiation field. This geometric separation significantly reduces the risk of radiation-induced cardiac toxicity, a long-term complication that can negate the survival benefits of the cancer treatment. However, the efficacy of DIBH is contingent upon precise, millisecond-level synchronization. The radiation beam must be triggered "ON" only when the patient’s chest surface falls within a specific spatial window—typically a 2-3 mm tolerance band representing peak inspiration—and remains stable. Conversely, if the patient exhales, coughs, or relaxes the breath hold, the beam must terminate immediately (Beam-Hold) to prevent irradiation of the heart or healthy lung tissue.

Current standard-of-care systems utilize optical structured light cameras. While effective, these optical systems suffer from inherent limitations: they require a direct line of sight, making them susceptible to occlusion by the large rotating gantry of the linear accelerator (LINAC); they can be sensitive to patient skin tone and ambient lighting conditions; and they require complex calibration to merge data from multiple camera pods.

### **1.2 The Millimeter-Wave Radar Alternative**

This project proposes a disruptive architectural shift by utilizing 60 GHz millimeter-wave (mmWave) radar as the primary sensing modality. Unlike optical photons, 60 GHz electromagnetic waves (wavelength $\\lambda \\approx 5$ mm) penetrate standard hospital gowns and sheets, preserving patient privacy while maintaining robust signal integrity regardless of room lighting or skin pigmentation. Furthermore, Frequency Modulated Continuous Wave (FMCW) radar provides direct time-of-flight distance measurement, which is inherently absolute, whereas stereo-optical systems must infer depth through parallax and triangulation.

However, the application of industrial radar sensors—typically designed for automotive cruise control or occupancy detection—to the high-stakes environment of medical gating introduces profound challenges. The data is noisy ("sparkle"), the interface bandwidth is constrained, and the consequences of software failure are catastrophic. To address these challenges, this system employs a rigorous software architecture built in Haskell. This choice prioritizes correctness, type safety, and mathematically provable concurrency via Software Transactional Memory (STM) over the rapid-prototyping convenience of Python or C++. This report details the comprehensive design, from the physics of the TI IWR6843 sensor to the linear algebra of the surface reconstruction engine.

## ---

**2\. The Hardware Stack: Sensor Physics and Configuration**

### **2.1 The Sensor: TI IWR6843ISK**

The core of the sensing stack is the Texas Instruments IWR6843ISK, a single-chip mmWave sensor operating in the 60-64 GHz band. This device integrates the RF front end, a DSP (C674x), and an ARM Cortex-R4F microcontroller. For this specific SGRT application, the sensor is treated not as an edge-computing device, but as a high-fidelity streamer of raw point cloud data to the host PC, where the Haskell runtime assumes control.

#### **2.1.1 FMCW Radar Fundamentals and Resolution**

The IWR6843 operates on the principle of Frequency Modulated Continuous Wave (FMCW) radar. The transmitter generates a "chirp"—a sinusoid whose frequency increases linearly with time. When this signal reflects off the patient's chest and returns to the receiver, it is mixed with the currently transmitting signal. Because the transmitted frequency has increased during the round-trip time, the mixer output contains a "beat frequency" ($f\_b$) that is directly proportional to the distance to the target ($R$).

The fundamental range resolution ($\\Delta R$) of the system is dictated by the total bandwidth ($B$) of the chirp:

$$\\Delta R \= \\frac{c}{2B}$$

where $c$ is the speed of light. The IWR6843 supports a bandwidth of up to 4 GHz. Utilizing the full 4 GHz bandwidth yields a theoretical range resolution of approximately 3.75 cm. While this coarse resolution might seem insufficient for mm-level gating, the accuracy of the phase measurement allows for sub-millimeter precision in determining the displacement of the chest wall, provided the Signal-to-Noise Ratio (SNR) is sufficient.1 The chirp configuration must therefore be optimized to maximize bandwidth (4 GHz sweep) while maintaining a chirp duration that places the beat frequency of the patient (at \~80 cm distance) well within the Intermediate Frequency (IF) bandwidth of the ADC.

#### **2.1.2 TDM-MIMO Beamforming for 3D Imaging**

A standard radar with a single Transmit (TX) and Receive (RX) chain measures range and velocity but has poor angular resolution. To generate the required 3D surface mesh of the patient's torso, the system must resolve targets in both Azimuth (horizontal) and Elevation (vertical) planes. The IWR6843ISK employs Time Division Multiplexing (TDM) Multiple Input Multiple Output (MIMO) technology to synthesize a virtual aperture significantly larger than the physical antenna array.

In the TDM scheme, the three TX antennas fire sequentially in repeating frames.

* **Chirp 0**: TX1 fires. The signal is received by all 4 RX antennas.  
* **Chirp 1**: TX2 fires. The signal is received by all 4 RX antennas.  
* **Chirp 2**: TX3 fires. The signal is received by all 4 RX antennas.

This sequence creates a virtual array of $N\_{TX} \\times N\_{RX} \= 12$ virtual elements.2 The physical geometry of the ISK antenna board arranges these virtual elements in an L-shaped pattern (or similar non-linear arrangement), which provides the spatial diversity necessary to calculate the Angle of Arrival (AoA) in both $\\theta$ (azimuth) and $\\phi$ (elevation). Without this Elevation capability, the radar would collapse the patient's 3D chest volume into a 2D accumulation, making it impossible to distinguish chest expansion from abdominal movement or patient rotation.2

### **2.2 Interface Bottlenecks: The XDS110 Limitation**

The sensor communicates with the host PC via the XDS110 USB-to-UART bridge. This interface presents two COM ports:

1. **User UART (115200 baud)**: Used for configuring the sensor (sending the chirp profile).  
2. **Data UART (921600 baud)**: Used for streaming the processed point cloud (TLV packets).

The 921,600 baud rate imposes a hard physical constraint on the system's throughput.

$$\\text{Throughput} \\approx \\frac{921,600 \\text{ bits/s}}{10 \\text{ bits/byte (8N1 overhead)}} \\approx 92 \\text{ KB/s}$$

A rich point cloud containing X, Y, Z coordinates, Doppler velocity, and SNR intensity typically requires 16-20 bytes per point.

$$\\frac{92,000 \\text{ bytes/s}}{20 \\text{ bytes/point}} \\approx 4,600 \\text{ points/s}$$

At a target frame rate of 20 Hz (sufficient for respiratory monitoring), the system can support a maximum of roughly 230 points per frame. This limitation is critical: it confirms that the system cannot rely on a high-density "Lidar-like" scan. Instead, it receives a sparse cloud of \~100-200 reliable points reflected from the patient's skin. This hardware constraint necessitates the algorithmic sophistication of Layer 3 (Surface Meshing), where a mathematical surface is fitted to this sparse data to generate a high-fidelity visualization.

### **2.3 Configuration Strategy**

To support the Haskell processing pipeline, the firmware configuration (profile\_3d.cfg) must be rigorously defined to ensure TDM operation and appropriate velocity resolution.

* **Frame Period**: 50 ms (20 Hz). This provides adequate temporal resolution for breathing (typically 0.2 \- 0.3 Hz) while allowing the Haskell runtime 50ms to process each frame, reducing the risk of pipeline stalls.  
* **Velocity Resolution**: Breathing involves slow velocities (0.01 m/s \- 0.1 m/s). The chirp configuration must use a sufficient number of loops (e.g., 64 or 128 chirps per frame) to maximize Doppler sensitivity. This allows the Kalman filter (Layer 4\) to distinguish the moving chest wall from the static treatment couch.4  
* **Clutter Removal**: The static clutter removal feature on the DSP should be enabled to filter out the couch and gantry, but the Haskell layer will perform additional, more aggressive spatial filtering.

## ---

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

**4\. Layer 3: The MIMO Surface Mesher (2.5D Polynomial Fitting)**

This layer represents the core algorithmic innovation of the system. The raw radar point cloud is sparse (\~100 points) and noisy ("sparkle"). Direct visualization of these points (e.g., via Delaunay triangulation) would result in a chaotic, flickering mesh that is clinically unusable.  
Instead, we treat the patient's torso as a continuous mathematical function: a 2.5D Surface.

### **4.1 The Mathematical Model**

We assume the patient's chest and abdomen form a smooth, convex manifold where height ($Z$) is a function of lateral position ($X, Y$). A Bi-Quadratic Polynomial provides an excellent approximation of human torso topology:

$$Z(x,y) \= c\_0 \+ c\_1 x \+ c\_2 y \+ c\_3 x^2 \+ c\_4 y^2 \+ c\_5 xy$$

* $c\_0$: Mean height (DC offset).  
* $c\_1, c\_2$: Slope/Tilt (Planar components).  
* $c\_3, c\_4$: Curvature (Quadratic components, representing the chest arch).  
* $c\_5$: Twist.

### **4.2 The Least Squares Solution**

To find the coefficient vector $\\mathbf{c} \= \[c\_0, c\_1, c\_2, c\_3, c\_4, c\_5\]^T$, we employ the method of Linear Least Squares. We want to minimize the sum of squared differences between the radar-measured $z\_i$ and the model-predicted $Z(x\_i, y\_i)$.  
For $N$ points in the ROI, we construct the Design Matrix $A$ (size $N \\times 6$) and the observation vector $\\mathbf{b}$ (size $N \\times 1$):

$$A \= \\begin{bmatrix} 1 & x\_1 & y\_1 & x\_1^2 & y\_1^2 & x\_1 y\_1 \\\\ 1 & x\_2 & y\_2 & x\_2^2 & y\_2^2 & x\_2 y\_2 \\\\ \\vdots & \\vdots & \\vdots & \\vdots & \\vdots & \\vdots \\\\ 1 & x\_N & y\_N & x\_N^2 & y\_N^2 & x\_N y\_N \\end{bmatrix}, \\quad \\mathbf{b} \= \\begin{bmatrix} z\_1 \\\\ z\_2 \\\\ \\vdots \\\\ z\_N \\end{bmatrix} $$The optimal coefficients $\\mathbf{c}$ are found by solving the Normal Equation:$$ A^T A \\mathbf{c} \= A^T \\mathbf{b} \\quad \\implies \\quad \\mathbf{c} \= (A^T A)^{-1} A^T \\mathbf{b} $$ \#\#\# 4.3 Implementation with \`hmatrix\` In Haskell, the \*\*\`hmatrix\`\*\* library provides a high-performance interface to standard BLAS/LAPACK routines (like \`dgels\` or \`dgesvd\`). \`\`\`haskell import Numeric.LinearAlgebra fitSurface :: \-\> Vector Double fitSurface points \= let a \= fromLists \[1, x p, y p, x p^2, y p^2, x p \* y p\] | p \<- points \] b \= fromList \[ z p | p \<- points \] in a \<\\\> b \-- The least squares solver operator \`\`\` The \`\<|\>\` operator automatically handles the over-determined system solver. This approach transforms the noise problem into a statistics problem. Random radar "sparkle" (outliers) will have high residuals but will not significantly skew the global surface fit, effectively acting as a powerful, zero-latency spatial low-pass filter.\[13, 14\] \#\#\# 4.4 Mesh Generation Once the coefficients $\\mathbf{c}$ are computed, the system generates a \*\*Virtual Mesh\*\*. We define a fixed $20 \\times 20$ grid of nodes $(u, v)$ spanning the ROI. For each node, we evaluate the polynomial: $$Z\_{mesh}(u,v) \= c\_0 \+ c\_1 u \+ \\dots$$ This generates a set of 400 stable vertices that form a smooth "breathing blanket." This mesh is what is visualized and used for gating, providing a stable, jitter-free representation of the patient's respiration. \--- \#\# 5\. Layer 4: The Gating Logic (The Brain) This layer translates the 3D surface model into a binary "Beam ON/OFF" decision. \#\#\# 5.1 Signal Extraction Gating requires a 1D signal: the "Breathing Amplitude." In optical marker systems, this is the Z-height of a specific plastic box. In this surface-based system, we calculate the \*\*Weighted Average Height\*\* of the virtual mesh. $$V\_{resp}(t) \= \\frac{1}{\\text{Area}} \\iint\_{ROI} Z\_{mesh}(x,y,t) \\,dx\\,dy$$ By integrating over the polynomial surface (or averaging the 400 mesh points), we further improve Signal-to-Noise Ratio (SNR). If one part of the chest flickers up and another down due to noise, the average remains stable. \#\#\# 5.2 Kalman Filtering and Latency Compensation The signal $V\_{resp}(t)$ represents the \*current\* state. However, the LINAC beam has a hardware latency. When the software sends the "Beam Off" command, it may take 50-100ms for the radiation to actually cease. In high-dose rate delivery, this lag can result in significant overdose to healthy tissue if the patient coughs. To compensate, we implement a \*\*Constant Velocity Kalman Filter\*\*.\[15, 16\] \* \*\*State Vector\*\*: $\\mathbf{x}\_k \= \[ \\text{position}, \\text{velocity} \]^T$. \* \*\*Prediction Step\*\*: Project the state forward based on Newtonian physics. $$\\hat{\\mathbf{x}}\_{k|k-1} \= \\begin{bmatrix} 1 & \\Delta t \\\\ 0 & 1 \\end{bmatrix} \\hat{\\mathbf{x}}\_{k-1}$$ \* \*\*Update Step\*\*: Correct the prediction using the new measurement $V\_{resp}(t)$. \* \*\*Forecasting\*\*: We calculate the predicted position at time $t \+ \\text{Latency}$. $$\\text{GatingSignal} \= \\text{position}\_k \+ \\text{velocity}\_k \\times 0.05 \\text{s}$$ This allows the system to trigger the beam-hold \*before\* the patient exits the tolerance window, effectively neutralizing the hardware lag. \#\#\# 5.3 Hysteresis Logic To prevent "chattering" (rapid ON/OFF switching) when the breathing amplitude hovers near the threshold, we employ a \*\*Schmidt Trigger (Hysteresis)\*\*. \* \*\*Beam ON Condition\*\*: Signal \> (Threshold \+ Tolerance). \* \*\*Beam OFF Condition\*\*: Signal \< (Threshold \- Tolerance). This creates a "dead zone" where the beam state is maintained, ensuring that the beam is only active when the patient is decisively within the breath-hold window. \--- \#\# 6\. Layer 5: The Safety Watchdog In a Class II/III medical device, software must be designed with the assumption that it \*will\* fail. The system must fail safely (i.e., Beam Off). \#\#\# 6.1 The "Dead Man's Switch" We implement a dedicated, high-priority \*\*Watchdog Thread\*\*. \* \*\*Mechanism\*\*: The Gating Logic thread is required to "kick" the watchdog by updating a shared timestamp \`TVar\` (\`LastKickTime\`) every time it successfully processes a frame and generates a gating decision. \* \*\*Timeout Logic\*\*: The Watchdog wakes up periodically (e.g., every 10ms) and checks the system clock. \`\`\`haskell currentTime \<- System.Clock.getTime Monotonic if (currentTime \- lastKickTime) \> 100 \* ms then forceBeamOff \`\`\` \* \*\*Failure Coverage\*\*: \* \*\*USB Disconnect\*\*: The Ingestion thread blocks/fails, no new data reaches the Parser, the Gating Logic starves, the Watchdog trips. \* \*\*GC Freeze\*\*: If a massive GC pause halts the Gating thread for \>100ms, the Watchdog (if scheduled) or the hardware failsafe triggers. \* \*\*Infinite Loop\*\*: If the polynomial fitting diverges or enters a loop, the Watchdog trips. \#\#\# 6.2 Precision Timing Standard Haskell \`UTCTime\` or \`getClockTime\` is insufficient due to system clock drift (NTP updates). We utilize the \*\*\`clock\`\*\* library to access the OS \`CLOCK\_MONOTONIC\` (Linux) or \`QueryPerformanceCounter\` (Windows) for nanosecond-precision timing that is immune to system time changes.\[17, 18\] \--- \#\# 7\. Visualization: OpenGL/Vulkan The user interface requires a real-time, 3D wireframe display of the patient's breathing surface to allow the therapist to visually verify alignment. \#\#\# 7.1 Performance Constraints: \`Gloss\` vs. \`OpenGLRaw\` While the Haskell library \`Gloss\` is excellent for 2D visualizations, it is ill-suited for this application. \`Gloss\` typically rebuilds the entire scene graph every frame and lacks efficient mechanisms for updating dynamic 3D geometry.\[19, 20\] Updating 400 vertices at 20-30 FPS requires \*\*Vertex Buffer Objects (VBOs)\*\*. \#\#\# 7.2 Efficient Mesh Rendering with \`OpenGLRaw\` We utilize \*\*\`Graphics.Rendering.OpenGL.Raw\`\*\* for direct access to the GPU pipeline. 1\. \*\*Initialization\*\*: A static VBO is allocated for the grid indices (connectivity), and a dynamic VBO is allocated for the vertex positions. 2\. \*\*Per-Frame Update\*\*: \* The CPU calculates the 400 $(x,y,z)$ coordinates of the virtual mesh (from Layer 3). \* We use \`glBufferSubData\` (or \`glMapBuffer\`) to upload this small dataset (\~5 KB) to the GPU. This avoids the overhead of immediate mode (\`glBegin/glEnd\`).\[21\] 3\. \*\*Rendering\*\*: \* We set \`glPolygonMode(GL\_FRONT\_AND\_BACK, GL\_LINE)\` to render a wireframe.\[22\] This allows the therapist to see "through" the surface to the target isocenter. \* A Fragment Shader colors the mesh \*\*Green\*\* if the Gating Logic is "Beam On" and \*\*Red\*\* if "Beam Off," providing immediate visual feedback. \--- \#\# 8\. Conclusion This report defines a rigorous architecture for a \*\*Haskell-based Radar SGRT system\*\*. By combining the physics of \*\*60 GHz TDM-MIMO radar\*\* with the safety guarantees of \*\*Functional Programming\*\*, the system addresses the critical challenges of medical gating. \* \*\*Pinned Memory Ring Buffers\*\* solve the GC latency problem, ensuring reliable data ingestion. \* \*\*Polynomial Surface Fitting\*\* solves the "Radar Sparkle" problem, transforming noisy point clouds into clinical-grade surface models. \* \*\*STM and Watchdogs\*\* ensure that the system fails safely, a non-negotiable requirement for radiation therapy. This architecture demonstrates that Haskell is not only viable but superior for high-assurance embedded control systems where correctness and safety are paramount. The resulting system offers a non-contact, privacy-preserving, and low-cost alternative to current optical SGRT technologies, potentially democratizing access to high-precision DIBH treatments. \*\*(Citations included inline throughout the text via source IDs)\*\*.\[1, 2, 3, 4, 5, 6, 7, 9, 10, 11, 12, 13, 14, 15, 16, 17, 20, 21, 22, 23, 24, 25\]$$

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