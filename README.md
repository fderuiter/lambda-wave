# mmWave-SGRT-hs

### A Fail-Safe, Real-Time Surface Guided Radiation Therapy System utilizing 60 GHz Radar and Haskell.**mmWave-SGRT-hs** is a research-grade implementation of a patient positioning and respiratory gating system for Radiation Oncology. It replaces traditional optical cameras with **60 GHz Millimeter-Wave (FMCW) Radar** to monitor patient breathing with sub-millimeter precision, regardless of room lighting, skin tone, or obstructions.

Built in **Haskell**, the architecture prioritizes correctness, type safety, and bounded latency over raw throughput, utilizing **Software Transactional Memory (STM)** to create a concurrent, mathematically provable gating engine.

---

## 🏥 Clinical Context: The DIBH ImperativeIn radiation therapy for left-sided breast cancer, the **Deep Inspiration Breath Hold (DIBH)** technique is used to physically separate the heart from the chest wall, reducing radiation-induced cardiac toxicity.

Current optical systems (cameras) suffer from:

* **Line-of-Sight issues:** Blocked by the rotating gantry.
* **Privacy concerns:** Requiring patients to expose their chest.
* **Skin tone bias:** Difficulty detecting darker skin tones in low light.

**This project solves these issues using Radar:**

* **Penetrating:** Sees through hospital gowns and sheets.
* **Absolute Depth:** Direct Time-of-Flight measurement (no triangulation required).
* **Robust:** Unaffected by lighting or skin pigmentation.

---

## 🏗️ System ArchitectureThe software is designed as a pipeline of isolated, concurrent actors to handle the noisy "sparkle" of radar data while guaranteeing fail-safe operation.

### Layer 1: The Ingestion Engine (Zero-Copy)To mitigate Haskell's Garbage Collection (GC) pauses ("Stop-the-World"), we utilize **Pinned Memory Ring Buffers**.

* **Mechanism:** A dedicated thread reads OS UART buffers directly into `mallocPlainForeignPtrBytes` (off-heap memory).
* **Result:** Immune to GC latency; prevents packet drops at 921,600 baud.

### Layer 2: Parser & ROI Filter* **Parsing:** Extracts TLV (Type-Length-Value) packets from the byte stream.
* **Coordinate Transform:** Converts Sensor Space (X,Y,Z) to Room Coordinates (IEC 61217) based on gantry angle.
* **ROI Clipping:** A spatial gate discards multipath reflections (walls, floor) outside the treatment couch.

### Layer 3: Surface Meshing (Polynomial Fitting)Raw radar data is sparse (~100 points) and noisy. We treat the torso as a smooth mathematical manifold.

* **Algorithm:** Bi-Quadratic Polynomial Least Squares Regression.


* **Implementation:** Uses `hmatrix` (BLAS/LAPACK) to solve the Normal Equation \mathbf{c} = (A^T A)^{-1} A^T \mathbf{b}.
* **Output:** A stable 20 \times 20 virtual mesh free of sensor jitter.

###Layer 4: The Gating Logic & Kalman Filter* **Latency Compensation:** A Constant Velocity Kalman Filter predicts chest position 50ms into the future to neutralize LINAC beam-off hardware lag.
* **Hysteresis:** Schmidt Trigger logic prevents rapid beam cycling ("chattering").

###Layer 5: The Watchdog (Dead Man's Switch)A high-priority thread monitors the system clock. If the Gating Thread fails to "kick" the watchdog (due to crash, infinite loop, or USB disconnect) within **100ms**, the beam is forced OFF.

---

## ⚡ Hardware Requirements1. **Sensor:** Texas Instruments **IWR6843ISK** (60-64 GHz mmWave Sensor).
2. **Carrier:** MMWAVEICBOOST or simple USB breakout.
3. **Mounting:** Standard tripod or Gantry mount (requires calibration).
4. **Host PC:**
* OS: Linux (Recommended for low-latency IO) or Windows.
* CPU: i5 or better (Matrix operations are CPU bound).



---

## 🛠️ Software Prerequisites* **GHC** (Glasgow Haskell Compiler) >= 9.2
* **Stack** or **Cabal**
* **System Libraries:**
* `liblapack-dev`, `libblas-dev` (for `hmatrix`)
* `freeglut3-dev` (for OpenGL visualization)



### Dependencies (Haskell)```cabal
dependencies:
  - base >= 4.7 && < 5
  - stm                 # Software Transactional Memory
  - hmatrix             # Linear Algebra
  - binary              # Parsing
  - clock               # Monotonic time
  - OpenGLRaw           # Visualization

```

---

##🚀 Installation & Usage1. **Clone the Repo**
```bash
git clone https://github.com/yourusername/mmWave-SGRT-hs.git
cd mmWave-SGRT-hs

```


2. **Flash the Sensor**
Flash the IWR6843 with the standard *Out of Box Demo* firmware provided by TI (SDK 3.5+).
3. **Build**
```bash
stack build

```


4. **Run**
Connect the sensor to USB. Identify your Data and User COM ports.
```bash
# Usage: stack run -- [Data Port] [User Port]
stack run -- /dev/ttyACM1 /dev/ttyACM0

```


5. **Controls**
* `Space`: Toggle Beam Hold (Manual Override).
* `C`: Calibrate Isocenter (Zero the mesh).
* `Esc`: Quit (Safe Shutdown).



---

## 📊 Code Example: The ROI Filter```haskell
-- Simple bounding box logic to remove multipath ghosts
inROI :: Point3D -> Bool
inROI p = 
    x p > -0.3 && x p < 0.3 && -- 60cm Torso Width
    y p >  0.0 && y p < 0.5 && -- 50cm Torso Depth
    z p >  0.5 && z p < 1.5    -- Range Gate (0.5m to 1.5m)

```

## ⚠️ Safety & Disclaimer**THIS SOFTWARE IS FOR RESEARCH PURPOSES ONLY.**

* **Not FDA Cleared:** This system has not been evaluated by the FDA or any regulatory body.
* **Not for Clinical Use:** Do not use this software to control radiation delivery on human patients.
* **No Warranty:** The software is provided "as is," without warranty of any kind.

*Designed in adherence to IEC 62304 concepts, but not certified.*

---

## 📚 References1. *Texas Instruments IWR6843 Technical Reference Manual*
2. *Real-Time Non-Contact Millimeter Wave Radar-Based Vital Sign Detection*, PMC NIH.
3. *Marlow, S.* "Parallel and Concurrent Programming in Haskell." O'Reilly Media.

---

**Maintainer:** [Your Name/Lab Name]
**Contact:** [Email]
