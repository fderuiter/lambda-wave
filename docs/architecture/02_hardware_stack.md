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

This sequence creates a virtual array of 12 virtual elements (see [Hardware Specifications](docs/reference/hardware_specs.md) for formulas).2 The physical geometry of the ISK antenna board arranges these virtual elements in an L-shaped pattern (or similar non-linear arrangement), which provides the spatial diversity necessary to calculate the Angle of Arrival (AoA) in both $\\theta$ (azimuth) and $\\phi$ (elevation). Without this Elevation capability, the radar would collapse the patient's 3D chest volume into a 2D accumulation, making it impossible to distinguish chest expansion from abdominal movement or patient rotation.2

### **2.2 Interface Bottlenecks: The XDS110 Limitation**

The sensor communicates with the host PC via the XDS110 USB-to-UART bridge. This interface presents two COM ports:

1. **User UART (115200 baud)**: Used for configuring the sensor (sending the chirp profile).  
2. **Data UART (921600 baud)**: Used for streaming the processed point cloud (TLV packets).

The 921,600 baud rate imposes a hard physical constraint on the system's throughput. For detailed throughput and data limit calculations, see [Hardware Specifications](docs/reference/hardware_specs.md).

At a target frame rate of 20 Hz (sufficient for respiratory monitoring), the system can support a maximum of roughly 230 points per frame. This limitation is critical: it confirms that the system cannot rely on a high-density "Lidar-like" scan. Instead, it receives a sparse cloud of \~100-200 reliable points reflected from the patient's skin. This hardware constraint necessitates the algorithmic sophistication of Layer 3 (Surface Meshing), where a mathematical surface is fitted to this sparse data to generate a high-fidelity visualization.

### **2.3 Configuration Strategy**

To support the Haskell processing pipeline, the firmware configuration (profile\_3d.cfg) must be rigorously defined to ensure TDM operation and appropriate velocity resolution.

* **Frame Period**: 50 ms (20 Hz). This provides adequate temporal resolution for breathing (typically 0.2 \- 0.3 Hz) while allowing the Haskell runtime 50ms to process each frame, reducing the risk of pipeline stalls.  
* **Velocity Resolution**: Breathing involves slow velocities (0.01 m/s \- 0.1 m/s). The chirp configuration must use a sufficient number of loops (e.g., 64 or 128 chirps per frame) to maximize Doppler sensitivity. This allows the Kalman filter (Layer 4\) to distinguish the moving chest wall from the static treatment couch.4  
* **Clutter Removal**: The static clutter removal feature on the DSP should be enabled to filter out the couch and gantry, but the Haskell layer will perform additional, more aggressive spatial filtering.

## ---


## Explicit Software Unit Interfaces and Failure Boundaries

### FR-DAQ-002: Sensor Configuration
- **Module:** `Hardware.Control`, `Data.Config`
- **Interfaces:** The unit exposes typed functional interfaces corresponding to Sensor Configuration, completely decoupled from upstream IO.
- **Failure Boundaries:** Invalid state transitions will trigger the watchdog. Errors are bounded to the local STM transaction.
