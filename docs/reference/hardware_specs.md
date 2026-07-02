# Hardware Specifications Reference

## Sensor Platform: TI IWR6843ISK

*   **Frequency Band:** <!-- MANIFEST:operating_frequency_ghz -->60<!-- /MANIFEST:operating_frequency_ghz --> GHz mmWave
*   **Operating Bandwidth (B):** Up to 4 GHz
*   **Antenna Configuration (MIMO):** 3 TX, 4 RX antennas
*   **Virtual Array Elements:** $N_{TX} \times N_{RX} = 12$ virtual elements
*   **Chirp Profile Configuration:** `config/ti_iwr6843isk/sgrt_profile.cfg`
*   **Frame Period:** 50 ms (20 Hz)
*   **Output Power:** 12 dBm per TX antenna

## Physical Interfaces

*   **Bridge Type:** XDS110 USB-to-UART
*   **User UART (Configuration):** 115,200 baud
*   **Data UART (Streaming):** 921,600 baud (Hard-coded constraint)
*   **Throughput Limit:** $\approx \frac{921,600 \text{ bits/s}}{10 \text{ bits/byte}} \approx 92 \text{ KB/s}$
*   **Maximum Point Rate:** $\approx 4,600 \text{ points/s}$ (at 20 bytes/point)
*   **Target Cloud Sparsity:** $\approx 100-200 \text{ points/frame}$

## Performance & Gating Tolerances

*   **Tracking Precision:** Sub-millimeter
*   **Phase-Based Theoretical Precision:** $\sim 0.1$ mm
*   **System End-to-End Latency Target:** $< 50$ ms
*   **GHC Runtime (RTS) Tuning Parameters:** `-N2` (thread affinity), `-A32m` (nursery size) for low latency
