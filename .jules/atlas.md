# Atlas's Journal 🗺️

## 2024-05-22 - Initial Survey

**Discovery:**
The project uses a sophisticated architecture to handle real-time radar data in Haskell, specifically addressing GC pauses with pinned memory and FFI.

**Dragons:** 🐉
1.  **Ingestion Engine:** Uses `mallocPlainForeignPtrBytes` and a C-based ring buffer to avoid GC pauses. This is a critical safety component.
2.  **Watchdog:** A dedicated thread that kills the beam if the main loop stalls for >100ms. This is the "Dead Man's Switch".
3.  **Coordinate Transform:** Raw radar data (Sensor Space) must be transformed to Room Coordinates (IEC 61217). Incorrect math here means the beam hits the wrong target.

**Glossary:**
*   **DIBH:** Deep Inspiration Breath Hold. A technique to separate the heart from the chest wall during radiation.
*   **FMCW:** Frequency Modulated Continuous Wave. The radar technology used.
*   **STM:** Software Transactional Memory. Used for concurrency.
*   **Sparkle:** Radar noise.
*   **TLV:** Type-Length-Value. The packet format from the TI sensor.
