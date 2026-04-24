## 2024-04-24 - Throttling High-Frequency Telemetry Text
**Learning:** Displaying raw textual metrics at high frequency (e.g. 30Hz) causes rapid flickering, making it impossible for users to read and increasing cognitive load without providing actionable information. Large numbers without formatting also take longer to mentally parse.
**Action:** Always decouple textual UI updates from underlying high-frequency event loops by throttling them (e.g. to ~4Hz). Additionally, use `toLocaleString()` or similar formatting for large numbers to improve quick readability.
