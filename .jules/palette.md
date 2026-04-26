## 2024-05-24 - Throttling High-Frequency Updates for Real-Time UI
**Learning:** High-frequency textual updates (~30Hz) for stats like timestamps and point counts in real-time interfaces cause visual flickering and cognitive overload, making the UI difficult to parse. Unformatted large numbers also reduce readability.
**Action:** Throttle textual DOM updates to ~4Hz and consistently use `toLocaleString()` for large numbers to ensure the UI remains smooth, readable, and accessible, without impacting the underlying real-time data flow.
