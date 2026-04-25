## 2024-04-25 - Prevent Textual Flickering in High-Frequency Streams
**Learning:** Real-time text updates at 30Hz (like frame times or point counts) cause visual flickering and cognitive overload, rendering the numbers unreadable.
**Action:** Always decouple textual UI updates from the main event loop by throttling them to ~4Hz and format large numbers with `toLocaleString()` for better scannability.
