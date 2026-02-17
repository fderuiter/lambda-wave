# Real-Time Web Dashboard

Lambda-Wave includes a lightweight, real-time web dashboard for remote monitoring of patient position and system status.

## Architecture

The dashboard is built on a **Client-Server** model where the Haskell application acts as the WebSocket server, streaming state updates to a browser-based client.

### Backend (Haskell)
- **Module:** `Control.WebUI.Server`
- **Technology:** `Warp` (HTTP) + `WebSockets`
- **Protocol:** JSON over WebSocket
- **Update Rate:** 30Hz (~33ms)
- **Isolation:** The WebUI runs in a separate thread and is compiled only when the `enable-web-ui` flag is set. It treats the `SystemState` as read-only.

### Frontend (HTML/JS)
- **Technology:** HTML5 Canvas + Vanilla JavaScript
- **Features:**
  - **Point Cloud Visualization:** Top-down view (X-Z plane) of the radar data.
  - **Kalman Target:** Visual indicator of the filtered patient position.
  - **Beam Status:** Color-coded (Red/Green/Yellow) indicator of the radiation beam state.
  - **Metrics:** Real-time display of position coordinates, velocity, and frame timing.
- **Assets:** The `index.html` file is embedded directly into the binary using `file-embed`, ensuring a single-file deployment.

## Usage

### Building

To enable the Web UI, you must compile with the `enable-web-ui` flag:

```bash
cabal build --flags=enable-web-ui
```

### Running

Run the executable with the flag enabled:

```bash
cabal run sgrt-radar-system-exe --flags=enable-web-ui
```

The system will log:
```
Web UI started on port 8080
```

### Accessing

Open a web browser and navigate to:
[http://localhost:8080](http://localhost:8080)

## Security Considerations

*   **Authentication:** None. The dashboard is intended for use within a secure, isolated clinical network (LAN).
*   **Encryption:** The server uses plain HTTP/WS. For remote access, it should be proxied behind Nginx/Apache with SSL/TLS.
*   **Impact on Safety:** The WebUI thread runs at a lower priority than the critical `watchdog` and `ingestion` threads. However, network congestion could theoretically impact system latency if not isolated.

## JSON API (WebSocket)

The server streams a JSON object representing the `SystemState`:

```json
{
  "currentPoints": [
    { "x": 1.2, "y": 0.5, "z": 2.1, "v": 0.0, "snr": 150 }
  ],
  "beamState": "BeamOff",
  "kalmanState": {
    "x": [1.2, 0.0, 0.0]
  },
  "lastFrameTime": 123456789
}
```
