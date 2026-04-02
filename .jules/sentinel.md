## 2024-03-30 - [CRITICAL] **Vector:** [app/Control/WebUI.hs] **Hazard:** [Web server binding to all network interfaces]
**Vulnerability:** The Warp web server for the medical system's UI was bound to 0.0.0.0 (all interfaces) by default.
**Learning:** `Warp.run` defaults to binding to all interfaces, exposing local control panels to the entire network without authentication.
**Prevention:** Always use `Warp.runSettings` with `setHost "127.0.0.1"` when the UI is intended only for local/loopback access.
## 2024-03-31 - [HIGH] **Vector:** [app/Control/WebUI.hs] **Hazard:** [Insecure Content-Security-Policy with 'unsafe-inline']
**Vulnerability:** The Content-Security-Policy (CSP) allowed 'unsafe-inline' for both scripts and styles, which makes the UI vulnerable to Cross-Site Scripting (XSS) attacks.
**Learning:** Even internal or local UIs should employ strict CSPs to adhere to defense-in-depth principles. Using 'unsafe-inline' completely bypasses the protections CSP is designed to provide against malicious code injection.
**Prevention:** Always calculate SHA256 hashes of inline scripts and styles and include them in the CSP header instead of using 'unsafe-inline'. For static assets embedded at compile time (like `indexHtml`), these hashes can be pre-calculated and hardcoded safely.
## 2024-04-01 - [CRITICAL] **Vector:** [src/Hardware/Control.hs] **Hazard:** [Path Traversal in configPath]
**Vulnerability:** The `configureSensor` function accepted arbitrary configuration paths without checking for absolute paths or directory traversal sequences (`..`).
**Learning:** Functions dealing with raw `FilePath` arguments representing file locations can be manipulated into reading sensitive files from the system if `..` traversal or absolute paths are used by an attacker.
**Prevention:** Implement an `isPathSafe` check before performing any file operations. Ensure the path is not absolute and does not contain `..` to effectively restrict access to specific, allowed files/directories.
## 2024-04-02 - [HIGH] **Vector:** [app/Control/WebUI.hs] **Hazard:** [Cross-Site WebSocket Hijacking (CSWSH)]
**Vulnerability:** The `wsApp` function allowed any website to establish a WebSocket connection via the browser and read sensitive radar data because it lacked an `Origin` header check.
**Learning:** `acceptRequest` accepts any incoming connection by default. This makes the system susceptible to Cross-Site WebSocket Hijacking, a form of CSRF where an attacker can interact with the local loopback WebSocket server from an untrusted site to exfiltrate safety-critical data.
**Prevention:** Always validate the `Origin` header using `Network.WebSockets.requestHeaders` and `pendingRequest` before calling `acceptRequest`. Connections from untrusted origins must be actively rejected with `rejectRequest`.
