## 2024-03-30 - [CRITICAL] **Vector:** [app/Control/WebUI.hs] **Hazard:** [Web server binding to all network interfaces]
**Vulnerability:** The Warp web server for the medical system's UI was bound to 0.0.0.0 (all interfaces) by default.
**Learning:** `Warp.run` defaults to binding to all interfaces, exposing local control panels to the entire network without authentication.
**Prevention:** Always use `Warp.runSettings` with `setHost "127.0.0.1"` when the UI is intended only for local/loopback access.
## 2024-03-31 - [HIGH] **Vector:** [app/Control/WebUI.hs] **Hazard:** [Insecure Content-Security-Policy with 'unsafe-inline']
**Vulnerability:** The Content-Security-Policy (CSP) allowed 'unsafe-inline' for both scripts and styles, which makes the UI vulnerable to Cross-Site Scripting (XSS) attacks.
**Learning:** Even internal or local UIs should employ strict CSPs to adhere to defense-in-depth principles. Using 'unsafe-inline' completely bypasses the protections CSP is designed to provide against malicious code injection.
**Prevention:** Always calculate SHA256 hashes of inline scripts and styles and include them in the CSP header instead of using 'unsafe-inline'. For static assets embedded at compile time (like `indexHtml`), these hashes can be pre-calculated and hardcoded safely.
