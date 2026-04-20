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
## 2024-04-03 - [MEDIUM] **Vector:** [src/Safety/Audit.hs] **Hazard:** [Log Injection / CRLF Injection]
**Vulnerability:** Untrusted string fields in `AuditEvent` (`component` and `message`) were written directly to the log file via `printf` without sanitization. If an attacker controls these fields and injects newline characters, they can forge fake log entries (e.g. Critical severity events) leading to log spoofing and obfuscation.
**Learning:** Append-only log files that rely on single-line formats must explicitly strip or escape control characters (like `\n` and `\r`) from any variable input before writing.
**Prevention:** Always sanitize strings before logging them, for example by mapping over the characters and replacing those matching `isControl` with spaces.
## 2024-04-04 - [MEDIUM] Add Cache-Control headers to prevent UI state caching
**Vulnerability:** The WebUI response lacked `Cache-Control` headers, allowing browsers to potentially cache the sensitive medical UI.
**Learning:** Even for local single-page apps, browsers may cache the HTML, which could lead to stale or sensitive state being exposed via the browser cache or back button.
**Prevention:** Always include `Cache-Control: no-store, no-cache, must-revalidate, max-age=0` and `Pragma: no-cache` for UIs displaying sensitive, real-time data to ensure no disk or memory caching occurs.
## 2024-04-05 - [MEDIUM] **Vector:** [app/Control/WebUI.hs] **Hazard:** [WebSocket Thread Leak / DoS]
**Vulnerability:** The WebSocket loop `forever $ sendTextData ...` sent continuous data without verifying if the client connection remained alive (e.g., if a client drops ungracefully resulting in a half-open TCP connection). This can lead to indefinite blocking and thread leaks when the kernel buffer fills, eventually causing Denial of Service.
**Learning:** Raw WebSocket `forever` send loops in Haskell will leak threads if the connection drops silently, because `sendTextData` may eventually block forever waiting for the client to acknowledge.
**Prevention:** Always use `Network.WebSockets.withPingThread` to actively ping clients and automatically tear down the connection and the associated thread if the client stops responding.
## 2024-04-06 - [CRITICAL] **Vector:** [app/Control/WebUI.hs] **Hazard:** [Incomplete WebSocket Authentication]
**Vulnerability:** The WebSocket endpoint relied solely on checking the `Origin` header for authentication. This alone is insufficient and bypassable by certain clients.
**Learning:** `Origin` headers can be spoofed outside the browser context, and while necessary, they are not enough for secure session management. Cryptographically secure token-based authentication must be used alongside origin validation.
**Prevention:** Generate a random token on server start (using `/dev/urandom`), pass it to the client via an `HttpOnly`, `SameSite=Strict` cookie, and explicitly verify this cookie on the incoming WebSocket request headers before calling `acceptRequest`.
## 2024-04-07 - [HIGH] **Vector:** [app/Control/WebUI.hs] **Hazard:** [Insecure Session Management]
**Vulnerability:** The session cookie generated for WebSocket authentication lacked the `Secure` flag.
**Learning:** Cookies without the `Secure` flag can be transmitted over unencrypted HTTP connections, making them susceptible to interception via Man-in-the-Middle (MitM) attacks. While the system operates on localhost, omitting this flag violates defense-in-depth principles and poses a risk if network configurations change.
**Prevention:** Always include the `Secure` flag in `Set-Cookie` headers for sensitive tokens, in addition to `HttpOnly` and `SameSite=Strict`.
## 2024-04-20 - [CRITICAL] **Vector:** [app/Main.hs] **Hazard:** [TOCTOU in Device Port Validation]
**Vulnerability:** The system checked if the sensor and CLI ports were character devices using their paths (`getFileStatus`) before opening them with `openFd`. This created a Time-of-Check to Time-of-Use race condition where the underlying file could be replaced between validation and use.
**Learning:** Checking a file path's status and then opening it separately is fundamentally unsafe in POSIX environments, as the filesystem mapping can change between the operations.
**Prevention:** To prevent TOCTOU vulnerabilities when accessing device files or performing security checks, always open the file descriptor first (`openFd`), and then validate its status using `System.Posix.Files.getFdStatus`.
