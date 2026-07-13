## 5\. Layer 4: The Gating Logic (The Brain) This layer translates the 3D surface model into a binary "Beam ON/OFF" decision.

### 5.1 Signal Extraction Gating requires a 1D signal: the "Breathing Amplitude." In optical marker systems, this is the Z-height of a specific plastic box. In this surface-based system, we calculate the \*\*Weighted Average Height\*\* of the virtual mesh. $$V\_{resp}(t) \= \\frac{1}{\\text{Area}} \\iint\_{ROI} Z\_{mesh}(x,y,t) \\,dx\\,dy$$ By integrating over the polynomial surface (or averaging the 400 mesh points), we further improve Signal-to-Noise Ratio (SNR). If one part of the chest flickers up and another down due to noise, the average remains stable.

### 5.2 Kalman Filtering and Latency Compensation The signal $V\_{resp}(t)$ represents the \*current\* state. However, the LINAC beam has a hardware latency. When the software sends the "Beam Off" command, it may take 50-100ms for the radiation to actually cease. In high-dose rate delivery, this lag can result in significant overdose to healthy tissue if the patient coughs. To compensate, we implement a \*\*Constant Velocity Kalman Filter\*\*.\[15, 16\] \* \*\*State Vector\*\*: $\\mathbf{x}\_k \= \[ \\text{position}, \\text{velocity} \]^T$. \* \*\*Prediction Step\*\*: Project the state forward based on Newtonian physics. $$\\hat{\\mathbf{x}}\_{k|k-1} \= \\begin{bmatrix} 1 & \\Delta t \\\\ 0 & 1 \\end{bmatrix} \\hat{\\mathbf{x}}\_{k-1}$$ \* \*\*Update Step\*\*: Correct the prediction using the new measurement $V\_{resp}(t)$. \* \*\*Forecasting\*\*: We calculate the predicted position at time $t \+ \\text{Latency}$. $$\\text{GatingSignal} \= \\text{position}\_k \+ \\text{velocity}\_k \\times 0.05 \\text{s}$$ This allows the system to trigger the beam-hold \*before\* the patient exits the tolerance window, effectively neutralizing the hardware lag.

### 5.3 Hysteresis Logic To prevent "chattering" (rapid ON/OFF switching) when the breathing amplitude hovers near the threshold, we employ a \*\*Schmidt Trigger (Hysteresis)\*\*. \* \*\*Beam ON Condition\*\*: Signal \> (Threshold \+ Tolerance). \* \*\*Beam OFF Condition\*\*: Signal \< (Threshold \- Tolerance). This creates a "dead zone" where the beam state is maintained, ensuring that the beam is only active when the patient is decisively within the breath-hold window. \---


## Explicit Software Unit Interfaces and Failure Boundaries

### FR-GAT-001: Automatic beam gating
- **Module:** `Control.Gating`
- **Interfaces:** The unit exposes typed functional interfaces corresponding to Automatic beam gating, completely decoupled from upstream IO.
- **Failure Boundaries:** Invalid state transitions will trigger the watchdog. Errors are bounded to the local STM transaction.

### FR-GAT-002: Total latency < 50ms
- **Module:** `Control.Gating`, `Main`
- **Interfaces:** The unit exposes typed functional interfaces corresponding to Total latency < 50ms, completely decoupled from upstream IO.
- **Failure Boundaries:** Invalid state transitions will trigger the watchdog. Errors are bounded to the local STM transaction.
