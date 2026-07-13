**4\. Layer 3: The MIMO Surface Mesher (2.5D Polynomial Fitting)**

This layer represents the core algorithmic innovation of the system. The raw radar point cloud is sparse (\~100 points) and noisy ("sparkle"). Direct visualization of these points (e.g., via Delaunay triangulation) would result in a chaotic, flickering mesh that is clinically unusable.  
Instead, we treat the patient's torso as a continuous mathematical function: a 2.5D Surface.

### **4.1 The Mathematical Model**

We assume the patient's chest and abdomen form a smooth, convex manifold where height ($Z$) is a function of lateral position ($X, Y$). A Bi-Quadratic Polynomial provides an excellent approximation of human torso topology:

$$Z(x,y) \= c\_0 \+ c\_1 x \+ c\_2 y \+ c\_3 x^2 \+ c\_4 y^2 \+ c\_5 xy$$

* $c\_0$: Mean height (DC offset).  
* $c\_1, c\_2$: Slope/Tilt (Planar components).  
* $c\_3, c\_4$: Curvature (Quadratic components, representing the chest arch).  
* $c\_5$: Twist.

### **4.2 The Least Squares Solution**

To find the coefficient vector $\\mathbf{c} \= \[c\_0, c\_1, c\_2, c\_3, c\_4, c\_5\]^T$, we employ the method of Linear Least Squares. We want to minimize the sum of squared differences between the radar-measured $z\_i$ and the model-predicted $Z(x\_i, y\_i)$.  
For $N$ points in the ROI, we construct the Design Matrix $A$ (size $N \\times 6$) and the observation vector $\\mathbf{b}$ (size $N \\times 1$):

$$A \= \\begin{bmatrix} 1 & x\_1 & y\_1 & x\_1^2 & y\_1^2 & x\_1 y\_1 \\\\ 1 & x\_2 & y\_2 & x\_2^2 & y\_2^2 & x\_2 y\_2 \\\\ \\vdots & \\vdots & \\vdots & \\vdots & \\vdots & \\vdots \\\\ 1 & x\_N & y\_N & x\_N^2 & y\_N^2 & x\_N y\_N \\end{bmatrix}, \\quad \\mathbf{b} \= \\begin{bmatrix} z\_1 \\\\ z\_2 \\\\ \\vdots \\\\ z\_N \\end{bmatrix} $$The optimal coefficients $\\mathbf{c}$ are found by solving the Normal Equation:$$ A^T A \\mathbf{c} \= A^T \\mathbf{b} \\quad \\implies \\quad \\mathbf{c} \= (A^T A)^{-1} A^T \\mathbf{b} $$

### 4.3 Implementation with \`hmatrix\` In Haskell, the \*\*\`hmatrix\`\*\* library provides a high-performance interface to standard BLAS/LAPACK routines (like \`dgels\` or \`dgesvd\`). \`\`\`haskell import Numeric.LinearAlgebra fitSurface :: \-\> Vector Double fitSurface points \= let a \= fromLists \[1, x p, y p, x p^2, y p^2, x p \* y p\] | p \<- points \] b \= fromList \[ z p | p \<- points \] in a \<\\\> b \-- The least squares solver operator \`\`\` The \`\<|\>\` operator automatically handles the over-determined system solver. This approach transforms the noise problem into a statistics problem. Random radar "sparkle" (outliers) will have high residuals but will not significantly skew the global surface fit, effectively acting as a powerful, zero-latency spatial low-pass filter.\[13, 14\]

### 4.4 Mesh Generation Once the coefficients $\\mathbf{c}$ are computed, the system generates a \*\*Virtual Mesh\*\*. We define a fixed $20 \\times 20$ grid of nodes $(u, v)$ spanning the ROI. For each node, we evaluate the polynomial: $$Z\_{mesh}(u,v) \= c\_0 \+ c\_1 u \+ \\dots$$ This generates a set of 400 stable vertices that form a smooth "breathing blanket." This mesh is what is visualized and used for gating, providing a stable, jitter-free representation of the patient's respiration. \---


## Explicit Software Unit Interfaces and Failure Boundaries

### FR-DSP-001: Static Clutter Removal
- **Module:** `SignalProcessing.FMCW`
- **Interfaces:** The unit exposes typed functional interfaces corresponding to Static Clutter Removal, completely decoupled from upstream IO.
- **Failure Boundaries:** Invalid state transitions will trigger the watchdog. Errors are bounded to the local STM transaction.

### FR-DSP-002: Phase Unwrapping
- **Module:** `SignalProcessing.FMCW`
- **Interfaces:** The unit exposes typed functional interfaces corresponding to Phase Unwrapping, completely decoupled from upstream IO.
- **Failure Boundaries:** Invalid state transitions will trigger the watchdog. Errors are bounded to the local STM transaction.

### FR-DSP-003: Kalman filter for motion prediction
- **Module:** `SignalProcessing.Kalman`
- **Interfaces:** The unit exposes typed functional interfaces corresponding to Kalman filter for motion prediction, completely decoupled from upstream IO.
- **Failure Boundaries:** Invalid state transitions will trigger the watchdog. Errors are bounded to the local STM transaction.

### FR-DSP-004: Phase Extraction
- **Module:** `SignalProcessing.FMCW`
- **Interfaces:** The unit exposes typed functional interfaces corresponding to Phase Extraction, completely decoupled from upstream IO.
- **Failure Boundaries:** Invalid state transitions will trigger the watchdog. Errors are bounded to the local STM transaction.

### PR-ACC-01: Correlation coefficient > 0.95 vs ground truth
- **Module:** `SignalProcessing.Kalman`, `SignalProcessing.FMCW`
- **Interfaces:** The unit exposes typed functional interfaces corresponding to Correlation coefficient > 0.95 vs ground truth, completely decoupled from upstream IO.
- **Failure Boundaries:** Invalid state transitions will trigger the watchdog. Errors are bounded to the local STM transaction.

### MR-001: FMCW Range Estimation (Equation 1)
- **Module:** `SignalProcessing.FMCW`
- **Interfaces:** The unit exposes typed functional interfaces corresponding to FMCW Range Estimation (Equation 1), completely decoupled from upstream IO.
- **Failure Boundaries:** Invalid state transitions will trigger the watchdog. Errors are bounded to the local STM transaction.

### MR-002: Chirp Z-Transform (Equation 2)
- **Module:** `SignalProcessing.FMCW`
- **Interfaces:** The unit exposes typed functional interfaces corresponding to Chirp Z-Transform (Equation 2), completely decoupled from upstream IO.
- **Failure Boundaries:** Invalid state transitions will trigger the watchdog. Errors are bounded to the local STM transaction.

### MR-003: Standard DFT (Equation 3)
- **Module:** None
- **Interfaces:** The unit exposes typed functional interfaces corresponding to Standard DFT (Equation 3), completely decoupled from upstream IO.
- **Failure Boundaries:** Invalid state transitions will trigger the watchdog. Errors are bounded to the local STM transaction.

### MR-004: Phase Extraction (Equation 4)
- **Module:** `SignalProcessing.FMCW`
- **Interfaces:** The unit exposes typed functional interfaces corresponding to Phase Extraction (Equation 4), completely decoupled from upstream IO.
- **Failure Boundaries:** Invalid state transitions will trigger the watchdog. Errors are bounded to the local STM transaction.

### MR-005: Displacement Calculation (Equation 5)
- **Module:** `SignalProcessing.FMCW`
- **Interfaces:** The unit exposes typed functional interfaces corresponding to Displacement Calculation (Equation 5), completely decoupled from upstream IO.
- **Failure Boundaries:** Invalid state transitions will trigger the watchdog. Errors are bounded to the local STM transaction.
