# Verified Mathematical Framework for mmWave Radiotherapy Setup

**Author:** Analysis of Bressler et al. (Med. Phys. 2024)
**Date:** Today

## Abstract

This document provides a verified breakdown of the mathematical models used in *Millimeter wave-based patient setup verification and motion tracking during radiotherapy*. The formulas have been cross-referenced with standard radar theory to correct typographical errors found in the original manuscript's text extraction.

---

## 1. Core Radar Principles

### 1.1 FMCW Range Estimation

The fundamental operation of Frequency-Modulated Continuous Wave (FMCW) radar relies on mixing the transmitted "chirp" with the received reflection. The resulting Intermediate Frequency (IF) signal contains a beat frequency $\hat{f}_{FFT}$ proportional to the target range $R$.

**Equation (1): Verified**

$$
\hat{f}_{FFT} = \frac{2 B R}{c T}
$$

**Definitions:**
* $B$: Bandwidth of the chirp (4 GHz).
* $c$: Speed of light ($3 \times 10^8$ m/s).
* $T$: Chirp duration (Observation window).

### 1.2 Resolution Limit

The standard Fast Fourier Transform (FFT) is limited by the observation window $T$. The minimum distinguishable frequency separation is $1/T$. In terms of distance, this creates a resolution limit (Range Bin):

$$
\Delta R_{FFT} = \frac{c}{2B}
$$

*Verification:* For $B = 4$ GHz, $\Delta R = 3.75$ cm. This matches the paper's reported limitations of the standard FFT approach.

---

## 2. The Chirp Z-Transform (CZT)

To overcome the coarse resolution of the FFT, the authors utilize the Chirp Z-Transform (CZT). This allows the spectral analysis to focus on a specific frequency band ("zooming in").

### 2.1 Correction of Manuscript Equation (2)

The manuscript's text contains typographical errors in Equation (2), specifically regarding the indices ($x_s$ vs $x_n$) and the frequency stepping term ($K/K$ vs $k/K$). The corrected mathematical formulation is presented below.

**Equation (2): Corrected & Verified**

$$
X_{k, CZT} = \sum_{n=0}^{N-1} x_n e^{-i 2\pi n \left( \frac{f_0 + B \frac{k}{K}}{f_s} \right)}
$$

**Correction Notes:**
* **Input Signal:** Changed $x_s$ (typo) to $x_n$, representing the $n$-th sample of the time-domain IF signal.
* **Frequency Step:** Changed $B \frac{K}{K}$ (typo) to $B \frac{k}{K}$, where $k$ is the output frequency bin index ($0 \le k < K$).
* **Physics:** The term inside the exponent represents the phase at time sample $n$ for the specific frequency $f_k = f_0 + B(k/K)$.

### 2.2 Standard DFT Comparison

For context, the standard Discrete Fourier Transform (Equation 3 in the paper) is provided below. This scans the entire unit circle rather than a specific arc.

$$
X_{k, DFT} = \sum_{n=0}^{N-1} x_n e^{-i \frac{2\pi n k}{N}}
$$

---

## 3. Phase-Based Motion Tracking

While CZT provides high-precision absolute distance, relative motion (displacement) is tracked using the phase of the signal, which is far more sensitive.

### 3.1 Phase Extraction

The phase $\phi$ is extracted directly from the complex output of the CZT at the peak frequency index.

**Equation (4): Verified**

$$
\phi = \angle \max \left( X_{k, CZT} \right)
$$

### 3.2 Displacement Calculation

Small displacements cause phase shifts in the reflected signal. The relationship between phase change $\Delta \phi$ and physical displacement $d$ is derived from the Doppler principle.

**Equation (5): Verified**

$$
d = \frac{c \cdot \Delta \phi}{4\pi f_{min}} = \frac{\lambda_{min} \cdot \Delta \phi}{4\pi}
$$

**Verification of Constants:**
* $f_{min}$: The starting frequency of the chirp (77 GHz).
* $4\pi$: This factor correctly accounts for the round-trip path of the radar signal. A displacement of $\lambda/2$ results in a round-trip path change of $\lambda$, which corresponds to a full $2\pi$ phase cycle.

---

## 4. Summary of Validated Algorithm

1. **Coarse Search:** Perform FFT to find the approximate range peak frequency $f_{FFT}$.
2. **Fine Search (Zoom 1):** Perform CZT centered at $f_{FFT}$ with bandwidth $2F_s/K$.
3. **Fine Search (Zoom 2):** Perform CZT centered at the new peak with bandwidth $2F_s/K^2$.
4. **Displacement:** Extract phase from the final CZT peak and unwrap it over time to track motion $d$.

---

## 5. The "Fonzi" Formula Stack (2025 Upgrade)

Based on recent research (Rong et al., 2025; arXiv 2025), the system is upgraded with an advanced processing chain to handle "Human Shape Geometric Error" (smearing) and improve resolution.

### 5.1 Step 1: The "Zoom" (Bluestein CZT)

The Chirp Z-Transform is implemented using **Bluestein's Algorithm** for $O((N+K) \log (N+K))$ efficiency, enabling millimeter-level pixel resolution.

$$
X_k = \sum_{n=0}^{N-1} x(n) A^{-n} W^{nk}
$$

Where:
* $A = A_0 e^{j 2\pi \theta_0}$ (Start frequency)
* $W = W_0 e^{j 2\pi \phi_0}$ (Step size / resolution)

### 5.2 Step 2: The "Motion" (Phase Extraction)

Phase is extracted using the In-phase (I) and Quadrature (Q) components:

$$
\phi[n] = \arctan\left(\frac{Q[n]}{I[n]}\right)
$$

### 5.3 Step 3: The "Unwrapping" (Differential Phase)

To track breathing motion larger than the wavelength ($\approx 3.9$ mm), a differential unwrapper is applied:

$$
\Delta \phi[n] = \phi[n] - \phi[n-1]
$$
*Logic Check:* If $|\Delta \phi| > \pi$, adjust by $\pm 2\pi$.

**Final Displacement:**
$$
D[n] = D[n-1] + \frac{\lambda}{4\pi} \Delta \phi[n]
$$

### 5.4 Step 4: The "Targeting" (MIMO Beamforming)

Digital Beamforming is used to focus the radar beam on specific reflection points (e.g., sternum vs. abdomen), acting as a "Digital Lens".

$$
y(t) = \sum_{k=1}^{M} w_k \cdot x_k(t) \cdot e^{-j \frac{2\pi}{\lambda} d_k \sin(\theta)}
$$

* $M$: Number of antennas.
* $w_k$: Tapering weights.
* $\theta$: Steering angle.
* $d_k$: Antenna position (distance).
