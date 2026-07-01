# Mathematical Equations and Constants Reference

## Physical Constants

*   **Speed of Light ($c$):** $3 \times 10^8 \text{ m/s}$

## Radar Equations

### FMCW Range Resolution
The fundamental range resolution ($\Delta R$) depends on the total bandwidth ($B$):
$$ \Delta R = \frac{c}{2B} $$

### Phase-Based Tracking Distance
Distance ($d$) based on phase change ($\Delta \phi$):
$$ d = \frac{\lambda_{min} \cdot \Delta \phi}{4\pi} = \frac{c}{4\pi f_{min}} \cdot \Delta \phi $$

## Surface Modeling

### Bi-Quadratic Polynomial
$$ Z(x,y) = c_0 + c_1 x + c_2 y + c_3 x^2 + c_4 y^2 + c_5 xy $$

### Linear Least Squares Normal Equation
Given design matrix $A$ and observation vector $\mathbf{b}$:
$$ A^T A \mathbf{c} = A^T \mathbf{b} \quad \implies \quad \mathbf{c} = (A^T A)^{-1} A^T \mathbf{b} $$

### Design Matrix ($A$)
$$ A = \begin{bmatrix} 1 & x_1 & y_1 & x_1^2 & y_1^2 & x_1 y_1 \\ 1 & x_2 & y_2 & x_2^2 & y_2^2 & x_2 y_2 \\ \vdots & \vdots & \vdots & \vdots & \vdots & \vdots \\ 1 & x_N & y_N & x_N^2 & y_N^2 & x_N y_N \end{bmatrix}, \quad \mathbf{b} = \begin{bmatrix} z_1 \\ z_2 \\ \vdots \\ z_N \end{bmatrix} $$

## Filtering & Tracking

### Constant Velocity Kalman Filter State Matrix
State vector ($\mathbf{x}_k$):
$$ \mathbf{x}_k = [ \text{position}, \text{velocity} ]^T $$
Prediction step matrix:
$$ \hat{\mathbf{x}}_{k|k-1} = \begin{bmatrix} 1 & \Delta t \\ 0 & 1 \end{bmatrix} \hat{\mathbf{x}}_{k-1} $$
Gating Signal calculation:
$$ \text{GatingSignal} = \text{position}_k + \text{velocity}_k \times 0.05 \text{s} $$
