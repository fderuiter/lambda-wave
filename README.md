# Surface Guided Radiation Therapy (SGRT) System

## Project Overview
This is a safety-critical SGRT system designed to interface with the Texas Instruments IWR6843ISK mmWave radar sensor. It performs real-time patient motion tracking and radiation beam gating.

## System Architecture
*   **Core Logic (Haskell):** Control Plane (Gating, Kalman Filter) and Data Plane (Signal Processing).
*   **Hardware Interface (C/C++):** Low-level ingestion via Pinned Memory Ring Buffer and FFI.
*   **Sensor:** Texas Instruments IWR6843ISK.

## Prerequisites
*   **Hardware:** TI IWR6843ISK Sensor + XDS110 USB-to-UART.
*   **Software:** Docker, Haskell Stack/Cabal.

## Quick Start (Simulation)
1.  Clone the repository.
2.  Run `docker build -t sgrt-system .`
3.  Run `docker run sgrt-system`

## Running Tests
To run the test suite:
```bash
cabal test
```

## Hardware Operation
1.  Connect the TI IWR6843ISK sensor via USB.
2.  Ensure drivers are installed.
3.  Configure `.env` with correct ports (e.g., `/dev/ttyUSB0`).
4.  Run the application.

See `docs/architecture.md` for detailed documentation.
