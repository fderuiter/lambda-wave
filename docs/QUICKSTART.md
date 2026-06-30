# Lambda-Wave Quick Start Guide

**Goal:** Get Lambda-Wave running in 15 minutes or less

---

## Table of Contents

1. [Prerequisites Check](#prerequisites-check)
2. [Installation Methods](#installation-methods)
3. [Verification](#verification)
4. [Next Steps](#next-steps)
5. [Troubleshooting](#troubleshooting)

---

## Prerequisites Check

Before starting, verify you have:

### For Docker Method (Easiest)
- [ ] Docker installed and running
- [ ] 4GB+ RAM available
- [ ] 10GB+ disk space

```bash
# Verify Docker
docker --version  # Should show Docker version 20.10+
docker run hello-world  # Should complete successfully
```

### For Native Method
- [ ] Linux, macOS, or Windows WSL2
- [ ] 8GB+ RAM
- [ ] 15GB+ disk space for dependencies

---

## Installation Methods

Choose one method based on your needs:

### Method 1: Docker (Recommended for First-Time Users)

**Best for:** Quick evaluation, simulation mode, consistent environment

```bash
# Step 1: Clone repository
git clone https://github.com/fderuiter/lambda-wave.git
cd lambda-wave

# Step 2: Build Docker image (this takes 5-10 minutes)
docker build -t lambda-wave:latest .

# Step 3: Run simulation
docker run -it lambda-wave:latest

# You should see:
# Initializing Lambda-Wave System...
# Configuration: Sensor=/dev/ttyUSB0, CLI=/dev/ttyUSB1
# System Armed. Starting UI...
```

**Expected Output:**
```
Initializing Lambda-Wave System...
Configuration: Sensor=/dev/ttyUSB0, CLI=/dev/ttyUSB1
[INFO] Ring Buffer created: 4MB
[INFO] Consumer loop started
[INFO] Watchdog armed: timeout=100ms
[INFO] Audit logging to: session.log
System Armed. Starting UI...
```

---

### Method 2: Native Build (Ubuntu/Debian)

**Best for:** Development, performance, native hardware access

```bash
# Step 1: Install system dependencies
sudo apt-get update
sudo apt-get install -y \
    ghc cabal-install \
    g++ \
    libgl1-mesa-dev \
    libglu1-mesa-dev \
    freeglut3-dev \
    libusb-1.0-0-dev \
    pkg-config

# Step 2: Clone repository
git clone https://github.com/fderuiter/lambda-wave.git
cd lambda-wave

# Step 3: Update Cabal package list
cabal update

# Step 4: Build dependencies (10-30 minutes on first run)
cabal build --only-dependencies

# Step 5: Build project
cabal build

# Step 6: Run application
cabal run sgrt-radar-system-exe
```

**Time Estimate:**
- System deps: 2-5 minutes
- Cabal deps: 10-30 minutes (first time only)
- Build: 2-5 minutes
- **Total:** 15-40 minutes

---

### Method 3: Native Build (macOS)

**Best for:** macOS users, development

```bash
# Step 1: Install Homebrew (if not already installed)
/bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"

# Step 2: Install dependencies
brew install ghc cabal-install
brew install pkg-config
brew install glew glfw3

# Step 3: Clone and build (same as Linux)
git clone https://github.com/fderuiter/lambda-wave.git
cd lambda-wave
cabal update
cabal build --only-dependencies
cabal build
cabal run sgrt-radar-system-exe
```

---

### Method 4: Windows (WSL2)

**Best for:** Windows users

```bash
# Step 1: Install WSL2 with Ubuntu
wsl --install -d Ubuntu-20.04

# Step 2: Inside WSL, follow Ubuntu instructions (Method 2)
# ...
```

---

## Hardware Setup (Optional)

**Required Hardware:**
- Texas Instruments IWR6843ISK mmWave evaluation kit
- 2x Micro-USB cables (data + power)
- USB hub (recommended)

### Physical Connection

```
                    ┌──────────────────┐
                    │   IWR6843ISK     │
                    │                  │
  USB Data ────────▶│  Data Port       │
                    │  (micro-USB)     │
  USB Config ──────▶│  CLI Port        │
                    │  (micro-USB)     │
                    └──────────────────┘
                            │
                            ▼
                    ┌──────────────────┐
                    │  Your Computer   │
                    │  /dev/ttyUSB0    │
                    │  /dev/ttyUSB1    │
                    └──────────────────┘
```

### Software Configuration

```bash
# Step 1: Verify ports are detected
ls -l /dev/ttyUSB*
# Should show:
# /dev/ttyUSB0  <- Data port
# /dev/ttyUSB1  <- Config port

# Step 2: Add user to dialout group (one-time setup)
sudo usermod -a -G dialout $USER
# Log out and back in for this to take effect

# Step 3: Set environment variables
export SGRT_SENSOR_PORT=/dev/ttyUSB0
export SGRT_CLI_PORT=/dev/ttyUSB1

# Step 4: Run with hardware
cabal run sgrt-radar-system-exe

# OR with Docker:
docker run -it \
    --device=/dev/ttyUSB0 \
    --device=/dev/ttyUSB1 \
    -e SGRT_SENSOR_PORT=/dev/ttyUSB0 \
    -e SGRT_CLI_PORT=/dev/ttyUSB1 \
    lambda-wave:latest
```

### Verify Hardware Connection

```bash
# Test data port
cat /dev/ttyUSB0
# Should show binary data stream if sensor is running

# Test config port (send command)
echo "version" > /dev/ttyUSB1
cat /dev/ttyUSB1
# Should show firmware version
```

---

## Verification

### 1. Build Verification

```bash
# Verify build succeeded
cabal build
# Should end with: "Build succeeded"

# Check executable exists
cabal list-bin sgrt-radar-system-exe
# Should show path to executable
```

### 2. Test Suite Verification

```bash
# Run all tests
cabal test
# Should show:
# Test suite sgrt-radar-system-test: PASSED
# All tests passed

# Verify specific tests
cabal test --test-show-details=direct
# Should show individual test results
```

### 3. Runtime Verification (Simulation)

```bash
# Run in simulation mode
cabal run sgrt-radar-system-exe

# Expected output:
# [OK] Initializing Lambda-Wave System...
# [OK] Ring Buffer created: 4MB
# [OK] Consumer loop started
# [OK] Watchdog armed
# [OK] Audit logging started
# [OK] System Armed. Starting UI...
```

### 4. Hardware Verification (if applicable)

```bash
# With sensor connected, you should see:
# [OK] Sensor configuration loaded: config/ti_iwr6843isk/sgrt_profile.cfg
# [OK] Sensor initialized
# [OK] Receiving frames: 60 Hz
# [OK] Point cloud: 128 points
```

---

## Next Steps

### For Developers

1. **Read Developer Guide:** [docs/DEVELOPER_GUIDE.md](DEVELOPER_GUIDE.md)
2. **Explore Codebase:**
   ```bash
   # View source structure
   tree src/
   
   # Read key modules
   cat src/Control/Gating.hs
   cat src/SignalProcessing/FMCW.hs
   ```

3. **Make a Change:**
   ```bash
   git checkout -b feature/my-feature
   # Edit code
   cabal test
   git commit -m "feat: My feature"
   ```

4. **Read Contributing Guide:** [CONTRIBUTING.md](../CONTRIBUTING.md)

### For Users/Evaluators

1. **Read Purpose & Architecture:** [docs/PURPOSE_AND_ARCHITECTURE.md](PURPOSE_AND_ARCHITECTURE.md)
2. **Review Project Status:** [docs/PROJECT_STATUS.md](PROJECT_STATUS.md)
3. **Run Performance Benchmarks:**
   ```bash
   cabal bench
   ```

### For Medical Physicists

1. **Review Mathematical Framework:** [docs/mathematical_framework.md](mathematical_framework.md)
2. **Understand Safety Systems:** [docs/PURPOSE_AND_ARCHITECTURE.md#safety--compliance](PURPOSE_AND_ARCHITECTURE.md#safety--compliance)
3. **Hardware Validation:**
   ```bash
   # Connect motion phantom
   # Run system and compare with ground truth
   ```

---

## Troubleshooting

### Problem: Docker build fails

**Symptom:**
```
ERROR: failed to solve: ...
```

**Solutions:**
```bash
# 1. Update Docker
sudo apt-get update
sudo apt-get upgrade docker-ce

# 2. Clean Docker cache
docker system prune -a

# 3. Increase Docker memory limit (Docker Desktop)
# Settings -> Resources -> Memory: 4GB+

# 4. Try build with no cache
docker build --no-cache -t lambda-wave .
```

---

### Problem: "Could not resolve dependencies"

**Symptom:**
```
cabal: Could not resolve dependencies:
[__0] trying: sgrt-radar-system-<!-- METADATA:project_version -->0.1.0.0<!-- /METADATA:project_version --> (user goal)
[__1] unknown package: vector
```

**Solutions:**
```bash
# 1. Update package index
cabal update

# 2. Clean build
rm -rf dist-newstyle/
cabal clean

# 3. Try with new index
rm -rf ~/.cabal/packages/
cabal update
cabal build --only-dependencies
```

---

### Problem: OpenGL not found

**Symptom:**
```
Missing C library: GL
```

**Solutions:**

**Ubuntu/Debian:**
```bash
sudo apt-get install -y \
    libgl1-mesa-dev \
    libglu1-mesa-dev \
    freeglut3-dev
```

**macOS:**
```bash
brew install glew glfw3
```

**Verify:**
```bash
pkg-config --cflags --libs gl glu glut
```

---

### Problem: Permission denied /dev/ttyUSB0

**Symptom:**
```
Permission denied: /dev/ttyUSB0
```

**Solution:**
```bash
# Add user to dialout group
sudo usermod -a -G dialout $USER

# Log out and log back in
# Then verify:
groups
# Should include "dialout"

# Alternative (temporary, not recommended):
sudo chmod 666 /dev/ttyUSB0
```

---

### Problem: Sensor not detected

**Symptom:**
```
ls /dev/ttyUSB*
ls: cannot access '/dev/ttyUSB*': No such file or directory
```

**Solutions:**
```bash
# 1. Check USB connection
lsusb
# Should show TI device

# 2. Load FTDI driver (if needed)
sudo modprobe ftdi_sio

# 3. Check dmesg for errors
dmesg | tail -20
```

---

### Problem: Build takes too long

**Symptom:**
Cabal build stuck at "Building dependencies..."

**Solutions:**
```bash
# 1. Use parallel builds
cabal build -j$(nproc)

# 2. Increase GHC memory
cabal build --ghc-options="+RTS -M4G -RTS"

# 3. Use Docker (pre-built dependencies)
docker build -t lambda-wave .
```

---

### Problem: Tests fail

**Symptom:**
```
Test suite sgrt-radar-system-test: FAILED
```

**Solutions:**
```bash
# 1. Run with verbose output
cabal test --test-show-details=direct

# 2. Run specific failing test
cabal test --test-options="-m <test-name>"

# 3. Check if it's a known issue
# See: https://github.com/fderuiter/lambda-wave/issues
```

---

## Getting Help

### Documentation
- [Build Guide](BUILD_GUIDE.md) - Detailed build instructions
- [Developer Guide](DEVELOPER_GUIDE.md) - Code structure and workflow
- [Project Status](PROJECT_STATUS.md) - Known issues and roadmap

### Community
- **GitHub Issues:** https://github.com/fderuiter/lambda-wave/issues
- **GitHub Discussions:** https://github.com/fderuiter/lambda-wave/discussions
- **Maintainer:** Frederick de Ruiter ([@fderuiter](https://github.com/fderuiter))
- **Email:** fpderuiter@gmail.com

### Before Asking for Help

Please include:
1. Operating system and version
2. Installation method (Docker/Native)
3. Full error message
4. Steps to reproduce
5. Output of:
   ```bash
   ghc --version
   cabal --version
   docker --version
   ```

---

## Success Checklist

- [ ] Repository cloned
- [ ] Dependencies installed
- [ ] Build completed successfully
- [ ] Tests pass
- [ ] Application runs (simulation or hardware)
- [ ] Output matches expected behavior

**If all checked:** 🎉 You're ready! Proceed to [Developer Guide](DEVELOPER_GUIDE.md) or [Project Status](PROJECT_STATUS.md)

**If stuck:** See [Troubleshooting](#troubleshooting) or [Getting Help](#getting-help)

---

**Last Updated:** January 2026  
**Estimated Time:** 15-40 minutes (depending on method)
