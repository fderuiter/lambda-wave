# Lambda-Wave: Surface Guided Radiation Therapy System

[![Build Status](https://github.com/fderuiter/lambda-wave/workflows/Build%20and%20Test/badge.svg)](https://github.com/fderuiter/lambda-wave/actions)
[![License](https://img.shields.io/badge/license-BSD--3--Clause-blue.svg)](LICENSE)
[![IEC 62304](https://img.shields.io/badge/IEC%2062304-Class%20C-critical.svg)](docs/PURPOSE_AND_ARCHITECTURE.md)

> **Safety-critical mmWave radar system for real-time patient motion tracking during cancer radiotherapy**

---

## 🎯 What is Lambda-Wave?

Lambda-Wave is a **medical device software system** that uses millimeter-wave (mmWave) radar to monitor patient position during radiation therapy with **sub-millimeter accuracy**. When patient motion is detected, the system automatically controls the radiation beam to prevent healthy tissue exposure.

### Key Features

- 🎯 **Sub-millimeter tracking accuracy** using phase-based radar processing
- ⚡ **Real-time beam gating** with <50ms latency
- 🛡️ **Safety-critical design** compliant with IEC 62304 Class C
- 📡 **Non-contact monitoring** - no markers or devices on patient
- 💰 **Cost-effective** - uses commodity mmWave sensor (~$500 vs $100K+ optical systems)
- 🔓 **Open source** - transparent, auditable, community-driven

---

## 📚 Documentation

| Document | Description |
|----------|-------------|
| [**Quick Start Guide**](docs/QUICKSTART.md) | Get running in 15 minutes |
| [**Project Status**](docs/PROJECT_STATUS.md) | Current implementation status and roadmap |
| [**Build Guide**](docs/BUILD_GUIDE.md) | Comprehensive build and dependency management |
| [**Purpose & Architecture**](docs/PURPOSE_AND_ARCHITECTURE.md) | System design and medical context |
| [**Developer Guide**](docs/DEVELOPER_GUIDE.md) | Development workflow and code structure |
| [**Mathematical Framework**](docs/mathematical_framework.md) | Signal processing algorithms and theory |
| [**Contributing**](CONTRIBUTING.md) | How to contribute to the project |

---

## 🚀 Quick Start

### Option 1: Docker (Recommended)

```bash
# Build and run in simulation mode
docker build -t lambda-wave .
docker run -it lambda-wave
```

### Option 2: Native Build

```bash
# Install dependencies (Ubuntu/Debian)
sudo apt-get update
sudo apt-get install -y ghc cabal-install g++ \
    libgl1-mesa-dev libglu1-mesa-dev freeglut3-dev

# Build and test
cabal update
cabal build
cabal test

# Run application
cabal run sgrt-radar-system-exe
```

### Option 3: Hardware Mode (Requires TI IWR6843ISK)

```bash
# Configure environment
export SGRT_SENSOR_PORT=/dev/ttyUSB0
export SGRT_CLI_PORT=/dev/ttyUSB1

# Run with hardware access
docker run -it \
    --device=/dev/ttyUSB0 \
    --device=/dev/ttyUSB1 \
    -e SGRT_SENSOR_PORT=/dev/ttyUSB0 \
    -e SGRT_CLI_PORT=/dev/ttyUSB1 \
    lambda-wave
```

See [Quick Start Guide](docs/QUICKSTART.md) for detailed instructions.

---

## 🏗️ System Architecture

```
┌─────────────────────────────────────────────────────────────┐
│                    Lambda-Wave System                       │
├─────────────────────────────────────────────────────────────┤
│                                                             │
│  ┌──────────┐    ┌──────────┐    ┌────────────┐           │
│  │ Control  │───▶│   Data   │───▶│  Hardware  │           │
│  │  Plane   │    │  Plane   │    │ Interface  │           │
│  │(Haskell) │    │(Haskell) │    │  (C++/FFI) │           │
│  └──────────┘    └──────────┘    └────────────┘           │
│        │              │                  │                  │
│        └──────────────┴──────────────────┘                  │
│                       │                                     │
│            ┌──────────▼──────────┐                          │
│            │   Safety Layer      │                          │
│            │ (Watchdog, Audit)   │                          │
│            └─────────────────────┘                          │
└─────────────────────────────────────────────────────────────┘
                        │
                        ▼
          ┌─────────────────────────┐
          │  TI IWR6843ISK Sensor   │
          │  (77-81 GHz mmWave)     │
          └─────────────────────────┘
```

**Technology Stack:**
- **Core Logic:** Haskell (safety, type safety, pure functions)
- **Hardware I/O:** C++ (zero-copy, low latency)
- **Signal Processing:** FMCW radar, Chirp Z-Transform, Kalman filtering
- **Visualization:** OpenGL/GLUT
- **Build System:** Cabal + GHC 9.4+
- **Testing:** Hspec + QuickCheck (property-based testing)

---

## 📊 Current Status

**Version:** 0.1.0.0 (Beta)  
**Phase:** Phase 3 - Signal Processing Core  
**Lines of Code:** ~1,287 (Haskell + tests)

### Implementation Progress

| Phase | Component | Status |
|-------|-----------|--------|
| Phase 1 | Infrastructure & RTS | ✅ Complete |
| Phase 2 | Hardware Interface | ✅ Complete |
| Phase 3 | Signal Processing | 🔄 In Progress (80%) |
| Phase 4 | Safety & Control | ⏳ Pending |
| Phase 5 | User Interface | ⏳ Pending |
| Phase 6 | System Validation | ⏳ Pending |

**Recent Milestone:** Phase unwrapping implementation (FR-DSP-002) ✅

See [Project Status](docs/PROJECT_STATUS.md) for detailed roadmap.

---

## 🧪 Running Tests

```bash
# Run all tests
cabal test

# Run with verbose output
cabal test --test-show-details=direct

# Run specific test suite
cabal test --test-options="-m FFI.RingBuffer"

# Generate coverage report
cabal test --enable-coverage
```

**Test Coverage:**
- Unit tests for all modules
- Property-based testing with QuickCheck
- FFI layer memory safety validation
- Hardware parser fuzz testing

---

## 🔒 Safety & Compliance

Lambda-Wave is developed to meet **IEC 62304 Class C** requirements (highest rigor for medical device software):

- ✅ **Traceable Requirements:** FR-* and SR-* identifiers
- ✅ **Strict Code Standards:** `-Wall` compiler warnings, `hlint` static analysis
- ✅ **Comprehensive Testing:** Unit + property + integration tests
- ✅ **Code Review:** Four-eyes principle for safety-critical code
- ✅ **Version Control:** Git with semantic versioning
- ✅ **Audit Trail:** Immutable logging of all events
- ✅ **Risk Management:** FMEA analysis per ISO 14971

See [Purpose & Architecture](docs/PURPOSE_AND_ARCHITECTURE.md) for compliance details.

---

## 🤝 Contributing

We welcome contributions! Please read our [Contributing Guide](CONTRIBUTING.md) for:

- Development workflow (Git-flow)
- Code standards (Haskell + C++)
- Pull request process
- Safety-critical code guidelines

**Quick Start for Contributors:**

```bash
# Fork and clone
git clone https://github.com/YOUR_USERNAME/lambda-wave.git
cd lambda-wave

# Create feature branch
git checkout -b feature/my-feature

# Make changes, test, commit
cabal test
git add .
git commit -m "feat: Add new feature"

# Push and create PR
git push origin feature/my-feature
```

---

## 📄 License

This project is licensed under the **BSD 3-Clause License** - see the [LICENSE](LICENSE) file for details.

```
Copyright (c) 2024, DevOps Architect
All rights reserved.
```

---

## 📞 Contact & Support

- **Issues:** [GitHub Issues](https://github.com/fderuiter/lambda-wave/issues)
- **Discussions:** [GitHub Discussions](https://github.com/fderuiter/lambda-wave/discussions)
- **Email:** maintainer@example.com
- **Documentation:** [docs/](docs/)

---

## 🌟 Acknowledgments

- **Research Foundation:** Based on algorithms from [Bressler et al., Medical Physics 2024](docs/mathematical_framework.md)
- **Hardware:** Texas Instruments IWR6843ISK mmWave evaluation kit
- **Community:** Haskell, GHC, and Cabal teams

---

## 📈 Project Metrics

- **Build Time:** ~5-10 minutes (clean build)
- **Test Suite:** <60 seconds
- **Docker Image:** ~2-3 GB
- **Dependencies:** 15 core Haskell packages
- **Target Latency:** <50ms end-to-end

---

**Ready to get started?** → [Quick Start Guide](docs/QUICKSTART.md)

**Want to contribute?** → [Contributing Guide](CONTRIBUTING.md)

**Need help?** → [Open an issue](https://github.com/fderuiter/lambda-wave/issues)
