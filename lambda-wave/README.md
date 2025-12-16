# Lambda-Wave

## Overview
Lambda-Wave is a Haskell-based radar SGRT (Surface Guided Radiation Therapy) system.

## Structure
- `app/`: Application entry point.
- `src/`: Core library (Logic, Hardware, Safety, UI).
- `cbits/`: C-based ring buffer for high-performance ingestion.
- `bench/`: Latency benchmarks.
- `test/`: Unit and Golden tests.

## Build
```bash
stack build
```

## Run
```bash
stack exec lambda-wave-exe
```
