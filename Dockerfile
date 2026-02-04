# Build Stage
FROM haskell:9.8.2 AS builder

WORKDIR /app

# Install build-time system dependencies
RUN apt-get update && apt-get install -y \
    build-essential \
    liblapack-dev \
    libblas-dev \
    clang \
    && rm -rf /var/lib/apt/lists/*

# 1. Copy ONLY the project definition first
# This ensures we only re-download dependencies if .cabal or .project changes
COPY sgrt-radar-system.cabal cabal.project ./

# 2. Build dependencies
RUN cabal update && cabal build --only-dependencies

# 3. Copy the rest of the source code
COPY . .

# 4. Build the actual application
RUN cabal build

# 5. Extract the binary to a known location
RUN mkdir -p /app/bin && cp $(cabal list-bin sgrt-radar-system-exe) /app/bin/sgrt-radar-system

# Runtime Stage (Slimmer Image)
FROM ubuntu:22.04
WORKDIR /app

# Install runtime libs (no compilers needed)
RUN apt-get update && apt-get install -y \
    liblapack3 \
    libblas3 \
    libatomic1 \
    && rm -rf /var/lib/apt/lists/*

# Copy artifacts from builder
COPY --from=builder /app/bin/sgrt-radar-system /app/sgrt-radar-system
COPY --from=builder /app/scripts/setup_env.sh /app/setup_env.sh

# Run the application
CMD ["/app/sgrt-radar-system"]
