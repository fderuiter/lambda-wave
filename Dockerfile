# Dockerfile for SGRT Radar System
FROM haskell@sha256:9cf51a755e3c8646dbfef84abb750bda3672258bdb375085e2db11f18c7c5e05

# Install system dependencies
COPY scripts/setup_env.sh /tmp/setup_env.sh
RUN chmod +x /tmp/setup_env.sh && /tmp/setup_env.sh

# Set working directory
WORKDIR /app

# Copy project definition
COPY sgrt-radar-system.cabal cabal.project ./

# Build dependencies securely
RUN cabal build --only-dependencies

# Copy source code
COPY . .

# Build the project
RUN cabal build

# Default command
CMD ["cabal", "run"]
