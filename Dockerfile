# Dockerfile for SGRT Radar System
FROM haskell:9.4

# Install system dependencies
RUN apt-get update && apt-get install -y \
    build-essential \
    liblapack-dev \
    libblas-dev \
    clang-format \
    && rm -rf /var/lib/apt/lists/*

# Set working directory
WORKDIR /app

# Copy project definition
COPY sgrt-radar-system.cabal cabal.project ./

# Update cabal index and install dependencies
RUN cabal update && cabal build --only-dependencies

# Copy source code
COPY . .

# Build the project
RUN cabal build

# Default command
CMD ["cabal", "run"]
