# Dockerfile for SGRT Radar System
FROM haskell:9.4

# Install system dependencies
COPY scripts/setup_env.sh /tmp/setup_env.sh
RUN chmod +x /tmp/setup_env.sh && /tmp/setup_env.sh

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
