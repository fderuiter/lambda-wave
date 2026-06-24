# Dockerfile for SGRT Radar System
FROM haskell:9.6.7-slim-bullseye

# Install system dependencies
COPY scripts/setup_env.sh /tmp/setup_env.sh
RUN chmod +x /tmp/setup_env.sh && /tmp/setup_env.sh

# Set working directory
WORKDIR /app

# Copy project definition
COPY sgrt-radar-system.cabal cabal.project cabal.project.freeze ./

# Build dependencies securely
RUN cabal build --only-dependencies

# Copy source code
COPY . .

# Build the project
RUN cabal build

# Default command
CMD ["cabal", "run"]
