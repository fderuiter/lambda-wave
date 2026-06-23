# Dockerfile for SGRT Radar System
FROM haskell@sha256:9ae9287b4b48a8e437c290b8aa1a4a0433a1c2a3d3cff965ad0883426a41c960

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
