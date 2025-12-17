#!/usr/bin/env bash

# Forge: Setup Environment Script
# Mission: Install system dependencies robustly across Linux (apt) and macOS (brew).

set -euo pipefail

# --- Logging Functions ---
log_info() {
    echo -e "\033[34m[INFO]\033[0m $1"
}

log_error() {
    echo -e "\033[31m[ERROR]\033[0m $1" >&2
}

log_success() {
    echo -e "\033[32m[SUCCESS]\033[0m $1"
}

# --- Utilities ---
command_exists() {
    command -v "$1" >/dev/null 2>&1
}

ensure_sudo() {
    if [ "$(id -u)" -eq 0 ]; then
        echo ""
    elif command_exists sudo; then
        echo "sudo"
    else
        log_error "This script requires root privileges or sudo access to install packages."
        exit 1
    fi
}

# --- OS Detection & Installation ---
main() {
    local os_name
    os_name="$(uname -s)"
    local sudo_cmd
    sudo_cmd="$(ensure_sudo)"

    log_info "Detected OS: $os_name"

    case "$os_name" in
        Linux)
            if command_exists apt-get; then
                log_info "Detected package manager: apt-get"

                # Prevent interactive prompts
                export DEBIAN_FRONTEND=noninteractive

                log_info "Updating package lists..."
                $sudo_cmd apt-get update

                log_info "Installing dependencies..."
                # build-essential: Compiler tools
                # liblapack-dev, libblas-dev: Linear algebra (hmatrix)
                # freeglut3-dev, libgl1-mesa-dev, libglu1-mesa-dev: OpenGL/GLUT
                # clang-format: C/C++ Linting
                $sudo_cmd apt-get install -y --no-install-recommends \
                    build-essential \
                    liblapack-dev \
                    libblas-dev \
                    freeglut3-dev \
                    libgl1-mesa-dev \
                    libglu1-mesa-dev \
                    libx11-dev \
                    libxrandr-dev \
                    libxinerama-dev \
                    libxcursor-dev \
                    libxi-dev \
                    pkg-config \
                    clang-format

                # Clean up apt cache to keep images small (only if running as root/docker context usually, but good practice)
                if [ -n "${DOCKER_CONTAINER:-}" ] || [ -f /.dockerenv ]; then
                     log_info "Cleaning up apt cache..."
                     rm -rf /var/lib/apt/lists/*
                fi

            elif command_exists dnf; then
                log_info "Detected package manager: dnf"
                log_info "Installing dependencies..."
                $sudo_cmd dnf install -y \
                    make automake gcc gcc-c++ kernel-devel \
                    lapack-devel blas-devel \
                    freeglut-devel mesa-libGL-devel mesa-libGLU-devel \
                    clang-tools-extra
            else
                log_error "Unsupported Linux distribution. Please install dependencies manually."
                exit 1
            fi
            ;;
        Darwin)
            if command_exists brew; then
                log_info "Detected package manager: Homebrew"
                log_info "Installing dependencies..."

                # Brew bundle or direct install? Direct install for simplicity.
                # lapack/openblas are keg-only in brew usually, might need env vars.
                # But installing them is the first step.
                brew install lapack openblas freeglut mesa clang-format

                log_info "Note: You may need to set LDFLAGS/CPPFLAGS for openblas/lapack."
            else
                log_error "Homebrew not found. Please install Homebrew first."
                exit 1
            fi
            ;;
        *)
            log_error "Unsupported OS: $os_name"
            exit 1
            ;;
    esac

    log_success "Environment setup complete."
}

main "$@"
