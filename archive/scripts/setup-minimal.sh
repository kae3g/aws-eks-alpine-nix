#!/usr/bin/env bash

# Minimal NixOS Development Setup Script
# This script sets up a minimal NixOS development environment

set -euo pipefail

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Logging functions
log_info() {
    echo -e "${BLUE}[INFO]${NC} $1"
}

log_success() {
    echo -e "${GREEN}[SUCCESS]${NC} $1"
}

log_warning() {
    echo -e "${YELLOW}[WARNING]${NC} $1"
}

log_error() {
    echo -e "${RED}[ERROR]${NC} $1"
}

# Check if we're on NixOS
check_nixos() {
    if [ -f /etc/nixos/configuration.nix ]; then
        log_success "Running on NixOS!"
        return 0
    else
log_warning "Not running on NixOS. This script is designed for NixOS systems."
        return 1
    fi
}

# Install Nix (if not already installed)
install_nix() {
    if command -v nix &> /dev/null; then
        log_success "Nix is already installed!"
        return 0
    fi
    
    log_info "Installing Nix..."
    sh <(curl -L https://nixos.org/nix/install) --no-daemon
    
    # Source nix
    if [ -f /home/$USER/.nix-profile/etc/profile.d/nix.sh ]; then
        source /home/$USER/.nix-profile/etc/profile.d/nix.sh
    fi
    
    log_success "Nix installed successfully!"
}

# Set up the minimal NixOS configuration
setup_nixos_config() {
    log_info "Setting up minimal NixOS configuration..."
    
    # Create backup of existing config
    if [ -f /etc/nixos/configuration.nix ]; then
        log_info "Backing up existing configuration..."
sudo cp /etc/nixos/configuration.nix /etc/nixos/configuration.nix.backup.$(date
+%Y%m%d_%H%M%S)
    fi
    
    # Copy our minimal config
    sudo cp nixos/minimal-config.nix /etc/nixos/configuration.nix
    
    log_success "NixOS configuration updated!"
}

# Build and apply the configuration
build_and_apply() {
    log_info "Building NixOS configuration..."
    
    # Build the configuration
    sudo nixos-rebuild build
    
    if [ $? -eq 0 ]; then
        log_success "Configuration built successfully!"
        
        log_info "Applying configuration..."
        sudo nixos-rebuild switch
        
        if [ $? -eq 0 ]; then
            log_success "Configuration applied successfully!"
        else
            log_error "Failed to apply configuration!"
            return 1
        fi
    else
        log_error "Failed to build configuration!"
        return 1
    fi
}

# Set up Docker
setup_docker() {
    log_info "Setting up Docker..."
    
    # Add user to docker group
    sudo usermod -aG docker $USER
    
    # Start Docker service
    sudo systemctl enable docker
    sudo systemctl start docker
    
    log_success "Docker setup complete!"
log_warning "You may need to log out and back in for Docker group changes to
take effect."
}

# Build the minimal container
build_container() {
    log_info "Building minimal NixOS container..."
    
    cd docker/
    docker build -f Dockerfile.minimal -t nixos-minimal .
    
    if [ $? -eq 0 ]; then
        log_success "Container built successfully!"
    else
        log_error "Failed to build container!"
        return 1
    fi
    
    cd ..
}

# Test the setup
test_setup() {
    log_info "Testing the setup..."
    
    # Test Haskell
    if command -v ghc &> /dev/null; then
        log_success "Haskell (GHC) is available!"
    else
        log_error "Haskell (GHC) not found!"
        return 1
    fi
    
    # Test Zsh
    if command -v zsh &> /dev/null; then
        log_success "Zsh is available!"
    else
        log_error "Zsh not found!"
        return 1
    fi
    
    # Test Docker
    if command -v docker &> /dev/null; then
        log_success "Docker is available!"
        
        # Test container
        if docker run --rm nixos-minimal ghc --version &> /dev/null; then
            log_success "Container is working!"
        else
            log_error "Container test failed!"
            return 1
        fi
    else
        log_error "Docker not found!"
        return 1
    fi
}

# Main function
main() {
    log_info "🚀 Setting up minimal NixOS development environment..."
    
    # Check if we're on NixOS
    if ! check_nixos; then
        log_warning "This script is designed for NixOS systems."
        log_info "You can still use the Docker setup for development."
    fi
    
    # Install Nix if needed
    install_nix
    
    # Set up NixOS configuration (if on NixOS)
    if check_nixos; then
        setup_nixos_config
        build_and_apply
        setup_docker
    fi
    
    # Build container
    build_container
    
    # Test everything
    test_setup
    
    log_success "🎉 Minimal NixOS development environment is ready!"
    log_info "Next steps:"
log_info "1. Run 'docker-compose -f docker/docker-compose.minimal.yml up' to
start containers"
    log_info "2. Run 'ghc hello.hs && ./hello' to test Haskell"
    log_info "3. Run 'zsh' to use the enhanced shell"
}

# Run main function
main "$@"
