#!/usr/bin/env bash

# Test script for minimal NixOS setup
# This script verifies that everything is working correctly

set -euo pipefail

# Colors
GREEN='\033[0;32m'
RED='\033[0;31m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m'

log_info() {
    echo -e "${BLUE}[TEST]${NC} $1"
}

log_success() {
    echo -e "${GREEN}[PASS]${NC} $1"
}

log_error() {
    echo -e "${RED}[FAIL]${NC} $1"
}

log_warning() {
    echo -e "${YELLOW}[WARN]${NC} $1"
}

# Test counter
TESTS_PASSED=0
TESTS_FAILED=0

# Test function
run_test() {
    local test_name="$1"
    local test_command="$2"
    
    log_info "Testing: $test_name"
    
    if eval "$test_command" &> /dev/null; then
        log_success "$test_name"
        ((TESTS_PASSED++))
        return 0
    else
        log_error "$test_name"
        ((TESTS_FAILED++))
        return 1
    fi
}

# Test NixOS configuration
test_nixos_config() {
    log_info "Testing NixOS configuration..."
    
    if [ -f nixos/minimal-config.nix ]; then
        run_test "NixOS config file exists" "test -f nixos/minimal-config.nix"
    else
        log_error "NixOS config file not found"
        ((TESTS_FAILED++))
    fi
    
    if [ -f nixos/minimal-flake.nix ]; then
        run_test "NixOS flake file exists" "test -f nixos/minimal-flake.nix"
    else
        log_error "NixOS flake file not found"
        ((TESTS_FAILED++))
    fi
}

# Test Docker setup
test_docker_setup() {
    log_info "Testing Docker setup..."
    
    run_test "Dockerfile exists" "test -f docker/Dockerfile.minimal"
run_test "Docker compose file exists" "test -f
docker/docker-compose.minimal.yml"
    
    if command -v docker &> /dev/null; then
        run_test "Docker is installed" "command -v docker"
        run_test "Docker daemon is running" "docker info &> /dev/null"
    else
        log_warning "Docker not installed - skipping Docker tests"
    fi
}

# Test scripts
test_scripts() {
    log_info "Testing scripts..."
    
    run_test "Setup script exists" "test -f scripts/setup-minimal.sh"
    run_test "Setup script is executable" "test -x scripts/setup-minimal.sh"
    run_test "Test script exists" "test -f test-minimal.sh"
    run_test "Test script is executable" "test -x test-minimal.sh"
}

# Test container build (if Docker is available)
test_container_build() {
    if command -v docker &> /dev/null; then
        log_info "Testing container build..."
        
        cd docker/
if docker build -f Dockerfile.minimal -t nixos-minimal-test . &> /dev/null; then
            log_success "Container builds successfully"
            ((TESTS_PASSED++))
            
            # Test container runs
if docker run --rm nixos-minimal-test ghc --version &> /dev/null; then
                log_success "Container runs and GHC works"
                ((TESTS_PASSED++))
            else
                log_error "Container runs but GHC doesn't work"
                ((TESTS_FAILED++))
            fi
        else
            log_error "Container build failed"
            ((TESTS_FAILED++))
        fi
        cd ..
    else
        log_warning "Docker not available - skipping container tests"
    fi
}

# Test file structure
test_file_structure() {
    log_info "Testing file structure..."
    
    run_test "NixOS directory exists" "test -d nixos"
    run_test "Docker directory exists" "test -d docker"
    run_test "Scripts directory exists" "test -d scripts"
    run_test "README exists" "test -f README.md"
    run_test "Advanced README exists" "test -f README.advanced.md"
    run_test "Backup directory exists" "test -d minimal-backup"
}

# Main test function
main() {
    echo "🧪 Testing minimal NixOS setup..."
    echo "=================================="
    
    test_file_structure
    test_nixos_config
    test_docker_setup
    test_scripts
    test_container_build
    
    echo ""
    echo "=================================="
    echo "📊 Test Results:"
    echo "✅ Passed: $TESTS_PASSED"
    echo "❌ Failed: $TESTS_FAILED"
    echo "📈 Total: $((TESTS_PASSED + TESTS_FAILED))"
    
    if [ $TESTS_FAILED -eq 0 ]; then
        echo ""
        log_success "🎉 All tests passed! Your minimal setup is ready!"
        echo ""
        echo "Next steps:"
        echo "1. Run: ./scripts/setup-minimal.sh"
        echo "2. Test: docker-compose -f docker/docker-compose.minimal.yml up"
        echo "3. Enjoy your minimal NixOS environment! 🚀"
    else
        echo ""
        log_error "Some tests failed. Please check the errors above."
        exit 1
    fi
}

# Run tests
main "$@"
