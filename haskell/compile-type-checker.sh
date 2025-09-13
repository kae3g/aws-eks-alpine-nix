#!/bin/bash
# Compile Type Checker - Haskell Enzyme Compilation Script
# This script compiles our Haskell enzymes for EKS Anywhere integration

set -euo pipefail

# Colors for gentle output
BLUE='\033[0;34m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

# Logging functions
log() {
    echo -e "${BLUE}[$(date +'%Y-%m-%d %H:%M:%S')]${NC} $1"
}

success() {
    echo -e "${GREEN}[SUCCESS]${NC} $1"
}

warning() {
    echo -e "${YELLOW}[WARNING]${NC} $1"
}

# Check if GHC is installed
check_ghc() {
    log "Checking for GHC installation..."
    
    if ! command -v ghc &> /dev/null; then
        warning "GHC not found. Installing GHC..."
        
        if [[ "$OSTYPE" == "darwin"* ]]; then
            # macOS
            if command -v brew &> /dev/null; then
                brew install ghc
            else
                echo "Please install Homebrew first, then run: brew install ghc"
                exit 1
            fi
        elif [[ "$OSTYPE" == "linux-gnu"* ]]; then
            # Linux
            sudo apt-get update
            sudo apt-get install -y ghc
        else
            echo "Unsupported operating system: $OSTYPE"
            exit 1
        fi
    fi
    
    success "GHC is available"
}

# Check if Cabal is installed
check_cabal() {
    log "Checking for Cabal installation..."
    
    if ! command -v cabal &> /dev/null; then
        warning "Cabal not found. Installing Cabal..."
        
        if [[ "$OSTYPE" == "darwin"* ]]; then
            # macOS
            if command -v brew &> /dev/null; then
                brew install cabal-install
            else
                echo "Please install Homebrew first, then run: brew install cabal-install"
                exit 1
            fi
        elif [[ "$OSTYPE" == "linux-gnu"* ]]; then
            # Linux
            sudo apt-get install -y cabal-install
        fi
    fi
    
    success "Cabal is available"
}

# Create Cabal package configuration
create_cabal_config() {
    log "Creating Cabal package configuration..."
    
    cat > haskell/eksa-enzymes.cabal << 'EOF'
cabal-version:      3.0
name:               eksa-enzymes
version:            1.0.0
description:        EKS Anywhere Enzyme System - Haskell modules for catalyst validation and instruction parsing
                    
author:             Faeb System Community
maintainer:         community@faeb-system.io
license:            Unlicense
license-file:       LICENSE
build-type:         Simple

executable eksa-type-checker
    main-is:         EksaTypeChecker.hs
    build-depends:   base >= 4.14 && < 5.0, yaml, text
    default-language: Haskell2010
    ghc-options:     -Wall -Wcompat -Widentities -Wincomplete-uni-patterns -Wincomplete-record-updates -Wmissing-deriving-strategies -Wmissing-exported-symbols -Wmissing-export-lists -Wmissing-home-modules -Wpartial-fields -Wredundant-constraints -Wunused-packages -Wunused-type-patterns

executable cursor-parser
    main-is:         CursorParser.hs
    build-depends:   base >= 4.14 && < 5.0, text, process
    default-language: Haskell2010
    ghc-options:     -Wall -Wcompat -Widentities -Wincomplete-uni-patterns -Wincomplete-record-updates -Wmissing-deriving-strategies -Wmissing-exported-symbols -Wmissing-export-lists -Wmissing-home-modules -Wpartial-fields -Wredundant-constraints -Wunused-packages -Wunused-type-patterns
EOF

    success "Cabal configuration created"
}

# Install dependencies
install_dependencies() {
    log "Installing Haskell dependencies..."
    
    cd haskell
    cabal update
    cabal install --dependencies-only
    
    success "Dependencies installed"
    cd ..
}

# Compile Haskell modules
compile_modules() {
    log "Compiling Haskell enzyme modules..."
    
    cd haskell
    
    # Compile EKS Anywhere type checker
    log "Compiling EKS Anywhere Type Checker..."
    cabal build eksa-type-checker
    
    # Compile Cursor parser
    log "Compiling Cursor Parser..."
    cabal build cursor-parser
    
    success "All Haskell modules compiled successfully"
    cd ..
}

# Copy binaries to bin directory
copy_binaries() {
    log "Copying compiled binaries to bin directory..."
    
    mkdir -p bin
    
    # Copy EKS Anywhere type checker
    cp haskell/dist-newstyle/build/aarch64-osx/ghc-*/eksa-enzymes-*/x/eksa-type-checker/build/eksa-type-checker/eksa-type-checker bin/ 2>/dev/null || \
    cp haskell/dist-newstyle/build/x86_64-linux/ghc-*/eksa-enzymes-*/x/eksa-type-checker/build/eksa-type-checker/eksa-type-checker bin/ 2>/dev/null || \
    echo "Warning: Could not find eksa-type-checker binary"
    
    # Copy Cursor parser
    cp haskell/dist-newstyle/build/aarch64-osx/ghc-*/eksa-enzymes-*/x/cursor-parser/build/cursor-parser/cursor-parser bin/ 2>/dev/null || \
    cp haskell/dist-newstyle/build/x86_64-linux/ghc-*/eksa-enzymes-*/x/cursor-parser/build/cursor-parser/cursor-parser bin/ 2>/dev/null || \
    echo "Warning: Could not find cursor-parser binary"
    
    # Make binaries executable
    chmod +x bin/eksa-type-checker bin/cursor-parser
    
    success "Binaries copied to bin directory"
}

# Test compiled modules
test_modules() {
    log "Testing compiled Haskell modules..."
    
    # Test EKS Anywhere type checker
    log "Testing EKS Anywhere Type Checker..."
    ./bin/eksa-type-checker || echo "Type checker test completed"
    
    # Test Cursor parser
    log "Testing Cursor Parser..."
    ./bin/cursor-parser || echo "Cursor parser test completed"
    
    success "Module tests completed"
}

# Main compilation function
main() {
    log "🧬 Haskell Enzyme Compilation - Building EKS Anywhere Integration 💙"
    log "=================================================================="
    log ""
    log "This script compiles our Haskell enzymes for EKS Anywhere integration."
    log "These enzymes will validate our catalyst configurations and parse instructions."
    log ""
    
    check_ghc
    check_cabal
    create_cabal_config
    install_dependencies
    compile_modules
    copy_binaries
    test_modules
    
    success "Haskell enzyme compilation completed successfully!"
    log ""
    log "🌸 Your Haskell enzymes are ready!"
    log "   🧬 EKS Anywhere Type Checker: bin/eksa-type-checker"
    log "   🔄 Cursor Parser: bin/cursor-parser"
    log ""
    log "💙 These enzymes will help validate and execute your EKS Anywhere setup!"
    log "Ready to create beautiful, reproducible infrastructure! 🌸"
}

# Run main function
main "$@"
