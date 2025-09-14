#!/usr/bin/env bash

# Build script for NixOS EKS worker AMI
# This script builds a custom NixOS AMI optimized for AWS EKS worker nodes

set -euo pipefail

# Configuration
REGION="${AWS_REGION:-us-west-2}"
INSTANCE_TYPE="${INSTANCE_TYPE:-t3.medium}"
AMI_NAME="${AMI_NAME:-nixos-eks-worker-$(date +%Y%m%d-%H%M%S)}"
AMI_DESCRIPTION="NixOS EKS Worker Node - $(date +%Y-%m-%d)"

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

# Check prerequisites
check_prerequisites() {
    log_info "Checking prerequisites..."
    
    # Check if AWS CLI is installed and configured
    if ! command -v aws &> /dev/null; then
        log_error "AWS CLI is not installed. Please install it first."
        exit 1
    fi
    
    # Check if AWS credentials are configured
    if ! aws sts get-caller-identity &> /dev/null; then
log_error "AWS credentials are not configured. Please run 'aws configure'
first."
        exit 1
    fi
    
    # Check if nix is installed
    if ! command -v nix &> /dev/null; then
        log_error "Nix is not installed. Please install it first."
        exit 1
    fi
    
    log_success "All prerequisites are met!"
}

# Build the NixOS configuration
build_nixos_config() {
    log_info "Building NixOS configuration..."
    
    cd "$(dirname "$0")/../nixos"
    
    # Build the configuration
    nix build .#nixosConfigurations.eks-worker.config.system.build.toplevel
    
    log_success "NixOS configuration built successfully!"
}

# Build the AMI using nixos-eks-ami
build_ami() {
    log_info "Building AMI using nixos-eks-ami..."
    
    cd "$(dirname "$0")/../nixos"
    
    # Use nixos-eks-ami to build the AMI
    nix run github:DeterminateSystems/nixos-eks-ami -- \
        --region "$REGION" \
        --instance-type "$INSTANCE_TYPE" \
        --ami-name "$AMI_NAME" \
        --ami-description "$AMI_DESCRIPTION" \
        --nixos-config ./configs/eks-worker.nix
    
    log_success "AMI built successfully!"
}

# Get the AMI ID
get_ami_id() {
    log_info "Getting AMI ID..."
    
    AMI_ID=$(aws ec2 describe-images \
        --region "$REGION" \
        --owners self \
        --filters "Name=name,Values=$AMI_NAME" \
        --query 'Images[0].ImageId' \
        --output text)
    
    if [ "$AMI_ID" = "None" ] || [ -z "$AMI_ID" ]; then
        log_error "Failed to find AMI with name: $AMI_NAME"
        exit 1
    fi
    
    log_success "AMI ID: $AMI_ID"
    echo "$AMI_ID"
}

# Main function
main() {
    log_info "Starting NixOS EKS worker AMI build process..."
    log_info "Region: $REGION"
    log_info "Instance Type: $INSTANCE_TYPE"
    log_info "AMI Name: $AMI_NAME"
    
    check_prerequisites
    build_nixos_config
    build_ami
    
    AMI_ID=$(get_ami_id)
    
    log_success "Build completed successfully!"
    log_info "AMI ID: $AMI_ID"
    log_info "You can now use this AMI to create EKS worker nodes."
    
    # Output the AMI ID for use in other scripts
    echo "$AMI_ID" > /tmp/ami-id.txt
}

# Run main function
main "$@"