#!/usr/bin/env bash
set -euo pipefail

# Build script for NixOS EKS worker AMI
# Usage: ./scripts/build-ami.sh [region] [kubernetes-version]

REGION=${1:-us-east-1}
K8S_VERSION=${2:-1.28}
AMI_NAME="nixos-eks-${K8S_VERSION}-$(date +%Y%m%d)"

echo "Building NixOS EKS AMI..."
echo "Region: $REGION"
echo "Kubernetes Version: $K8S_VERSION"
echo "AMI Name: $AMI_NAME"

# Check if we're in the nixos directory
if [ ! -f "flake.nix" ]; then
    echo "Error: Please run this script from the nixos/ directory"
    exit 1
fi

# Build the AMI using nixos-eks-ami
nix run github:DeterminateSystems/nixos-eks-ami -- \
  build \
  --ami-name "$AMI_NAME" \
  --kubernetes-version "$K8S_VERSION" \
  --region "$REGION" \
  --nixos-configuration "$(pwd)/flake.nix"

echo "AMI build completed!"
echo "AMI Name: $AMI_NAME"
echo "Region: $REGION"
echo ""
echo "Next steps:"
echo "1. Note the AMI ID from the output above"
echo "2. Update kubernetes/cluster-config.yaml with the AMI ID"
echo "3. Run: eksctl create cluster -f kubernetes/cluster-config.yaml"
