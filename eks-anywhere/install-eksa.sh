#!/bin/bash
# EKS Anywhere Installation Script
# This script installs EKS Anywhere and sets up our sovereign infrastructure

set -euo pipefail

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
BLUE='\033[0;34m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

# Logging function
log() {
    echo -e "${BLUE}[$(date +'%Y-%m-%d %H:%M:%S')]${NC} $1"
}

error() {
    echo -e "${RED}[ERROR]${NC} $1" >&2
}

success() {
    echo -e "${GREEN}[SUCCESS]${NC} $1"
}

warning() {
    echo -e "${YELLOW}[WARNING]${NC} $1"
}

# Check prerequisites
check_prerequisites() {
    log "Checking prerequisites for EKS Anywhere installation..."
    
    # Check if Docker is running
    if ! docker info >/dev/null 2>&1; then
        error "Docker is not running. Please start Docker and try again."
        exit 1
    fi
    
    # Check if kubectl is installed
    if ! command -v kubectl &> /dev/null; then
        error "kubectl is not installed. Please install kubectl first."
        exit 1
    fi
    
    # Check if eksctl is installed
    if ! command -v eksctl &> /dev/null; then
        warning "eksctl is not installed. Installing eksctl..."
        install_eksctl
    fi
    
    success "Prerequisites check completed"
}

# Install eksctl
install_eksctl() {
    log "Installing eksctl..."
    
    if [[ "$OSTYPE" == "darwin"* ]]; then
        # macOS
        brew tap weaveworks/tap
        brew install weaveworks/tap/eksctl
    elif [[ "$OSTYPE" == "linux-gnu"* ]]; then
        # Linux
curl --silent --location
"https://github.com/weaveworks/eksctl/releases/latest/download/eksctl_$(uname
-s)_amd64.tar.gz" | tar xz -C /tmp
        sudo mv /tmp/eksctl /usr/local/bin
    else
        error "Unsupported operating system: $OSTYPE"
        exit 1
    fi
    
    success "eksctl installed successfully"
}

# Download EKS Anywhere
download_eks_anywhere() {
    log "Downloading EKS Anywhere..."
    
    local eksa_version="v0.18.0"
local
download_url="https://github.com/aws/eks-anywhere/releases/download/${eksa_version}/eksctl-anywhere-${eksa_version}-$(uname
-s | tr '[:upper:]' '[:lower:]')-amd64.tar.gz"
    
    mkdir -p bin
    cd bin
    
    curl -L "${download_url}" | tar xz
    chmod +x eksctl-anywhere
    
    success "EKS Anywhere downloaded and installed"
    cd ..
}

# Create cluster configuration
create_cluster_config() {
    log "Creating cluster configuration..."
    
    # Generate SSH key if it doesn't exist
    if [[ ! -f ~/.ssh/id_rsa.pub ]]; then
        log "Generating SSH key pair..."
        ssh-keygen -t rsa -b 4096 -f ~/.ssh/id_rsa -N ""
    fi
    
    # Update cluster configuration with actual SSH key
    local ssh_key=$(cat ~/.ssh/id_rsa.pub)
    sed -i.bak "s|ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABgQC\.\.\.|${ssh_key}|g" eks-anywhere/eksa-cluster.yaml
    
    success "Cluster configuration created"
}

# Install cluster dependencies
install_cluster_dependencies() {
    log "Installing cluster dependencies..."
    
    ./bin/eksctl-anywhere create cluster -f eks-anywhere/eksa-cluster.yaml \
        --install-packages packages.yaml \
        --kubeconfig ./kubeconfig \
        --verbose 5
    
    success "Cluster dependencies installed"
}

# Deploy Faeb System enzymes
deploy_faeb_enzymes() {
    log "Deploying Faeb System enzymes..."
    
    export KUBECONFIG=./kubeconfig
    
    # Apply custom components
    kubectl apply -f eks-anywhere/custom-components.yaml
    
    # Wait for enzymes to be ready
kubectl wait --for=condition=ready pod -l app=faeb-system-enzymes --timeout=300s
    
    success "Faeb System enzymes deployed successfully"
}

# Setup monitoring and observability
setup_monitoring() {
    log "Setting up monitoring and observability..."
    
    export KUBECONFIG=./kubeconfig
    
    # Install Prometheus
kubectl apply -f
https://raw.githubusercontent.com/prometheus-operator/prometheus-operator/main/bundle.yaml
    
    # Install Grafana
kubectl apply -f
https://raw.githubusercontent.com/grafana/helm-charts/main/charts/grafana/values.yaml
    
    # Install Loki for logging
    helm repo add grafana https://grafana.github.io/helm-charts
    helm install loki grafana/loki-stack
    
    success "Monitoring and observability setup completed"
}

# Main installation function
main() {
    log "Starting EKS Anywhere installation for Faeb System..."
log "This will create a sovereign infrastructure cluster with enzyme/catalyst
architecture"
    
    check_prerequisites
    download_eks_anywhere
    create_cluster_config
    install_cluster_dependencies
    deploy_faeb_enzymes
    setup_monitoring
    
    success "EKS Anywhere installation completed successfully!"
    log "Your sovereign infrastructure cluster is ready!"
    log "Kubeconfig saved to: ./kubeconfig"
log "Access your cluster with: export KUBECONFIG=./kubeconfig && kubectl get
nodes"
    
    echo ""
    echo "🌸 The Three Breaths System is now active:"
    echo "   🌱 First Breath (Haskell): Creating ideas from pure thought"
    echo "   🎵 Second Breath (Nix): Making ideas real with reproducibility"
    echo "   🎨 Third Breath (Faeb): Making reality beautiful with visuals"
    echo ""
    echo "💙 Your sovereign infrastructure is ready to serve the community!"
}

# Run main function
main "$@"
