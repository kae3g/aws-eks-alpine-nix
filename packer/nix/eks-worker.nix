# EKS Worker Node Configuration with Nix 💙
# This Nix expression defines the declarative environment for our Alpine+Nix EKS worker nodes
# It ensures that every worker node has exactly the same software environment

{ pkgs ? import <nixpkgs> {} }:

# Create a custom environment that includes all EKS worker essentials
pkgs.buildEnv {
  name = "eks-worker-environment";
  
  paths = with pkgs; [
    # Core utilities
    coreutils
    bash
    curl
    wget
    jq
    yq-go
    
    # AWS and Kubernetes tooling
    awscli2
    kubectl
    kubernetes
    docker
    docker-compose
    
    # Monitoring and debugging tools
    htop
    iotop
    netstat-nat
    tcpdump
    strace
    
    # Security tools
    openssh
    gnupg
    age
    
    # Development tools (for debugging and maintenance)
    git
    vim
    nano
    tmux
    
    # Network tools
    bind
    dnsutils
    nmap
    
    # System monitoring
    sysstat
    procps
    util-linux
  ];
  
  # Create a custom shell script that sets up the environment
  postBuild = ''
    cat > $out/bin/eks-worker-setup << 'EOF'
#!/bin/bash
# EKS Worker Setup Script
# This script ensures the Nix environment is properly loaded

# Source the Nix environment
if [ -f /home/nix/.nix-profile/etc/profile.d/nix.sh ]; then
    source /home/nix/.nix-profile/etc/profile.d/nix.sh
fi

# Add Nix paths to the current environment
export PATH="$out/bin:$PATH"

# Set up Docker group membership for the nix user
if ! groups nix | grep -q docker; then
    sudo usermod -aG docker nix
fi

# Ensure Docker is running
if ! systemctl is-active --quiet docker; then
    sudo systemctl start docker
    sudo systemctl enable docker
fi

# Set up kubelet directory
sudo mkdir -p /var/lib/kubelet
sudo chown nix:nixbld /var/lib/kubelet

echo "EKS Worker environment configured successfully!"
echo "Available tools: $(ls $out/bin | tr '\n' ' ')"
EOF
    
    chmod +x $out/bin/eks-worker-setup
  '';
}
