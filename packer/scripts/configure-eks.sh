#!/bin/sh
# Configure Alpine Linux for EKS worker nodes
set -e

echo "🚀 Configuring EKS worker environment..."

# Install EKS dependencies
apk add --no-cache \
    ca-certificates \
    iptables \
    ebtables \
    ethtool \
    socat \
    conntrack-tools

# Install AWS CLI and tools
apk add --no-cache \
    aws-cli \
    python3 \
    py3-pip

pip3 install --upgrade \
    awscli \
    python-dateutil

# Create eks user
adduser -S -D -h /home/eks -s /bin/sh eks
addgroup eks nixbld

# Configure kernel modules
echo "br_netfilter" >> /etc/modules
echo "nf_conntrack_ipv4" >> /etc/modules
echo "nf_conntrack_ipv6" >> /etc/modules

# Configure sysctl for Kubernetes
cat > /etc/sysctl.d/99-kubernetes.conf << EOF
net.bridge.bridge-nf-call-iptables = 1
net.bridge.bridge-nf-call-ip6tables = 1
net.ipv4.ip_forward = 1
net.ipv6.conf.all.forwarding = 1
EOF

# Load sysctl settings
sysctl --system

# Configure containerd (installed via Nix later)
mkdir -p /etc/containerd
containerd config default > /etc/containerd/config.toml

# Configure systemd services (if using openrc)
mkdir -p /etc/runlevels/default
ln -s /etc/init.d/containerd /etc/runlevels/default/containerd
ln -s /etc/init.d/kubelet /etc/runlevels/default/kubelet

echo "✅ EKS worker configuration complete"
