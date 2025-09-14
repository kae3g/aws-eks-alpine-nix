#!/bin/sh
# Install Nix package manager on Alpine Linux
set -e

echo "🔧 Installing Nix package manager..."

# Install dependencies
apk add --no-cache \
    curl \
    xz \
    sudo \
    bash

# Create nix build group and user
addgroup -S nixbld
for i in $(seq 1 10); do
    adduser -S -D -h /var/empty -s /sbin/nologin -G nixbld nixbld$i
done

# Create nix directory structure
mkdir -p /nix
chown root:nixbld /nix
chmod 1777 /nix

# Install Nix
curl -L https://nixos.org/nix/install | sh -s -- --no-daemon

# Initialize nix environment
. /root/.nix-profile/etc/profile.d/nix.sh

# Verify installation
if command -v nix-env >/dev/null 2>&1; then
    echo "✅ Nix installed successfully"
    nix-env --version
else
    echo "❌ Nix installation failed"
    exit 1
fi

# Configure nix for Alpine Linux
mkdir -p /etc/nix
cat > /etc/nix/nix.conf << EOF
sandbox = false
extra-sandbox-paths = /dev
build-users-group = nixbld
EOF

echo "📦 Nix configuration complete"
