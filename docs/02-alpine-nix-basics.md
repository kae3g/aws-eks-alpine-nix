# Phase 2: Alpine Linux and Nix Basics
## The Tools of Our Gentle Revolution

## Why Alpine Linux?

Alpine Linux is a security-oriented, lightweight Linux distribution:
- **5MB base image** - minimal attack surface
- **musl libc** - security-focused standard library
- **BusyBox** - minimalist Unix utilities
- **OpenRC** - simple init system
- **Security-focused** - built with grsecurity/PaX patches

## Why Nix?

Nix is a powerful package manager that enables:
- **Reproducible builds** - same inputs always produce same outputs
- **Declarative configuration** - define what you want, not how to get it
- **Atomic upgrades** - never break your system during updates
- **Multiple versions** - run different versions of software simultaneously
- **Pure dependencies** - no more "works on my machine" problems

## Installing Alpine Linux

### For Development Environment
```bash
# Download Alpine Linux standard ISO
# Boot from USB and install with:
setup-alpine
```

### For AWS AMI (We'll automate this later)
```bash
# Use the Alpine Linux cloud image as base
# We'll use Packer to automate this process
```

## Installing Nix on Alpine

```bash
# Install dependencies
apk add --no-cache curl xz sudo bash

# Create nix build group and users
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

# Initialize environment
. ~/.nix-profile/etc/profile.d/nix.sh
```

## Basic Nix Commands

```bash
# Search for packages
nix search nixpkgs python

# Install package
nix-env -iA nixpkgs.python3

# List installed packages
nix-env -q

# Remove package
nix-env -e python3

# Garbage collection
nix-collect-garbage -d
```

## Our First Nix Expression

Create `hello.nix`:
```nix
{ pkgs ? import <nixpkgs> {} }:

pkgs.stdenv.mkDerivation {
  name = "hello-custom";
  version = "1.0";
  
  src = ./.;
  
  buildPhase = ''
    gcc -o hello hello.c
  '';
  
  installPhase = ''
    mkdir -p $out/bin
    cp hello $out/bin/
  '';
}
```

## Alpine + Nix = Sovereign Foundation

Together, Alpine and Nix provide:
- **Minimal base** (Alpine) + **reproducible builds** (Nix)
- **Security focus** + **dependency purity**
- **Small footprint** + **powerful packaging**

This combination forms the perfect foundation for our sovereign infrastructure.

## Next Steps

Now that you understand the basics, we'll:
1. Build our first Alpine+Nix AMI with Packer
2. Configure it as an EKS worker node
3. Deploy our sovereign Kubernetes cluster

*The gentle revolution builds on strong foundations.*
