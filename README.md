# 🌸 Gentle Revolution: Sovereign AWS EKS with Alpine Linux and Nix
> **b122m faeb internet identity** | **Verifiable, Reproducible, Minimal**

## 💙 Philosophy

This repository embodies a gentle revolution against complex, opaque cloud infrastructure. Built on RISC-V paradigm principles of simplicity, modularity, and open standards, we build sovereign systems using:

- **Alpine Linux**: Security-oriented, minimal Linux distribution (5MB base!)
- **Nix**: Declarative package management and configuration
- **AWS EKS**: Managed Kubernetes without vendor lock-in
- **Terraform/Packer**: Infrastructure as reproducible code
- **b122m faeb identity**: Cryptographic sovereignty from first commit
- **RISC-V Paradigm**: Reduced Instruction Set Computer principles for infrastructure design

## 🚀 Quick Start

### Prerequisites
1. **Establish Identity** (required first):
```bash
gpg --full-gen-key
# Choose: (9) ECC and ECC, (1) Curve 25519, 0 = no expiration
# Identity: "yourname (b122m faeb internet identity) <your-email@gmail.com>"
gpg --export-ssh-key your-email@gmail.com > ~/.ssh/id_ed25519.pub
```

2. **Configure Git Signing**:
```bash
git config --global user.name "yourname"
git config --global user.email "your-email@gmail.com"
git config --global user.signingkey $(gpg --list-secret-keys --keyid-format LONG | grep sec | awk '{print $2}' | cut -d'/' -f2)
git config --global commit.gpgsign true
```

3. **Clone and Initialize**:
```bash
git clone git@github.com:kae3g/aws-eks-alpine-nix.git
cd aws-eks-alpine-nix
./bin/init-identity
```

## 📁 Repository Structure
```
aws-eks-alpine-nix/
├── identity/           # Cryptographic foundation
├── bin/               # Sovereign automation scripts
├── docs/              # Gentle learning journey
├── nix/               # Declarative package management
├── packer/            # Alpine+Nix AMI builder
├── terraform/         # EKS cluster definition
├── examples/          # Sample applications
└── monitoring/        # Observability stack
```

## 🌈 What Makes This Different

- **Identity-First**: Every commit GPG-signed, every artifact verifiable
- **Mathematical Precision**: Nix-based reproducible builds
- **Gentle Learning**: Documentation that respects your intelligence
- **Sovereign Infrastructure**: No hidden APIs, no telemetry, no lock-in
- **Community Focus**: Built for learning together, not in isolation
- **RISC-V Principles**: 
  - **Simplicity**: Minimal, orthogonal infrastructure components
  - **Modularity**: Extensible architecture with standard components
  - **Open Source**: Royalty-free, community-driven development
  - **Performance**: Efficient resource utilization through reduced complexity
  - **Verifiability**: Formal verification of infrastructure behavior

## 📖 Learning Journey

1. **Phase 1**: Identity Establishment (you are here)
2. **Phase 2**: AMI Construction with Alpine+Nix
3. **Phase 3**: EKS Cluster Deployment
4. **Phase 4**: Application Deployment
5. **Phase 5**: Monitoring and Observability

## 🔐 Security by Design

- **Non-standard SSH ports** (4922) reduce automated attacks
- **Key-based authentication only** - no passwords
- **Minimal attack surface** with Alpine Linux
- **All commits cryptographically signed**
- **Infrastructure as verifiable code**

## 💫 Verification

All artifacts are signed and verifiable:
```bash
# Verify commits
git verify-commit HEAD

# Verify AMI checksums
sha256sum alpine-nix-ami-*.json

# Verify Nix packages
nix-store --verify --check-contents
```

## 🆘 Need Help?

Join our gentle revolution community:
- Issues: https://github.com/kae3g/aws-eks-alpine-nix/issues
- Discussions: https://github.com/kae3g/aws-eks-alpine-nix/discussions

## 📄 License

Apache 2.0 - You have the freedom to use, modify, and distribute this work.

---
*This is a gentle revolution. We build together with care and intention.* 🌸💙