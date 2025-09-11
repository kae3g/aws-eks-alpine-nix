# NixOS on AWS EKS: A Declarative Kubernetes Stack

This repository contains a complete, declarative infrastructure setup for running a Haskell application on a Kubernetes cluster built on **NixOS worker nodes**, managed by **AWS EKS**. Everything—from the host OS configuration to the Kubernetes manifests—is defined in code, primarily using the Nix language.

## 📋 Overview

This guide will walk you through:

1. **Building a custom NixOS AMI** tailored for AWS EKS worker nodes
2. **Provisioning an EKS cluster** with a managed control plane and NixOS workers
3. **Setting up core cluster services** (NGINX Ingress, TLS via Let's Encrypt) using Nix-friendly Helm charts
4. **Deploying a sample Haskell application** using a multi-stage Dockerfile
5. **Configuring host and network security** declaratively through NixOS and Kubernetes

## 🚀 Project Roadmap & Todo List

### Phase 1: Foundation & Infrastructure ✅
- [x] **Set up repository structure** with directories for nixos, kubernetes, haskell-app, and docs
- [x] **Create NixOS configuration** for EKS worker AMI with security hardening
- [x] **Set up EKS cluster configuration** using eksctl for NixOS nodes
- [ ] **Create Terraform configuration** for infrastructure as code

### Phase 2: Core Services & Security
- [ ] **Set up NGINX Ingress Controller** and cert-manager with Let's Encrypt
- [ ] **Create Kubernetes Network Policies** for security
- [ ] **Add monitoring and logging** with Prometheus/Grafana and centralized logging

### Phase 3: Application Development
- [ ] **Create sample Haskell Scotty application** with Dockerfile
- [ ] **Create Kubernetes deployment manifests** for the Haskell app
- [ ] **Set up CI/CD pipeline** with GitHub Actions for automated builds and deployments

### Phase 4: Advanced Features
- [ ] **Create Nix Flake** for reproducible development environment
- [x] **Add comprehensive documentation** with step-by-step instructions
- [x] **Implement advanced security policies** and hardening

## ⚙️ Prerequisites

- **AWS Account** with appropriate permissions (EC2, EKS, IAM, VPC)
- **AWS CLI** configured on your local machine (`aws configure`)
- **`kubectl`** and **`eksctl`** installed locally
- **Nix** installed on your local machine (`sh <(curl -L https://nixos.org/nix/install)`)
- **Docker** for building the application container

## 🏗️ Repository Structure

```
aws-eks-nixos-config/
├── nixos/                    # NixOS configurations
│   ├── modules/             # Custom NixOS modules
│   └── configs/             # NixOS configuration files
├── kubernetes/              # Kubernetes manifests
│   ├── manifests/           # YAML manifests
│   └── helm/               # Helm chart configurations
├── haskell-app/            # Sample Haskell application
│   ├── src/                # Haskell source code
│   └── app/                # Application configuration
├── terraform/              # Infrastructure as Code
├── monitoring/             # Monitoring and logging configs
├── scripts/                # Utility scripts
├── docs/                   # Documentation
└── .github/workflows/      # CI/CD pipelines
```

## 🔧 Quick Start

1. **Clone the repository:**
   ```bash
   git clone git@github.com:kae3g/aws-eks-nixos-config.git
   cd aws-eks-nixos-config
   git checkout dev  # Use the dev branch with latest features
   ```

2. **Set up your development environment:**
   ```bash
   # Install Nix (if not already installed)
   sh <(curl -L https://nixos.org/nix/install)
   
   # Enter the development shell
   nix develop
   ```

3. **Configure AWS credentials:**
   ```bash
   aws configure
   ```

4. **Set up AWS prerequisites (automated):**
   ```bash
   ./scripts/setup-aws-prerequisites.sh
   ```

5. **Build your NixOS AMI:**
   ```bash
   cd nixos/
   ./scripts/build-ami.sh
   ```

6. **Create your EKS cluster:**
   ```bash
   # Update AMI ID in cluster config
   AMI_ID=$(cat /tmp/ami-id.txt)
   sed -i "s/ami-xxxxxxxxx/$AMI_ID/g" ../kubernetes/eks-cluster.yaml
   
   # Create the cluster
   eksctl create cluster -f ../kubernetes/eks-cluster.yaml
   ```

7. **Verify your cluster:**
   ```bash
   kubectl get nodes
   kubectl get pods -A
   ```

## ✨ What's Been Implemented

### 🔒 Security-First NixOS Configuration
- **Comprehensive Security Hardening**: AppArmor, auditd, firewall rules, and system hardening
- **Container Runtime**: Containerd with CRI v1 and proper CNI configuration
- **Kubernetes Integration**: Kubelet service optimized for EKS
- **AWS Integration**: SSM Agent and CLI tools for seamless AWS connectivity

### 🚀 Automated Setup Scripts
- **AMI Build Script**: Automated NixOS AMI creation with error handling
- **AWS Prerequisites**: One-command setup for all required AWS resources
- **EKS Cluster Config**: Complete eksctl configuration for NixOS worker nodes

### 📚 Comprehensive Documentation
- **Step-by-step guides** for every component
- **Security best practices** and hardening techniques
- **Troubleshooting guides** and debugging tips
- **Cost optimization** recommendations

## 📚 Documentation

- [AWS Setup Guide](docs/aws-setup-guide.md) - Complete AWS resource setup and configuration
- [NixOS Configuration](docs/nixos-config.md) - Custom NixOS modules and security hardening
- [EKS Cluster Setup](kubernetes/eks-cluster.yaml) - eksctl configuration for NixOS nodes
- [Build Scripts](scripts/) - Automated AMI building and AWS setup

## 🤝 Contributing

1. Fork the repository
2. Create a feature branch (`git checkout -b feature/amazing-feature`)
3. Commit your changes (`git commit -m 'Add some amazing feature'`)
4. Push to the branch (`git push origin feature/amazing-feature`)
5. Open a Pull Request

## 📄 License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.

## 🙏 Acknowledgments

- [DeterminateSystems/nixos-eks-ami](https://github.com/DeterminateSystems/nixos-eks-ami) for the NixOS EKS AMI builder
- The NixOS community for excellent documentation and examples
- The Kubernetes community for robust container orchestration tools
