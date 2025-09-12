# NixOS on AWS EKS: A Declarative Infrastructure Stack 💛

[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
[![AWS](https://img.shields.io/badge/AWS-Provider-orange.svg)](https://aws.amazon.com/)
[![NixOS](https://img.shields.io/badge/NixOS-24.11-blue.svg)](https://nixos.org/)

A complete, declarative foundation for a secure and reproducible Kubernetes cluster on AWS EKS, built with **NixOS worker nodes** and orchestrated by Terraform. This repository embodies the principles of Infrastructure as Code and immutable infrastructure, designed for clarity and reliability. 💛

## Purpose 💛

To provide a guided path from a single, declarative NixOS instance to a full AWS EKS cluster, where every element—from the host operating system to the application deployment—is defined, versioned, and repeatable. This is not just infrastructure; it's a philosophy of building systems that are beautiful, secure, and maintainable.

## Core Principles 💛

*   **🔒 Security First:** NixOS nodes are configured with root logins disabled, permitting only SSH key access, with firewalls restricting all unnecessary ports. Security is not an afterthought—it's the foundation.
*   **📦 Complete Declarativity:** The entire system state is defined in code: the OS with Nix, the infrastructure with Terraform, and the application with Kubernetes manifests. Nothing is left to chance.
*   **🔄 Reproducibility:** Every deployment is an identical recreation of a defined state, ensuring consistency from development to production. What works once will work always.
*   **🌱 Designed for Growth:** The structure is crafted to begin simply and scale elegantly into a complex, production-ready platform. Start small, dream big.

## Repository Structure 💛

```
aws-eks-nixos-config/
├── terraform-minimal/     # ✅ CURRENT FOCUS: Single NixOS node for learning & validation
│   ├── main.tf           # - Core Terraform resources (VPC, EC2, SG)
│   ├── variables.tf      # - Configurable parameters
│   ├── outputs.tf        # - Useful output values (IPs, IDs)
│   └── terraform.tfvars.example  # - Example configuration
├── dev-advanced/         # 🚧 NEXT PHASE: Full EKS cluster with NixOS nodes
├── SETUP-GUIDE.md        # - Step-by-step instructions to get started
├── PROGRESS.md           # - Project logbook and completed milestones
└── README.md             # - You are here 💛
```

## Beginning Your Journey 💛

The established path begins with the `terraform-minimal` configuration—a single, secure NixOS instance that serves as both a learning tool and a foundation for what comes next.

1. **Prerequisites:** Ensure you have the [AWS CLI](https://aws.amazon.com/cli/), [Terraform](https://www.terraform.io/), and [Nix](https://nixos.org/) prepared on your system.
2. **Follow the Guide:** Detailed, step-by-step instructions are in the [**SETUP-GUIDE.md**](./SETUP-GUIDE.md).
3. **Deploy:** Navigate to the `terraform-minimal/` directory and execute the deployment commands.
4. **Track Progress:** Keep your journey documented in [**PROGRESS.md**](./PROGRESS.md).

## Current Status 💛

| Phase | Status | Description |
| :--- | :--- | :--- |
| **Phase 0: Foundation** | ✅ **Complete** | AWS Account, CLI, Billing, SSH Keys |
| **Phase 1: Minimal Node** | ✅ **Code Complete** | Single, secure NixOS EC2 instance |
| **Phase 2: EKS Cluster** | 🚧 **In Progress** | EKS control plane with NixOS node group |
| **Phase 3: App Deployment** | 📋 **Planned** | Haskell application deployment with CI/CD |
| **Phase 4: Advanced Patterns** | 📋 **Planned** | Service Mesh, GitOps, Monitoring |

**📋 See the detailed project log and next actions in [PROGRESS.md](./PROGRESS.md).** 💛

## What You'll Build 💛

By the end of this journey, you'll have:

- ✅ **NixOS host** with Zsh shell and Haskell (no root access, SSH keys only)
- ✅ **One container** with the same NixOS environment (non-root user, immutable)
- ✅ **Ephemeral infrastructure** (disposable, not eternal)
- ✅ **Persistent connections** with Mosh
- ✅ **Complete automation** with Terraform
- ✅ **Industry-standard security** throughout

## Philosophy 💛

**Infrastructure should be ephemeral, not eternal—everything should be disposable and replaceable!**

This project embodies the principle that the best systems are those that can be destroyed and recreated at will. We build not for permanence, but for reliability through reproducibility. Every component is designed to be understood, modified, and replaced with confidence.

**Less is more, but make it work perfectly!** ✨

## Contributing 💛

This is primarily a personal learning project, but suggestions and discussions are welcome! Please open an issue first to discuss any significant changes. The goal is to create something that others can learn from and build upon.

## License 💛

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.

---

*Built with 💛 for the love of declarative infrastructure and beautiful systems.*