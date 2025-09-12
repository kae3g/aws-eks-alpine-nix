# NixOS on AWS EKS: A Professional Reference for Declarative Infrastructure 💛

[![License: Unlicense](https://img.shields.io/badge/License-Unlicense-blue.svg)](https://unlicense.org/)
[![AWS](https://img.shields.io/badge/AWS-Provider-orange.svg)](https://aws.amazon.com/)
[![NixOS](https://img.shields.io/badge/NixOS-24.11-blue.svg)](https://nixos.org/)
[![Professional Reference](https://img.shields.io/badge/Standard-Professional%20Reference-gold.svg)](https://github.com/kae3g/aws-eks-nixos-config)

A comprehensive, professional-grade reference implementation for deploying secure, reproducible Kubernetes clusters on AWS EKS with **NixOS worker nodes**, orchestrated by Terraform. This repository establishes the gold standard for declarative infrastructure documentation, combining theoretical excellence with practical implementation. 💛

## Professional Reference Standard 💛

This repository represents more than a tutorial—it is a **professional reference standard** for declarative functional container orchestration. Every component is designed to demonstrate not just *how* to build infrastructure, but *why* modern security practices and declarative principles are essential for production systems.

> **For Engineering Teams:** This project establishes a benchmark for infrastructure documentation quality. It teaches modern authentication methods (SSO), security-first design principles, and the importance of comprehensive documentation that explains both implementation and philosophy.

## Purpose & Vision 💛

To provide a **definitive guide** from a single, declarative NixOS instance to a full AWS EKS cluster, where every element—from the host operating system to the application deployment—is defined, versioned, and repeatable. This is not just infrastructure; it's a **philosophy of building systems** that are beautiful, secure, maintainable, and professionally documented.

### Professional Standards Achieved

- ✅ **Comprehensive Documentation**: Every step explained with context and rationale
- ✅ **Security-First Design**: Modern authentication (SSO) and defense-in-depth principles
- ✅ **Educational Excellence**: Teaching *why* alongside *how* for deeper understanding
- ✅ **Production Readiness**: Designed to scale from learning to enterprise deployment
- ✅ **Quality Benchmark**: Sets the standard for infrastructure documentation excellence

## Core Principles 💛

*   **🔒 Security First:** NixOS nodes configured with root logins disabled, permitting only SSH key access, with firewalls restricting all unnecessary ports. Security is not an afterthought—it's the foundation of professional infrastructure.
*   **📦 Complete Declarativity:** The entire system state is defined in code: the OS with Nix, the infrastructure with Terraform, and the application with Kubernetes manifests. Nothing is left to chance in professional systems.
*   **🔄 Reproducibility:** Every deployment is an identical recreation of a defined state, ensuring consistency from development to production. What works once will work always—the hallmark of professional infrastructure.
*   **🌱 Designed for Growth:** The structure is crafted to begin simply and scale elegantly into a complex, production-ready platform. Professional systems start small and dream big.
*   **📚 Documentation Excellence:** Every component is thoroughly documented with context, rationale, and educational value. Professional infrastructure requires professional communication.

## Repository Structure 💛

```
aws-eks-nixos-config/
├── terraform-minimal/     # ✅ CURRENT FOCUS: Single NixOS node for learning & validation
│   ├── main.tf           # - Core Terraform resources (VPC, EC2, SG)
│   ├── variables.tf      # - Configurable parameters
│   ├── outputs.tf        # - Useful output values (IPs, IDs)
│   └── terraform.tfvars.example  # - Example configuration
├── dev-advanced/         # 🚧 NEXT PHASE: Full EKS cluster with NixOS nodes
├── SETUP-GUIDE.md        # - Professional step-by-step reference guide
├── PROGRESS.md           # - Engineering logbook and milestone tracking
└── README.md             # - Professional reference overview 💛
```

## Beginning Your Professional Journey 💛

The established path begins with the `terraform-minimal` configuration—a single, secure NixOS instance that serves as both a learning tool and a foundation for professional infrastructure development.

1. **Prerequisites:** Ensure you have the [AWS CLI](https://aws.amazon.com/cli/), [Terraform](https://www.terraform.io/), and [Nix](https://nixos.org/) prepared on your system.
2. **Follow the Professional Guide:** Comprehensive, step-by-step instructions are in the [**SETUP-GUIDE.md**](./SETUP-GUIDE.md).
3. **Deploy with Confidence:** Navigate to the `terraform-minimal/` directory and execute the deployment commands.
4. **Track Professional Progress:** Maintain your engineering journey in [**PROGRESS.md**](./PROGRESS.md).

> **Modern Authentication Standard:** This project demonstrates AWS Single Sign-On (SSO) as the professional standard for secure cloud access. SSO provides temporary, secure tokens that expire automatically, establishing the security baseline for enterprise infrastructure.

## Current Status 💛

| Phase | Status | Description |
| :--- | :--- | :--- |
| **Phase 0: Foundation** | ✅ **Complete** | AWS Account, CLI, Billing, SSH Keys |
| **Phase 1: Minimal Node** | ✅ **Code Complete** | Single, secure NixOS EC2 instance |
| **Phase 2: EKS Cluster** | 🚧 **In Progress** | EKS control plane with NixOS node group |
| **Phase 3: App Deployment** | 📋 **Planned** | Haskell application deployment with CI/CD |
| **Phase 4: Advanced Patterns** | 📋 **Planned** | Service Mesh, GitOps, Monitoring |

**📋 See the detailed professional project log and next actions in [PROGRESS.md](./PROGRESS.md).** 💛

## Professional Standards Demonstrated 💛

By engaging with this repository, you'll experience:

- ✅ **NixOS host** with Zsh shell and Haskell (no root access, SSH keys only)
- ✅ **One container** with the same NixOS environment (non-root user, immutable)
- ✅ **Ephemeral infrastructure** (disposable, not eternal)
- ✅ **Persistent connections** with Mosh
- ✅ **Complete automation** with Terraform
- ✅ **Industry-standard security** throughout
- ✅ **Professional documentation** that teaches context and rationale
- ✅ **Modern authentication** practices (SSO over static credentials)

## Professional Philosophy 💛

**Infrastructure should be ephemeral, not eternal—everything should be disposable and replaceable!**

This project embodies the professional principle that the best systems are those that can be destroyed and recreated at will. We build not for permanence, but for reliability through reproducibility. Every component is designed to be understood, modified, and replaced with confidence—the foundation of professional infrastructure engineering.

> **Professional Security Philosophy:** We demonstrate modern authentication methods (SSO) that provide temporary credentials instead of long-lived access keys. This establishes the security baseline for professional cloud infrastructure and dramatically reduces the risk of credential leaks.

**Less is more, but make it work perfectly!** ✨

## Professional Reference Value 💛

This repository serves as a **professional reference standard** for:

- **Infrastructure Documentation Excellence**: Comprehensive guides that explain both implementation and philosophy
- **Modern Security Practices**: SSO authentication, defense-in-depth, and least-privilege principles
- **Declarative Infrastructure**: Complete system state defined in code with version control
- **Educational Engineering**: Teaching *why* alongside *how* for deeper professional understanding
- **Production Readiness**: Designed to scale from learning to enterprise deployment

## Contributing to Professional Standards 💛

This repository establishes a benchmark for infrastructure documentation quality. Contributions should maintain the high standard of:

- **Comprehensive Documentation**: Every change documented with context and rationale
- **Educational Value**: Teaching principles alongside implementation
- **Security First**: Maintaining modern authentication and security practices
- **Professional Communication**: Clear, precise, and context-rich explanations

Please open an issue first to discuss any significant changes. The goal is to maintain and elevate the professional reference standard.

## License 💛

This project is released into the public domain under The Unlicense - see the [LICENSE](LICENSE) file for details. You are free to use, modify, distribute, and even sell this software for any purpose, commercial or non-commercial, without any restrictions.

---

*Built with 💛 for the love of declarative infrastructure, professional documentation excellence, and beautiful systems that set the standard for quality engineering communication.*