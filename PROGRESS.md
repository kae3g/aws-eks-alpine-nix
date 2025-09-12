# Project Progress Log 💛

A clear record of achievements, current status, and the path forward. This is your engineering logbook—a place to track not just what you've done, but why you've done it and what comes next. 💛

## 📍 Current Position

The foundational code for Phase 1 is complete and awaits its initial deployment. The local environment is prepared, and the AWS account is configured with IAM Identity Center. We stand at the threshold of seeing our declarative infrastructure come to life. 💛

## ✅ Achieved Milestones

### Phase 0: Foundation ✅
- [x] **Tool Installation**: AWS CLI (v2.29.1), Terraform (v1.5.7), Mosh (v1.4.0) installed and verified
- [x] **SSH Key Generation**: ED25519 key with 100 rounds of KDF created (`id_ed25519_aws_nixos`)
- [x] **AWS Account Setup**: New AWS account created with billing alerts configured
- [x] **IAM Identity Center**: Enabled with AWS Organizations (instance ID: 7223ed32f18fae8a)
- [x] **User & Permissions**: Created 'admin' group, user 'kae3g', and 'AdministratorAccess' permission set
- [x] **MFA Setup**: Authenticator app registered for user 'kae3g'
- [x] **AWS CLI Configuration**: Configured to use IAM Identity Center SSO
- [x] **SSH Key Import**: Public key imported to AWS as 'nixos-key'

### Phase 1: Minimal Node Implementation ✅
- [x] **Repository Structure**: Established clear separation between `terraform-minimal` (current focus) and `dev-advanced` (future EKS setup)
- [x] **Terraform Code**: Composed `main.tf` that utilizes dynamic AMI lookup, configures essential security groups, and provides useful outputs
- [x] **NixOS Configuration**: Defined comprehensive `configuration.nix` that enforces security policies, sets up user environment with Zsh and Home Manager, and declares necessary system packages
- [x] **Documentation**: Prepared comprehensive setup guide and progress tracking system
- [x] **Security Design**: Implemented root login disabled, SSH keys only, firewall configuration, and non-root user setup

## 🧭 Immediate Next Actions

The immediate steps to take are these:

- [ ] **Execute Deployment**: Run `terraform apply` for the minimal setup
- [ ] **Validate Access**: Confirm SSH and Mosh connectivity to the provisioned instance
- [ ] **Verify Configuration**: Confirm operational status of declared user environment and tools (Zsh, Haskell, Mosh)
- [ ] **Test Lifecycle**: Run `terraform destroy` followed by `terraform apply` to prove full reproducibility
- [ ] **Document Results**: Update this progress log with deployment outcomes and any lessons learned

## 📋 Future Roadmap Items

### Phase 2: EKS Cluster Development 🚧
- [ ] **Research EKS Integration**: Study AWS EKS best practices and NixOS worker node requirements
- [ ] **Custom AMI Builder**: Implement `nixos-eks-ami` builder for creating EKS-compatible NixOS AMIs
- [ ] **EKS Control Plane**: Terraform code for EKS cluster creation
- [ ] **NixOS Node Group**: Terraform code for EKS node group using custom NixOS AMI
- [ ] **Security Hardening**: Implement least-privilege IAM policies and security group configurations

### Phase 3: Application Deployment 📋
- [ ] **CI/CD Pipeline**: Develop GitHub Actions workflow for automated Terraform plans and applies
- [ ] **Sample Application**: Create Haskell web application with Dockerfile
- [ ] **Kubernetes Manifests**: Write deployment, service, and ingress configurations
- [ ] **Helm Charts**: Create reusable Helm charts for application deployment
- [ ] **Monitoring Setup**: Implement logging and monitoring solutions

### Phase 4: Advanced Patterns 📋
- [ ] **Service Mesh**: Integrate Istio or Linkerd for service-to-service communication
- [ ] **GitOps**: Implement ArgoCD or Flux for declarative application deployment
- [ ] **Observability**: Set up Prometheus, Grafana, and distributed tracing
- [ ] **Security Scanning**: Implement container and infrastructure security scanning
- [ ] **Disaster Recovery**: Design backup and recovery procedures

## 📖 Project Log

### 2025-09-11 💛
**The Foundation is Laid**

The initial construction of the repository is complete. The Terraform and NixOS code for a single node has been written to reflect best practices in security and declarative configuration. The focus now shifts to the first deployment and validation of this foundation.

**Key Decisions Made:**
- **NixOS 24.11**: Selected the latest stable release for better long-term support and package versions
- **ED25519 SSH Keys**: Employed for maximum security with 100 rounds of KDF
- **Mosh UDP Range**: Defined 60000-61000 range balancing functionality with security intention
- **IAM Permissions**: Using `AdministratorAccess` initially for learning simplicity, with note to refine in Phase 2
- **Repository Structure**: Clear separation between minimal and advanced configurations for progressive complexity

**Current Status:**
- All prerequisites completed
- AWS account fully configured with IAM Identity Center
- Terraform code ready for deployment
- Documentation comprehensive and user-friendly

**Next Milestone:**
First successful deployment of the minimal NixOS instance, followed by validation of all declared configurations.

## 💡 Lessons Learned & Insights

### Technical Insights
- **IAM Identity Center** provides superior security and user management compared to traditional IAM users
- **ED25519 SSH keys** offer better security and performance than RSA keys
- **Declarative configuration** requires upfront investment but pays dividends in reproducibility
- **Security-first design** prevents common vulnerabilities from the start

### Process Insights
- **Documentation as code** ensures knowledge is preserved and shareable
- **Progressive complexity** allows learning without overwhelming
- **Version control everything** enables experimentation with confidence
- **Clear separation of concerns** makes the system understandable and maintainable

## 🎯 Success Metrics

### Phase 1 Success Criteria
- [ ] Single NixOS instance deploys successfully via Terraform
- [ ] SSH and Mosh connectivity established and verified
- [ ] All declared packages (Haskell, Zsh, Mosh) installed and functional
- [ ] Security configurations (no root login, SSH keys only) enforced
- [ ] Full lifecycle test (destroy → apply) completes successfully

### Long-term Success Criteria
- [ ] Complete EKS cluster with NixOS worker nodes
- [ ] Production-ready application deployment pipeline
- [ ] Comprehensive monitoring and observability
- [ ] Security scanning and compliance validation
- [ ] Documentation that enables others to replicate the setup

---

*This progress log is a living document. Update it regularly with achievements, challenges, and insights. It serves not just as a record of what has been done, but as a guide for what comes next.* 💛

*The journey continues, one step at a time, with each step building upon the last.* 💛