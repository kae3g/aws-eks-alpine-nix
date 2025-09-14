# NixOS Configuration for AWS EKS Worker Nodes
![Parametric Flower](parametric-flower-compressed.png)

This document describes the NixOS configuration for AWS EKS worker nodes,
including security hardening, container runtime setup, and Kubernetes
integration.

## Overview

Our NixOS configuration is designed to create a secure, optimized AMI for AWS
EKS worker nodes. The configuration includes:

- **Security Hardening**: Comprehensive security measures including AppArmor,
auditd, firewall rules, and system hardening
- **Container Runtime**: Containerd with proper CRI configuration for Kubernetes
- **Kubernetes Integration**: Kubelet service with EKS-optimized settings
- **AWS Integration**: AWS SSM Agent and CLI tools
- **Monitoring Tools**: Essential debugging and monitoring utilities

## Configuration Structure

```
nixos/
├── flake.nix                 # Nix flake configuration
├── configs/
│   └── eks-worker.nix        # Main EKS worker configuration
└── modules/
    └── eks-security.nix      # Security hardening module
```

## Main Configuration (`configs/eks-worker.nix`)

### Container Runtime (Containerd)

The configuration sets up containerd as the container runtime with:

- **CRI v1**: Enabled with proper settings for Kubernetes
- **CNI Integration**: Configured for container networking
- **Security**: AppArmor and OOM score restrictions enabled
- **Cgroup Driver**: Systemd for proper resource management

### Kubelet Service

The kubelet is configured with:

- **Container Runtime**: Remote containerd endpoint
- **Cgroup Driver**: Systemd for resource management
- **Pod Manifest Path**: `/etc/kubernetes/manifests`
- **Resolv Conf**: Systemd-resolved for DNS resolution

### Kernel Modules

Essential kernel modules for Kubernetes:

- `br_netfilter`: Bridge netfilter for iptables rules
- `ip_vs*`: IP Virtual Server for load balancing
- `nf_conntrack`: Connection tracking for NAT

### Sysctl Configuration

Network and security settings:

- Bridge netfilter for iptables
- IP forwarding enabled
- IPv6 forwarding enabled

### System Packages

Essential packages included:

- **Container Tools**: containerd, runc, cni-plugins
- **Kubernetes Tools**: kubectl, kubelet
- **AWS Tools**: awscli2, aws-ssm-agent
- **Monitoring**: htop, iotop, tcpdump, strace

## Security Module (`modules/eks-security.nix`)

### Security Features

- **AppArmor**: Container security profiles
- **Auditd**: Security event auditing
- **Core Dumps**: Disabled for security
- **PAM**: Secure authentication configuration

### Network Security

- **Firewall**: Comprehensive iptables rules
- **SSH**: Hardened configuration with key-only authentication
- **Fail2ban**: SSH brute force protection
- **IPv6**: Disabled for security

### System Hardening

- **Kernel Parameters**: Security-focused boot options
- **Swap**: Disabled for Kubernetes
- **File Systems**: Secure tmpfs mounts
- **Services**: Unnecessary services disabled

### User Security

- **Root Login**: Disabled
- **EKS User**: Created with proper permissions
- **SSH Keys**: Public key authentication only

## Building the AMI

### Prerequisites

1. **AWS CLI**: Configured with appropriate permissions
2. **Nix**: Installed on your local machine
3. **AWS Credentials**: Properly configured

### Build Process

1. **Navigate to the nixos directory**:
   ```bash
   cd nixos/
   ```

2. **Build the configuration**:
   ```bash
   nix build .#nixosConfigurations.eks-worker.config.system.build.toplevel
   ```

3. **Use the build script**:
   ```bash
   ./scripts/build-ami.sh
   ```

### Build Script Options

The build script supports several environment variables:

- `AWS_REGION`: AWS region (default: us-west-2)
- `INSTANCE_TYPE`: EC2 instance type (default: t3.medium)
- `AMI_NAME`: Custom AMI name (default: auto-generated)

Example:
```bash
AWS_REGION=us-east-1 INSTANCE_TYPE=t3.large ./scripts/build-ami.sh
```

## Security Considerations

### Network Security

- **Firewall Rules**: Only necessary ports are open
- **SSH Hardening**: Key-only authentication, fail2ban protection
- **Network Isolation**: IPv6 disabled, unnecessary services removed

### Container Security

- **AppArmor**: Container security profiles enabled
- **Cgroup Restrictions**: OOM score restrictions
- **Runtime Security**: Containerd with security features

### System Security

- **Kernel Hardening**: Security-focused kernel parameters
- **File System Security**: Secure tmpfs mounts
- **Audit Logging**: Comprehensive security event logging

## Monitoring and Debugging

### Included Tools

- **htop**: Process monitoring
- **iotop**: I/O monitoring
- **tcpdump**: Network packet analysis
- **strace**: System call tracing

### Logging

- **Systemd Journal**: Centralized logging
- **Audit Logs**: Security event logging
- **Kubernetes Logs**: Container and pod logs

## Troubleshooting

### Common Issues

1. **Build Failures**: Check AWS credentials and permissions
2. **Container Runtime Issues**: Verify containerd configuration
3. **Network Issues**: Check firewall rules and kernel modules
4. **Authentication Issues**: Verify SSH key configuration

### Debugging Commands

```bash
# Check system status
systemctl status containerd kubelet

# View logs
journalctl -u containerd -u kubelet

# Check network configuration
ip addr show
iptables -L

# Check kernel modules
lsmod | grep -E "(br_netfilter|ip_vs|nf_conntrack)"
```

## Customization

### Adding Packages

To add additional packages, modify the `environment.systemPackages` section in
`configs/eks-worker.nix`:

```nix
environment.systemPackages = with pkgs; [
  # Existing packages...
  your-package
];
```

### Modifying Security Settings

Security settings can be customized in `modules/eks-security.nix`. Be careful
when modifying security settings as they can impact system security.

### Network Configuration

Network settings can be modified in the `networking` section of the security
module. Ensure that any changes maintain the security posture of the system.

## Best Practices

1. **Regular Updates**: Keep the NixOS configuration updated
2. **Security Audits**: Regularly review security settings
3. **Testing**: Test configurations in non-production environments
4. **Documentation**: Keep documentation updated with changes
5. **Monitoring**: Implement comprehensive monitoring and alerting

## References

- [NixOS Manual](https://nixos.org/manual/nixos/stable/)
- [Kubernetes Documentation](https://kubernetes.io/docs/)
- [AWS EKS Documentation](https://docs.aws.amazon.com/eks/)
- [Containerd Documentation](https://containerd.io/docs/)
