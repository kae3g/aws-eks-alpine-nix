{ config, lib, pkgs, ... }:

{
  imports = [
    # Import our security hardening module
    ../modules/eks-security.nix
  ];

  # Basic system configuration
  system.stateVersion = "24.05";
  
  # Enable required services for EKS
  services = {
    # Enable containerd for container runtime
    containerd = {
      enable = true;
      # Configure containerd with proper settings for EKS
      settings = {
        plugins."io.containerd.grpc.v1.cri" = {
          # Enable CRI v1
          disable_cgroup = false;
          disable_apparmor = false;
          restrict_oom_score_adj = true;
          # Configure CNI
          cni = {
            bin_dir = "/opt/cni/bin";
            conf_dir = "/etc/cni/net.d";
          };
        };
      };
    };

    # Enable kubelet service
    kubelet = {
      enable = true;
      # Configure kubelet for EKS
      kubeconfig = "/etc/kubernetes/kubelet/kubeconfig";
      extraArgs = [
        "--container-runtime=remote"
        "--container-runtime-endpoint=unix:///run/containerd/containerd.sock"
        "--cgroup-driver=systemd"
        "--pod-manifest-path=/etc/kubernetes/manifests"
        "--resolv-conf=/run/systemd/resolve/resolv.conf"
      ];
    };
  };

  # AWS-specific configuration
  networking.hostName = "eks-worker";
  
  # Enable AWS-specific services
  services.aws-ssm-agent.enable = true;
  
  # Configure timezone
  time.timeZone = "UTC";
  
  # Enable required kernel modules for EKS
  boot.kernelModules = [
    "br_netfilter"
    "ip_vs"
    "ip_vs_rr"
    "ip_vs_wrr"
    "ip_vs_sh"
    "nf_conntrack"
  ];

  # Configure sysctl for Kubernetes
  boot.kernel.sysctl = {
    "net.bridge.bridge-nf-call-iptables" = 1;
    "net.bridge.bridge-nf-call-ip6tables" = 1;
    "net.ipv4.ip_forward" = 1;
    "net.ipv4.conf.all.forwarding" = 1;
    "net.ipv6.conf.all.forwarding" = 1;
  };

  # Install required packages for EKS
  environment.systemPackages = with pkgs; [
    # Container runtime tools
    containerd
    runc
    cni-plugins
    
    # Kubernetes tools
    kubectl
    kubelet
    
    # AWS tools
    awscli2
    aws-ssm-agent
    
    # Monitoring and debugging tools
    htop
    iotop
    tcpdump
    strace
  ];

  # Create necessary directories
  systemd.tmpfiles.rules = [
    "d /etc/kubernetes/manifests 0755 root root -"
    "d /etc/kubernetes/kubelet 0755 root root -"
    "d /var/lib/kubelet 0755 root root -"
    "d /var/log/kubernetes 0755 root root -"
  ];
}
