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
    };
  };

  # AWS-specific configuration
  networking.hostName = "eks-worker";
}
