{ config, lib, pkgs, ... }:

with lib;

{
  # Enable security hardening features
  security = {
    # Enable AppArmor for container security
    apparmor.enable = true;
    
    # Enable auditd for security auditing
    auditd.enable = true;
    
    # Disable core dumps for security
    coreDump.enable = false;
  };

  # Network security hardening
  networking = {
    # Configure firewall
    firewall = {
      enable = true;
      # Allow SSH
      allowedTCPPorts = [ 22 ];
      # Allow Kubernetes API server
      allowedTCPPorts = [ 6443 ];
      # Allow node port range
      allowedTCPPortRanges = [
        { from = 30000; to = 32767; }
      ];
    };
  };

  # System hardening
  boot = {
    # Enable kernel security features
    kernelParams = [
      "audit=1"
      "slab_nomerge"
      "pti=on"
      "vsyscall=none"
    ];
  };
}
