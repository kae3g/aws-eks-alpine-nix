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
    
    # Enable PAM security
    pam = {
      enableSSHAgentAuth = true;
      services = {
        sshd = {
          # Disable password authentication
          passwordAuth = false;
          # Enable public key authentication
          publicKeyAuth = true;
        };
      };
    };
    
    # Configure sudo
    sudo = {
      enable = true;
      wheelNeedsPassword = false;
      extraRules = [
        {
          users = [ "root" ];
          commands = [
            {
              command = "ALL";
              options = [ "NOPASSWD" "SETENV" ];
            }
          ];
        }
      ];
    };
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
      # Allow kubelet API
      allowedTCPPorts = [ 10250 ];
      # Allow kube-proxy health check
      allowedTCPPorts = [ 10256 ];
      # Allow CNI
      allowedTCPPorts = [ 6783 ];
      # Allow Calico BGP
      allowedTCPPorts = [ 179 ];
      # Allow Calico Typha
      allowedTCPPorts = [ 5473 ];
    };
    
    # Disable IPv6 for security
    enableIPv6 = false;
    
    # Configure network security
    networkmanager.enable = false;
  };

  # System hardening
  boot = {
    # Enable kernel security features
    kernelParams = [
      "audit=1"
      "slab_nomerge"
      "pti=on"
      "vsyscall=none"
      "selinux=0"
      "apparmor=1"
      "security=apparmor"
      "cgroup_enable=memory"
      "swapaccount=1"
    ];
    
    # Disable swap for Kubernetes
    kernel.sysctl = {
      "vm.swappiness" = 0;
    };
  };

  # User security
  users = {
    # Disable root login
    users.root = {
      hashedPassword = "!";
      openssh.authorizedKeys.keys = [ ];
    };
    
    # Create eks user
    users.eks = {
      isNormalUser = true;
      extraGroups = [ "wheel" "docker" "systemd-journal" ];
      openssh.authorizedKeys.keys = [ ];
    };
  };

  # Services security
  services = {
    # Disable unnecessary services
    avahi.enable = false;
    bluetooth.enable = false;
    cups.enable = false;
    
    # Configure SSH
    openssh = {
      enable = true;
      settings = {
        PermitRootLogin = "no";
        PasswordAuthentication = false;
        PubkeyAuthentication = true;
        X11Forwarding = false;
        AllowUsers = [ "eks" ];
        MaxAuthTries = 3;
        ClientAliveInterval = 300;
        ClientAliveCountMax = 2;
      };
    };
    
    # Enable fail2ban for SSH protection
    fail2ban = {
      enable = true;
      jails = {
        sshd = ''
          enabled = true
          port = ssh
          filter = sshd
          logpath = /var/log/auth.log
          maxretry = 3
          bantime = 3600
        '';
      };
    };
  };

  # File system security
  fileSystems = {
    "/tmp" = {
      device = "tmpfs";
      fsType = "tmpfs";
      options = [ "nosuid" "nodev" "noexec" "size=1G" ];
    };
    "/var/tmp" = {
      device = "tmpfs";
      fsType = "tmpfs";
      options = [ "nosuid" "nodev" "noexec" "size=1G" ];
    };
  };

  # Environment security
  environment = {
    # Remove dangerous packages
    systemPackages = with pkgs; [
      # Keep only essential packages
    ];
    
    # Set secure environment variables
    variables = {
      TMPDIR = "/tmp";
    };
  };

  # Systemd security
  systemd = {
    # Disable coredumps
    coredump.enable = false;
    
    # Configure journal security
    journald = {
      extraConfig = ''
        SystemMaxUse=1G
        SystemMaxFileSize=100M
        MaxRetentionSec=1month
        ForwardToSyslog=no
        ForwardToKMsg=no
        ForwardToConsole=no
        ForwardToWall=no
      '';
    };
  };
}
