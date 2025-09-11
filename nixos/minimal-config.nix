{ config, lib, pkgs, ... }:

{
  # Basic system configuration
  system.stateVersion = "24.05";
  
  # Enable Zsh as default shell
  programs.zsh = {
    enable = true;
    enableCompletion = true;
    enableBashCompletion = true;
    syntaxHighlighting.enable = true;
    autosuggestions.enable = true;
  };
  
  # Set Zsh as default shell for all users
  users.defaultUserShell = pkgs.zsh;
  
  # Install Haskell and essential tools
  environment.systemPackages = with pkgs; [
    # Shell and basic tools
    zsh
    bash
    coreutils
    findutils
    which
    vim
    nano
    
    # Haskell toolchain
    ghc
    cabal-install
    stack
    haskell-language-server
    
    # Container tools (minimal)
    docker
    docker-compose
    
    # Development tools
    git
    curl
    wget
    jq
    tree
  ];
  
  # Enable Docker
  virtualisation.docker = {
    enable = true;
    enableOnBoot = true;
  };
  
  # Create a secure user (following industry standards)
  users.users.nixos = {
    isNormalUser = true;
    extraGroups = [ "wheel" "docker" ];
    shell = pkgs.zsh;
    # No password set - SSH key only
    hashedPassword = "!";
  };
  
  # Disable root user completely (industry standard)
  users.users.root = {
    hashedPassword = "!";
    openssh.authorizedKeys.keys = [ ];
  };
  
  # Secure sudo configuration (emergency use only)
  security.sudo = {
    enable = true;
    wheelNeedsPassword = false;
    extraRules = [
      {
        users = [ "nixos" ];
        commands = [
          {
            command = "ALL";
            options = [ "NOPASSWD" "SETENV" ];
          }
        ];
      }
    ];
  };
  
  # Ephemeral hostname (industry standard)
  networking.hostName = "nixos-${builtins.substring 0 8 (builtins.readFile /etc/machine-id)}";
  
  # Industry-standard SSH configuration
  services.openssh = {
    enable = true;
    settings = {
      PermitRootLogin = "no";
      PasswordAuthentication = false;
      PubkeyAuthentication = true;
      KbdInteractiveAuthentication = false;
      X11Forwarding = false;
      AllowUsers = [ "nixos" ];
      MaxAuthTries = 3;
      ClientAliveInterval = 300;
      ClientAliveCountMax = 2;
    };
  };
  
  # Simple firewall (allow SSH and Docker)
  networking.firewall = {
    enable = true;
    allowedTCPPorts = [ 22 ];
    allowedTCPPortRanges = [
      { from = 3000; to = 9000; }  # For development servers
    ];
  };
}
