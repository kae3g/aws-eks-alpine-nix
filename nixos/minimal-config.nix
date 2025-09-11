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
  
  # Create a simple user
  users.users.nixos = {
    isNormalUser = true;
    extraGroups = [ "wheel" "docker" ];
    shell = pkgs.zsh;
  };
  
  # Enable sudo for the nixos user
  security.sudo.wheelNeedsPassword = false;
  
  # Basic networking
  networking.hostName = "nixos-minimal";
  
  # Enable SSH (simple setup)
  services.openssh = {
    enable = true;
    settings = {
      PermitRootLogin = "no";
      PasswordAuthentication = false;
      PubkeyAuthentication = true;
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
