{
  description = "Minimal NixOS configuration for development";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-24.11";  # Updated to latest stable as of 2025-09-11
  };

  outputs = { self, nixpkgs, ... }@inputs:
    let
      system = "x86_64-linux";
      pkgs = nixpkgs.legacyPackages.${system};
    in
    {
      nixosConfigurations.minimal = nixpkgs.lib.nixosSystem {
        inherit system;
        modules = [
          ./minimal-config.nix
        ];
      };
      
      # Development shell
      devShells.${system}.default = pkgs.mkShell {
        buildInputs = with pkgs; [
          # Haskell tools
          ghc
          cabal-install
          stack
          haskell-language-server
          
          # Container tools
          docker
          docker-compose
          
          # Development tools
          git
          curl
          jq
        ];
        
        shellHook = ''
          echo "🚀 Welcome to NixOS minimal development environment!"
          echo "📦 Available tools: ghc, cabal, stack, docker, git"
          echo "💡 Run 'nixos-rebuild switch' to apply changes"
        '';
      };
    };
}
