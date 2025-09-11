{
  description = "NixOS configuration for AWS EKS worker nodes";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-24.05";
    nixos-eks-ami.url = "github:DeterminateSystems/nixos-eks-ami";
  };

  outputs = { self, nixpkgs, nixos-eks-ami, ... }@inputs:
    let
      system = "x86_64-linux";
      pkgs = nixpkgs.legacyPackages.${system};
    in
    {
      nixosConfigurations.eks-worker = nixpkgs.lib.nixosSystem {
        inherit system;
        modules = [
          # Base EKS AMI configuration
          nixos-eks-ami.nixosModules.aws-eks
          # Our custom security hardening
          ./modules/eks-security.nix
          # Main configuration
          ./configs/eks-worker.nix
        ];
      };
    };
}
