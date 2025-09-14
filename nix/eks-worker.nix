# Declarative EKS worker environment
{ pkgs ? import <nixpkgs> {} }:

pkgs.buildEnv {
  name = "eks-worker-environment";
  paths = with pkgs; [
    # Container runtime
    containerd
    runc
    cri-tools

    # Kubernetes components
    kubelet
    kubectl
    kubeadm
    cni-plugins

    # AWS tools
    awscli2
    jq
    curl

    # Monitoring and debugging
    htop
    iotop
    iftop
    tcpdump
    lsof

    # Security tools
    clamav
    rkhunter
    audit

    # Sovereign identity tools
    gnupg
    openssh
    git
  ];

  extraOutputsToInstall = [ "bin" "lib" "out" ];

  postBuild = ''
    # Create containerd configuration
    mkdir -p $out/etc/containerd
    containerd config default > $out/etc/containerd/config.toml

    # Create kubelet service directory
    mkdir -p $out/etc/systemd/system/kubelet.service.d
    cat > $out/etc/systemd/system/kubelet.service.d/10-sovereign.conf << EOF
    [Service]
    Environment="KUBELET_EXTRA_ARGS=--node-labels=sovereignty=b122m-faeb"
    EOF

    # Create sovereign identity directory
    mkdir -p $out/etc/sovereign
    echo "b122m faeb sovereign worker" > $out/etc/sovereign/identity
  '';
}
