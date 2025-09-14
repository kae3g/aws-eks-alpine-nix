# Sovereign AMI: Alpine Linux + Nix for EKS workers
variable "gpg_fingerprint" {
  type    = string
  default = env("GPG_FINGERPRINT")
}

variable "ssh_public_key" {
  type    = string
  default = env("SSH_PUBLIC_KEY")
}

source "amazon-ebs" "alpine-nix-eks" {
  ami_name        = "alpine-nix-eks-worker-{{timestamp}}"
  instance_type   = "t3.micro"
  region          = "us-west-2"
  ssh_username    = "root"
  
  source_ami_filter {
    filters = {
      name                = "alpine-*-x86_64"
      architecture        = "x86_64"
      virtualization-type = "hvm"
    }
    owners      = ["538276064493"] # Alpine official
    most_recent = true
  }
}

build {
  sources = ["source.amazon-ebs.alpine-nix-eks"]

  # Provision identity verification
  provisioner "shell" {
    inline = [
      "echo 'Verifying build with GPG key: ${var.gpg_fingerprint}'",
      "apk add --no-cache gnupg"
    ]
  }

  # Install Nix package manager
  provisioner "shell" {
    script = "scripts/install-nix.sh"
  }

  # Configure EKS worker environment
  provisioner "shell" {
    script = "scripts/configure-eks.sh"
  }

  # Install sovereign identity tools
  provisioner "shell" {
    inline = [
      "nix-env -i -f nix/eks-worker.nix",
      "mkdir -p /etc/ssh/ssh_keys",
      "echo '${var.ssh_public_key}' >> /etc/ssh/ssh_keys/sovereign_key.pub"
    ]
  }

  # Harden SSH configuration
  provisioner "shell" {
    inline = [
      "echo 'Port 4922' >> /etc/ssh/sshd_config",
      "echo 'PasswordAuthentication no' >> /etc/ssh/sshd_config",
      "echo 'PubkeyAuthentication yes' >> /etc/ssh/sshd_config",
      "echo 'AllowUsers root' >> /etc/ssh/sshd_config"
    ]
  }

  # Final verification
  provisioner "shell" {
    inline = [
      "echo 'Sovereign AMI build complete'",
      "echo 'GPG identity: ${var.gpg_fingerprint}'",
      "date > /etc/sovereign-build-date"
    ]
  }

  post-processor "manifest" {
    output = "manifest.json"
    strip_path = true
  }
}