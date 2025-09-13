# Alpine Linux + Nix EKS Worker AMI Builder 💙
# This Packer template creates a custom Amazon Machine Image (AMI)
# that combines Alpine Linux's security and minimalism with Nix's
# declarative package management for EKS worker nodes

# Variables for customization
variable "aws_region" {
  type        = string
  description = "AWS region where the AMI will be created"
  default     = "us-east-1"
}

variable "alpine_version" {
  type        = string
  description = "Alpine Linux version to use as base"
  default     = "3.19"
}

variable "ami_name_prefix" {
  type        = string
  description = "Prefix for the AMI name"
  default     = "alpine-nix-eks-worker"
}

# Source configuration for AWS EBS
source "amazon-ebs" "alpine_nix" {
  region        = var.aws_region
  source_ami    = "ami-0a5c5f1b567a5a355" # Alpine Linux AMI - update with latest
  instance_type = "t3.micro"
  ssh_username  = "alpine"
  
  # AMI naming with timestamp for uniqueness
  ami_name = "${var.ami_name_prefix}-${formatdate("YYYY-MM-DD-hhmm", timestamp())}"
  
  # Tags for organization and tracking
  ami_tags = {
    Name        = "${var.ami_name_prefix}-${formatdate("YYYY-MM-DD-hhmm", timestamp())}"
    Purpose     = "EKS Worker Node"
    OS          = "Alpine Linux"
    PackageManager = "Nix"
    Environment = "Production"
    ManagedBy   = "Packer"
    Project     = "Alpine-Nix-EKS"
  }
  
  # Instance tags
  tags = {
    Name = "Alpine-Nix-AMI-Builder"
    Purpose = "AMI Creation"
  }
}

# Build configuration
build {
  sources = ["source.amazon-ebs.alpine_nix"]

  # Provisioner 1: Update Alpine and install essential packages
  provisioner "shell" {
    inline = [
      "sudo apk update",
      "sudo apk upgrade",
      "sudo apk add --no-cache xz bash curl ca-certificates git openssh-client",
      "sudo apk add --no-cache aws-cli docker kubectl", # EKS essentials
      "echo 'Alpine base packages installed successfully'"
    ]
  }

  # Provisioner 2: Set up Nix user and group
  provisioner "shell" {
    inline = [
      "sudo addgroup -S nixbld",
      "sudo adduser -S nix -G nixbld -s /bin/bash",
      "sudo mkdir -m 0755 /nix",
      "sudo chown nix:nixbld /nix",
      "echo 'Nix user and group created successfully'"
    ]
  }

  # Provisioner 3: Install Nix package manager as the nix user
  provisioner "shell" {
    inline = [
      "sudo -u nix bash -c 'curl -L https://nixos.org/nix/install | sh'",
      "echo 'Nix package manager installed successfully'"
    ]
  }

  # Provisioner 4: Set up Nix environment and install EKS essentials
  provisioner "shell" {
    inline = [
      "sudo -u nix bash -c '. /home/nix/.nix-profile/etc/profile.d/nix.sh && nix-env -iA nixpkgs.awscli2'",
      "sudo -u nix bash -c '. /home/nix/.nix-profile/etc/profile.d/nix.sh && nix-env -iA nixpkgs.kubectl'",
      "sudo -u nix bash -c '. /home/nix/.nix-profile/etc/profile.d/nix.sh && nix-env -iA nixpkgs.docker'",
      "sudo -u nix bash -c '. /home/nix/.nix-profile/etc/profile.d/nix.sh && nix-env -iA nixpkgs.kubernetes'",
      "echo 'EKS essential packages installed via Nix successfully'"
    ]
  }

  # Provisioner 5: Configure system services for EKS
  provisioner "shell" {
    inline = [
      "sudo rc-update add docker default",
      "sudo rc-update add sshd default",
      "sudo setup-hostname alpine-nix-worker",
      "echo 'System services configured successfully'"
    ]
  }

  # Provisioner 6: Create Nix configuration for EKS worker
  provisioner "file" {
    source      = "nix/eks-worker.nix"
    destination = "/tmp/eks-worker.nix"
  }

  provisioner "shell" {
    inline = [
      "sudo -u nix cp /tmp/eks-worker.nix /home/nix/",
      "sudo -u nix bash -c '. /home/nix/.nix-profile/etc/profile.d/nix.sh && nix-env -if /home/nix/eks-worker.nix'",
      "echo 'EKS worker Nix configuration applied successfully'"
    ]
  }

  # Provisioner 7: Final cleanup and optimization
  provisioner "shell" {
    inline = [
      "sudo rm -rf /tmp/*",
      "sudo apk cache clean",
      "sudo -u nix nix-collect-garbage -d", # Clean up unused Nix store paths
      "echo 'Cleanup completed successfully'"
    ]
  }

  # Post-processor: Display AMI information
  post-processor "manifest" {
    output = "alpine-nix-ami-manifest.json"
    strip_path = true
  }
}
