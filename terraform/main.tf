# Sovereign EKS Infrastructure
terraform {
  required_version = ">= 1.0.0"
  required_providers {
    aws = {
      source  = "hashicorp/aws"
      version = "~> 4.0"
    }
  }

  backend "s3" {
    bucket = "sovereign-infrastructure-state"
    key    = "eks-alpine-nix/terraform.tfstate"
    region = "us-west-2"
    encrypt = true
  }
}

# Sovereign identity configuration
locals {
  sovereign_identity = {
    gpg_fingerprint = "YOUR_GPG_FINGERPRINT_HERE"
    ssh_public_key  = file("${path.module}/../identity/ssh-public-key.pub")
    build_timestamp = timestamp()
  }
}

# AWS provider with assumed role for sovereignty
provider "aws" {
  region = "us-west-2"
  assume_role {
role_arn = "arn:aws:iam::${local.account_id}:role/SovereignInfrastructureRole"
  }

  default_tags {
    tags = {
      Sovereignty    = "b122m-faeb"
      GPGIdentity    = local.sovereign_identity.gpg_fingerprint
      Repository     = "https://github.com/kae3g/aws-eks-alpine-nix"
      Declarative    = "nix"
      BaseSystem     = "alpine"
    }
  }
}

# Sovereign VPC with minimal attack surface
module "sovereign_vpc" {
  source = "./modules/vpc"

  name                 = "sovereign-vpc"
  cidr                 = "10.42.0.0/16"
  azs                  = ["us-west-2a", "us-west-2b"]
  private_subnets      = ["10.42.1.0/24", "10.42.2.0/24"]
  public_subnets       = ["10.42.101.0/24", "10.42.102.0/24"]
  enable_nat_gateway   = true
  single_nat_gateway   = true
  enable_vpn_gateway   = false

  tags = {
    Sovereignty = "true"
    Purpose     = "eks-workers"
  }
}

# Sovereign EKS cluster
module "sovereign_eks" {
  source = "./modules/eks"

  cluster_name    = "sovereign-cluster"
  cluster_version = "1.27"

  vpc_id          = module.sovereign_vpc.vpc_id
  subnet_ids      = module.sovereign_vpc.private_subnets

  # Alpine+Nix worker group
  worker_groups = [
    {
      name                 = "alpine-nix-workers"
      ami_id               = data.aws_ami.alpine_nix_eks.id
      instance_type        = "t3.medium"
      asg_desired_capacity = 2
      asg_min_size         = 2
      asg_max_size         = 6
      ssh_key_name         = aws_key_pair.sovereign.key_name
      ssh_port             = 4922
    }
  ]

  tags = {
    Sovereignty = "true"
    ManagedBy   = "terraform"
  }
}

# Sovereign SSH key pair
resource "aws_key_pair" "sovereign" {
  key_name   = "sovereign-key-${local.sovereign_identity.gpg_fingerprint}"
  public_key = local.sovereign_identity.ssh_public_key

  tags = {
    GPGIdentity = local.sovereign_identity.gpg_fingerprint
  }
}

# Data source for our custom Alpine+Nix AMI
data "aws_ami" "alpine_nix_eks" {
  most_recent = true
  owners      = ["self"]

  filter {
    name   = "name"
    values = ["alpine-nix-eks-worker-*"]
  }

  filter {
    name   = "tag:Sovereignty"
    values = ["true"]
  }
}
