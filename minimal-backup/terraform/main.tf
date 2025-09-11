# NixOS EKS Infrastructure Configuration
# This Terraform configuration creates the complete AWS infrastructure
# for our NixOS EKS cluster with security hardening

terraform {
  required_version = ">= 1.0"
  
  required_providers {
    aws = {
      source  = "hashicorp/aws"
      version = "~> 5.0"
    }
    kubernetes = {
      source  = "hashicorp/kubernetes"
      version = "~> 2.20"
    }
    helm = {
      source  = "hashicorp/helm"
      version = "~> 2.10"
    }
  }
  
  # Configure remote state storage (uncomment when ready)
  # backend "s3" {
  #   bucket = "nixos-eks-terraform-state"
  #   key    = "infrastructure/terraform.tfstate"
  #   region = "us-west-2"
  # }
}

# Configure the AWS Provider
provider "aws" {
  region = var.aws_region
  
  default_tags {
    tags = {
      Project     = "nixos-eks"
      Environment = var.environment
      ManagedBy   = "terraform"
      Owner       = var.owner
    }
  }
}

# Data sources
data "aws_availability_zones" "available" {
  state = "available"
}

data "aws_caller_identity" "current" {}

# Local values
locals {
  cluster_name = "${var.project_name}-${var.environment}"
  
  common_tags = {
    Project     = var.project_name
    Environment = var.environment
    ManagedBy   = "terraform"
    Owner       = var.owner
  }
  
  # Availability zones (use 3 for high availability)
  azs = slice(data.aws_availability_zones.available.names, 0, 3)
}
