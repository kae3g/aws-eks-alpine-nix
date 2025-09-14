# Minimal NixOS AWS Infrastructure
# Updated for 2025-09-11 with current versions and best practices
# Note: Some versions may need verification as of 2025-09-11

terraform {
  required_version = ">= 1.6"
  
  required_providers {
    aws = {
      source  = "hashicorp/aws"
      version = "~> 5.0"
    }
  }
}

# Configure the AWS Provider
provider "aws" {
  region = var.aws_region
  
  default_tags {
    tags = {
      Project     = "nixos-minimal"
      Environment = "dev"
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
  cluster_name = "nixos-minimal-${var.environment}"
  common_tags = {
    Project     = "nixos-minimal"
    Environment = var.environment
    ManagedBy   = "terraform"
    Owner       = var.owner
  }
}

# VPC
resource "aws_vpc" "main" {
  cidr_block           = var.vpc_cidr
  enable_dns_hostnames = true
  enable_dns_support   = true
  
  tags = merge(local.common_tags, {
    Name = "${local.cluster_name}-vpc"
  })
}

# Internet Gateway
resource "aws_internet_gateway" "main" {
  vpc_id = aws_vpc.main.id
  
  tags = merge(local.common_tags, {
    Name = "${local.cluster_name}-igw"
  })
}

# Public Subnet
resource "aws_subnet" "public" {
  vpc_id                  = aws_vpc.main.id
  cidr_block              = var.public_subnet_cidr
  availability_zone       = data.aws_availability_zones.available.names[0]
  map_public_ip_on_launch = true
  
  tags = merge(local.common_tags, {
    Name = "${local.cluster_name}-public"
  })
}

# Route Table for Public Subnet
resource "aws_route_table" "public" {
  vpc_id = aws_vpc.main.id
  
  route {
    cidr_block = "0.0.0.0/0"
    gateway_id = aws_internet_gateway.main.id
  }
  
  tags = merge(local.common_tags, {
    Name = "${local.cluster_name}-public-rt"
  })
}

# Route Table Association
resource "aws_route_table_association" "public" {
  subnet_id      = aws_subnet.public.id
  route_table_id = aws_route_table.public.id
}

# Security Group for NixOS instances
resource "aws_security_group" "nixos_sg" {
  name_prefix = "${local.cluster_name}-nixos-"
  vpc_id      = aws_vpc.main.id
  
  # SSH access
  ingress {
    from_port   = 22
    to_port     = 22
    protocol    = "tcp"
    cidr_blocks = ["0.0.0.0/0"]
    description = "SSH"
  }
  
  # Mosh access (UDP 60000-61000)
  ingress {
    from_port   = 60000
    to_port     = 61000
    protocol    = "udp"
    cidr_blocks = ["0.0.0.0/0"]
    description = "Mosh"
  }
  
  # HTTP/HTTPS for development
  ingress {
    from_port   = 80
    to_port     = 80
    protocol    = "tcp"
    cidr_blocks = ["0.0.0.0/0"]
    description = "HTTP"
  }
  
  ingress {
    from_port   = 443
    to_port     = 443
    protocol    = "tcp"
    cidr_blocks = ["0.0.0.0/0"]
    description = "HTTPS"
  }
  
  # All outbound traffic
  egress {
    from_port   = 0
    to_port     = 0
    protocol    = "-1"
    cidr_blocks = ["0.0.0.0/0"]
  }
  
  tags = merge(local.common_tags, {
    Name = "${local.cluster_name}-nixos-sg"
  })
}

# Get latest NixOS AMI
data "aws_ami" "nixos" {
  most_recent = true
  owners      = ["080433136561"] # Official NixOS AWS account
  
  filter {
    name   = "name"
    values = ["nixos-*-24.11.*-x86_64-linux"]
  }
  
  filter {
    name   = "virtualization-type"
    values = ["hvm"]
  }
}

# NixOS EC2 Instance
resource "aws_instance" "nixos_host" {
  ami                    = data.aws_ami.nixos.id
  instance_type          = var.instance_type
  key_name               = var.key_pair_name
  vpc_security_group_ids = [aws_security_group.nixos_sg.id]
  subnet_id              = aws_subnet.public.id
  
  # Ephemeral hostname
  tags = merge(local.common_tags, {
    Name = "${local.cluster_name}-host-${random_id.instance_suffix.hex}"
  })
  
  # Connection for provisioning
  connection {
    type        = "ssh"
    user        = "nixos"
    private_key = file(var.private_key_path)
    host        = self.public_ip
  }
  
  # Copy NixOS configuration
  provisioner "file" {
    source      = "../nixos/"
    destination = "/tmp/nixos-config"
  }
  
  # Apply NixOS configuration
  provisioner "remote-exec" {
    inline = [
      "sudo cp -r /tmp/nixos-config/* /etc/nixos/",
      "sudo nixos-rebuild switch"
    ]
  }
  
  # Ensure instance is ready
  provisioner "remote-exec" {
    inline = [
      "echo 'NixOS instance is ready!'"
    ]
  }
}

# Random suffix for ephemeral naming
resource "random_id" "instance_suffix" {
  byte_length = 4
}

# Outputs
output "instance_public_ip" {
  description = "Public IP address of the NixOS instance"
  value       = aws_instance.nixos_host.public_ip
}

output "instance_public_dns" {
  description = "Public DNS name of the NixOS instance"
  value       = aws_instance.nixos_host.public_dns
}

output "ssh_command" {
  description = "SSH command to connect to the instance"
value       = "ssh -i ${var.private_key_path}
nixos@${aws_instance.nixos_host.public_ip}"
}

output "mosh_command" {
  description = "Mosh command to connect to the instance"
value       = "mosh -ssh=\"ssh -i ${var.private_key_path}\"
nixos@${aws_instance.nixos_host.public_ip}"
}
