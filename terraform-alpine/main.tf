# Alpine Linux + Nix EKS Infrastructure 💙
# This Terraform configuration deploys an EKS cluster using our custom
# Alpine+Nix AMI for worker nodes, combining maximum security with
# declarative package management

terraform {
  required_version = ">= 1.5"
  
  required_providers {
    aws = {
      source  = "hashicorp/aws"
      version = "~> 5.0"
    }
  }
}

# AWS Provider configuration with SSO
provider "aws" {
  region = var.aws_region
  
  # Use AWS CLI SSO configuration
  shared_config_files      = ["~/.aws/config"]
  shared_credentials_files = ["~/.aws/credentials"]
  profile                  = "AdministratorAccess-059549154267" # Update with your SSO profile name
  
  default_tags {
    tags = {
      Project     = "alpine-nix-eks"
      Environment = "dev"
      ManagedBy   = "terraform"
      Owner       = var.owner
      Philosophy  = "Declarative Infrastructure"
    }
  }
}

# Data source to find our custom Alpine+Nix AMI
data "aws_ami" "alpine_nix" {
  most_recent = true
  owners      = ["self"] # Our custom AMI is owned by our account

  filter {
    name   = "name"
    values = ["alpine-nix-eks-worker-*"]
  }

  filter {
    name   = "state"
    values = ["available"]
  }
}

# VPC for our EKS cluster
resource "aws_vpc" "main" {
  cidr_block           = "10.0.0.0/16"
  enable_dns_hostnames = true
  enable_dns_support   = true

  tags = {
    Name = "alpine-nix-eks-vpc"
  }
}

# Internet Gateway
resource "aws_internet_gateway" "main" {
  vpc_id = aws_vpc.main.id

  tags = {
    Name = "alpine-nix-eks-igw"
  }
}

# Public Subnets
resource "aws_subnet" "public" {
  count = 2

  vpc_id                  = aws_vpc.main.id
  cidr_block              = "10.0.${count.index + 1}.0/24"
  availability_zone       = data.aws_availability_zones.available.names[count.index]
  map_public_ip_on_launch = true

  tags = {
    Name = "alpine-nix-eks-public-${count.index + 1}"
    "kubernetes.io/role/elb" = "1"
  }
}

# Private Subnets
resource "aws_subnet" "private" {
  count = 2

  vpc_id            = aws_vpc.main.id
  cidr_block        = "10.0.${count.index + 10}.0/24"
  availability_zone = data.aws_availability_zones.available.names[count.index]

  tags = {
    Name = "alpine-nix-eks-private-${count.index + 1}"
    "kubernetes.io/role/internal-elb" = "1"
  }
}

# Route Table for Public Subnets
resource "aws_route_table" "public" {
  vpc_id = aws_vpc.main.id

  route {
    cidr_block = "0.0.0.0/0"
    gateway_id = aws_internet_gateway.main.id
  }

  tags = {
    Name = "alpine-nix-eks-public-rt"
  }
}

# Route Table Associations for Public Subnets
resource "aws_route_table_association" "public" {
  count = 2

  subnet_id      = aws_subnet.public[count.index].id
  route_table_id = aws_route_table.public.id
}

# EKS Cluster IAM Role
resource "aws_iam_role" "eks_cluster" {
  name = "alpine-nix-eks-cluster-role"

  assume_role_policy = jsonencode({
    Statement = [{
      Action = "sts:AssumeRole"
      Effect = "Allow"
      Principal = {
        Service = "eks.amazonaws.com"
      }
    }]
    Version = "2012-10-17"
  })
}

resource "aws_iam_role_policy_attachment" "eks_cluster_policy" {
  policy_arn = "arn:aws:iam::aws:policy/AmazonEKSClusterPolicy"
  role       = aws_iam_role.eks_cluster.name
}

# EKS Node Group IAM Role
resource "aws_iam_role" "eks_node" {
  name = "alpine-nix-eks-node-role"

  assume_role_policy = jsonencode({
    Statement = [{
      Action = "sts:AssumeRole"
      Effect = "Allow"
      Principal = {
        Service = "ec2.amazonaws.com"
      }
    }]
    Version = "2012-10-17"
  })
}

resource "aws_iam_role_policy_attachment" "eks_worker_node_policy" {
  policy_arn = "arn:aws:iam::aws:policy/AmazonEKSWorkerNodePolicy"
  role       = aws_iam_role.eks_node.name
}

resource "aws_iam_role_policy_attachment" "eks_cni_policy" {
  policy_arn = "arn:aws:iam::aws:policy/AmazonEKS_CNI_Policy"
  role       = aws_iam_role.eks_node.name
}

resource "aws_iam_role_policy_attachment" "eks_container_registry_policy" {
  policy_arn = "arn:aws:iam::aws:policy/AmazonEC2ContainerRegistryReadOnly"
  role       = aws_iam_role.eks_node.name
}

# EKS Cluster
resource "aws_eks_cluster" "main" {
  name     = "alpine-nix-eks"
  role_arn = aws_iam_role.eks_cluster.arn
  version  = "1.28"

  vpc_config {
    subnet_ids = concat(aws_subnet.public[*].id, aws_subnet.private[*].id)
  }

  depends_on = [
    aws_iam_role_policy_attachment.eks_cluster_policy,
  ]

  tags = {
    Name = "alpine-nix-eks-cluster"
  }
}

# EKS Node Group using our custom Alpine+Nix AMI
resource "aws_eks_node_group" "alpine_nix" {
  cluster_name    = aws_eks_cluster.main.name
  node_group_name = "alpine-nix-nodes"
  node_role_arn   = aws_iam_role.eks_node.arn
  subnet_ids      = aws_subnet.private[*].id

  # Use our custom Alpine+Nix AMI
  ami_type = "CUSTOM"
  image_id = data.aws_ami.alpine_nix.id

  capacity_type  = "ON_DEMAND"
  instance_types = ["t3.medium"]

  scaling_config {
    desired_size = 2
    max_size     = 4
    min_size     = 1
  }

  # User data script for final node configuration
  user_data = base64encode(<<-EOF
    #!/bin/bash
    set -ex
    
    # Ensure Nix environment is loaded
    if [ -f /home/nix/.nix-profile/etc/profile.d/nix.sh ]; then
        source /home/nix/.nix-profile/etc/profile.d/nix.sh
    fi
    
    # Run the EKS worker setup script
    if [ -f /home/nix/.nix-profile/bin/eks-worker-setup ]; then
        /home/nix/.nix-profile/bin/eks-worker-setup
    fi
    
    # Configure kubelet with our custom settings
    echo "KUBELET_EXTRA_ARGS=--node-labels=alpine-nix=true" >> /etc/kubernetes/kubelet/kubelet-config.json
    
    # Start kubelet
    systemctl enable kubelet
    systemctl start kubelet
  EOF
  )

  depends_on = [
    aws_iam_role_policy_attachment.eks_worker_node_policy,
    aws_iam_role_policy_attachment.eks_cni_policy,
    aws_iam_role_policy_attachment.eks_container_registry_policy,
  ]

  tags = {
    Name = "alpine-nix-eks-nodes"
  }
}

# Data sources
data "aws_availability_zones" "available" {
  state = "available"
}

# Outputs
output "cluster_endpoint" {
  description = "Endpoint for EKS control plane"
  value       = aws_eks_cluster.main.endpoint
}

output "cluster_security_group_id" {
  description = "Security group ids attached to the cluster control plane"
  value       = aws_eks_cluster.main.vpc_config[0].cluster_security_group_id
}

output "cluster_iam_role_name" {
  description = "IAM role name associated with EKS cluster"
  value       = aws_iam_role.eks_cluster.name
}

output "cluster_certificate_authority_data" {
  description = "Base64 encoded certificate data required to communicate with the cluster"
  value       = aws_eks_cluster.main.certificate_authority[0].data
}

output "cluster_name" {
  description = "The name/id of the EKS cluster"
  value       = aws_eks_cluster.main.name
}

output "cluster_arn" {
  description = "The Amazon Resource Name (ARN) of the cluster"
  value       = aws_eks_cluster.main.arn
}
