# EKS Cluster Configuration

# KMS Key for EKS Encryption
resource "aws_kms_key" "eks" {
  count = var.enable_encryption ? 1 : 0
  
  description             = "EKS encryption key for ${local.cluster_name}"
  deletion_window_in_days = var.kms_key_deletion_window
  
  tags = merge(local.common_tags, {
    Name = "${local.cluster_name}-eks-encryption-key"
    Type = "KMSKey"
  })
}

resource "aws_kms_alias" "eks" {
  count = var.enable_encryption ? 1 : 0
  
  name          = "alias/${local.cluster_name}-eks-encryption"
  target_key_id = aws_kms_key.eks[0].key_id
}

# OIDC Identity Provider for IRSA
resource "aws_iam_openid_connect_provider" "eks" {
  count = var.enable_irsa ? 1 : 0
  
  client_id_list  = ["sts.amazonaws.com"]
thumbprint_list = [data.tls_certificate.eks[0].certificates[0].sha1_fingerprint]
  url             = aws_eks_cluster.main.identity[0].oidc[0].issuer
  
  tags = merge(local.common_tags, {
    Name = "${local.cluster_name}-oidc-provider"
    Type = "OIDCProvider"
  })
}

# Get EKS cluster certificate for OIDC
data "tls_certificate" "eks" {
  count = var.enable_irsa ? 1 : 0
  
  url = aws_eks_cluster.main.identity[0].oidc[0].issuer
}

# EKS Cluster
resource "aws_eks_cluster" "main" {
  name     = local.cluster_name
  role_arn = aws_iam_role.eks_cluster.arn
  version  = var.eks_cluster_version
  
  vpc_config {
subnet_ids              = concat(aws_subnet.private[*].id,
aws_subnet.public[*].id)
    endpoint_private_access = var.enable_private_access
    endpoint_public_access  = var.enable_public_access
    security_group_ids      = [aws_security_group.eks_cluster.id]
  }
  
  # Encryption configuration
  dynamic "encryption_config" {
    for_each = var.enable_encryption ? [1] : []
    content {
      provider {
        key_arn = aws_kms_key.eks[0].arn
      }
      resources = ["secrets"]
    }
  }
  
  # Enable logging
  dynamic "enabled_cluster_log_types" {
    for_each = var.enable_cluster_logging ? var.cluster_log_types : []
    content {
      log_types = enabled_cluster_log_types.value
    }
  }
  
  depends_on = [
    aws_iam_role_policy_attachment.eks_cluster_policy,
    aws_cloudwatch_log_group.eks_cluster
  ]
  
  tags = merge(local.common_tags, {
    Name = local.cluster_name
    Type = "EKSCluster"
  })
}

# CloudWatch Log Group for EKS Cluster
resource "aws_cloudwatch_log_group" "eks_cluster" {
  count = var.enable_cluster_logging ? 1 : 0
  
  name              = "/aws/eks/${local.cluster_name}/cluster"
  retention_in_days = 7
  
  tags = merge(local.common_tags, {
    Name = "${local.cluster_name}-eks-logs"
    Type = "CloudWatchLogGroup"
  })
}

# EKS Node Group
resource "aws_eks_node_group" "main" {
  cluster_name    = aws_eks_cluster.main.name
  node_group_name = "${local.cluster_name}-nodes"
  node_role_arn   = aws_iam_role.eks_node_group.arn
  subnet_ids      = aws_subnet.private[*].id
  
  # Use custom NixOS AMI if provided, otherwise use EKS optimized AMI
  ami_type = var.nixos_ami_id != "" ? "CUSTOM" : "AL2_x86_64"
  
  # Use custom AMI if provided
  dynamic "launch_template" {
    for_each = var.nixos_ami_id != "" ? [1] : []
    content {
      id      = aws_launch_template.nixos[0].id
      version = aws_launch_template.nixos[0].latest_version
    }
  }
  
  capacity_type  = "ON_DEMAND"
  instance_types = var.node_group_instance_types
  
  scaling_config {
    desired_size = var.node_group_desired_size
    max_size     = var.node_group_max_size
    min_size     = var.node_group_min_size
  }
  
  update_config {
    max_unavailable_percentage = 25
  }
  
# Ensure that IAM Role permissions are created before and deleted after EKS Node
Group handling.
# Otherwise, EKS will not be able to properly delete EC2 Instances and Elastic
Network Interfaces.
  depends_on = [
    aws_iam_role_policy_attachment.eks_worker_node_policy,
    aws_iam_role_policy_attachment.eks_cni_policy,
    aws_iam_role_policy_attachment.eks_container_registry_policy,
    aws_iam_role_policy_attachment.eks_ssm_policy,
  ]
  
  tags = merge(local.common_tags, {
    Name = "${local.cluster_name}-nodes"
    Type = "EKSNodeGroup"
  })
}

# Launch Template for NixOS nodes (if custom AMI is provided)
resource "aws_launch_template" "nixos" {
  count = var.nixos_ami_id != "" ? 1 : 0
  
  name_prefix   = "${local.cluster_name}-nixos-"
  image_id      = var.nixos_ami_id
  instance_type = var.node_group_instance_types[0]
  
  vpc_security_group_ids = [aws_security_group.eks_nodes.id]
  
  key_name = var.key_pair_name
  
  user_data = base64encode(templatefile("${path.module}/user_data.sh", {
    cluster_name = aws_eks_cluster.main.name
    cluster_endpoint = aws_eks_cluster.main.endpoint
    cluster_ca = aws_eks_cluster.main.certificate_authority[0].data
  }))
  
  tag_specifications {
    resource_type = "instance"
    tags = merge(local.common_tags, {
      Name = "${local.cluster_name}-nixos-node"
      Type = "EC2Instance"
    })
  }
  
  tags = merge(local.common_tags, {
    Name = "${local.cluster_name}-nixos-launch-template"
    Type = "LaunchTemplate"
  })
}

# EKS Add-ons
resource "aws_eks_addon" "vpc_cni" {
  cluster_name = aws_eks_cluster.main.name
  addon_name   = "vpc-cni"
  
  tags = merge(local.common_tags, {
    Name = "${local.cluster_name}-vpc-cni"
    Type = "EKSAddon"
  })
}

resource "aws_eks_addon" "coredns" {
  cluster_name = aws_eks_cluster.main.name
  addon_name   = "coredns"
  
  depends_on = [aws_eks_node_group.main]
  
  tags = merge(local.common_tags, {
    Name = "${local.cluster_name}-coredns"
    Type = "EKSAddon"
  })
}

resource "aws_eks_addon" "kube_proxy" {
  cluster_name = aws_eks_cluster.main.name
  addon_name   = "kube-proxy"
  
  tags = merge(local.common_tags, {
    Name = "${local.cluster_name}-kube-proxy"
    Type = "EKSAddon"
  })
}

resource "aws_eks_addon" "ebs_csi_driver" {
  cluster_name = aws_eks_cluster.main.name
  addon_name   = "aws-ebs-csi-driver"
  
  tags = merge(local.common_tags, {
    Name = "${local.cluster_name}-ebs-csi-driver"
    Type = "EKSAddon"
  })
}

# CloudWatch Container Insights (if enabled)
resource "aws_eks_addon" "cloudwatch_observability" {
  count = var.enable_cloudwatch_container_insights ? 1 : 0
  
  cluster_name = aws_eks_cluster.main.name
  addon_name   = "amazon-cloudwatch-observability"
  
  tags = merge(local.common_tags, {
    Name = "${local.cluster_name}-cloudwatch-observability"
    Type = "EKSAddon"
  })
}
