#!/usr/bin/env bash

# AWS Prerequisites Setup Script
# This script helps set up the required AWS resources for the NixOS EKS cluster

set -euo pipefail

# Configuration
REGION="${AWS_REGION:-us-west-2}"
CLUSTER_NAME="nixos-eks-cluster"
KEY_PAIR_NAME="eks-nixos-key"
KMS_KEY_ALIAS="eks-nixos-encryption"

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Logging functions
log_info() {
    echo -e "${BLUE}[INFO]${NC} $1"
}

log_success() {
    echo -e "${GREEN}[SUCCESS]${NC} $1"
}

log_warning() {
    echo -e "${YELLOW}[WARNING]${NC} $1"
}

log_error() {
    echo -e "${RED}[ERROR]${NC} $1"
}

# Check if AWS CLI is configured
check_aws_cli() {
    log_info "Checking AWS CLI configuration..."
    
    if ! command -v aws &> /dev/null; then
        log_error "AWS CLI is not installed. Please install it first."
        exit 1
    fi
    
    if ! aws sts get-caller-identity &> /dev/null; then
        log_error "AWS credentials are not configured. Please run 'aws configure' first."
        exit 1
    fi
    
    log_success "AWS CLI is properly configured!"
}

# Create EC2 Key Pair
create_key_pair() {
    log_info "Creating EC2 Key Pair: $KEY_PAIR_NAME"
    
    # Check if key pair already exists
    if aws ec2 describe-key-pairs --key-names "$KEY_PAIR_NAME" --region "$REGION" &> /dev/null; then
        log_warning "Key pair $KEY_PAIR_NAME already exists. Skipping creation."
        return
    fi
    
    # Create key pair
    aws ec2 create-key-pair \
        --key-name "$KEY_PAIR_NAME" \
        --region "$REGION" \
        --query 'KeyMaterial' \
        --output text > "${KEY_PAIR_NAME}.pem"
    
    # Set proper permissions
    chmod 400 "${KEY_PAIR_NAME}.pem"
    
    log_success "Key pair created: ${KEY_PAIR_NAME}.pem"
    log_warning "IMPORTANT: Keep this key file secure! You'll need it to SSH into your nodes."
}

# Create KMS Key for EKS encryption
create_kms_key() {
    log_info "Creating KMS key for EKS encryption..."
    
    # Check if key already exists
    KEY_ID=$(aws kms describe-key --key-id "alias/$KMS_KEY_ALIAS" --region "$REGION" --query 'KeyMetadata.KeyId' --output text 2>/dev/null || echo "")
    
    if [ -n "$KEY_ID" ] && [ "$KEY_ID" != "None" ]; then
        log_warning "KMS key with alias $KMS_KEY_ALIAS already exists. Skipping creation."
        echo "$KEY_ID"
        return
    fi
    
    # Create KMS key
    KEY_ID=$(aws kms create-key \
        --description "EKS encryption key for $CLUSTER_NAME" \
        --region "$REGION" \
        --query 'KeyMetadata.KeyId' \
        --output text)
    
    # Create alias
    aws kms create-alias \
        --alias-name "alias/$KMS_KEY_ALIAS" \
        --target-key-id "$KEY_ID" \
        --region "$REGION"
    
    log_success "KMS key created: $KEY_ID"
    echo "$KEY_ID"
}

# Create IAM roles and policies
create_iam_resources() {
    log_info "Creating IAM roles and policies..."
    
    # Create EKS cluster service role
    CLUSTER_ROLE_NAME="EKS-Cluster-Service-Role"
    
    if ! aws iam get-role --role-name "$CLUSTER_ROLE_NAME" &> /dev/null; then
        log_info "Creating EKS cluster service role..."
        
        # Create trust policy
        cat > cluster-trust-policy.json << EOF
{
  "Version": "2012-10-17",
  "Statement": [
    {
      "Effect": "Allow",
      "Principal": {
        "Service": "eks.amazonaws.com"
      },
      "Action": "sts:AssumeRole"
    }
  ]
}
EOF
        
        # Create role
        aws iam create-role \
            --role-name "$CLUSTER_ROLE_NAME" \
            --assume-role-policy-document file://cluster-trust-policy.json
        
        # Attach policies
        aws iam attach-role-policy \
            --role-name "$CLUSTER_ROLE_NAME" \
            --policy-arn arn:aws:iam::aws:policy/AmazonEKSClusterPolicy
        
        # Clean up
        rm cluster-trust-policy.json
        
        log_success "EKS cluster service role created!"
    else
        log_warning "EKS cluster service role already exists. Skipping creation."
    fi
}

# Install eksctl
install_eksctl() {
    log_info "Checking if eksctl is installed..."
    
    if command -v eksctl &> /dev/null; then
        log_success "eksctl is already installed!"
        return
    fi
    
    log_info "Installing eksctl..."
    
    # Download and install eksctl
    curl --silent --location "https://github.com/weaveworks/eksctl/releases/latest/download/eksctl_$(uname -s)_amd64.tar.gz" | tar xz -C /tmp
    sudo mv /tmp/eksctl /usr/local/bin
    
    log_success "eksctl installed successfully!"
}

# Generate updated cluster configuration
generate_cluster_config() {
    log_info "Generating updated cluster configuration..."
    
    KEY_ID="$1"
    ACCOUNT_ID=$(aws sts get-caller-identity --query Account --output text)
    
    # Update the cluster configuration with actual values
    sed -i.bak \
        -e "s/ACCOUNT_ID/$ACCOUNT_ID/g" \
        -e "s/KEY_ID/$KEY_ID/g" \
        -e "s/ami-xxxxxxxxx/ami-$(date +%Y%m%d)/g" \
        ../kubernetes/eks-cluster.yaml
    
    log_success "Cluster configuration updated with actual values!"
}

# Main function
main() {
    log_info "Setting up AWS prerequisites for NixOS EKS cluster..."
    log_info "Region: $REGION"
    log_info "Cluster Name: $CLUSTER_NAME"
    
    check_aws_cli
    install_eksctl
    create_key_pair
    KEY_ID=$(create_kms_key)
    create_iam_resources
    generate_cluster_config "$KEY_ID"
    
    log_success "AWS prerequisites setup completed!"
    log_info "Next steps:"
    log_info "1. Build your NixOS AMI using: ./scripts/build-ami.sh"
    log_info "2. Update the AMI ID in kubernetes/eks-cluster.yaml"
    log_info "3. Create the EKS cluster using: eksctl create cluster -f kubernetes/eks-cluster.yaml"
    log_info "4. Don't forget to keep your key file secure: ${KEY_PAIR_NAME}.pem"
}

# Run main function
main "$@"
