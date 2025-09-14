# AWS Setup Guide for NixOS EKS Cluster
![Parametric Flower](parametric-flower-compressed.png)

This guide walks you through setting up the required AWS resources for your
NixOS EKS cluster.

## Prerequisites

Before you begin, ensure you have:

1. **AWS Account** with appropriate permissions
2. **AWS CLI** installed and configured
3. **eksctl** installed (or use our setup script)
4. **kubectl** installed
5. **Nix** installed on your local machine

## Required AWS Permissions

Your AWS user/role needs the following permissions:

### EKS Permissions
- `eks:*` (EKS cluster management)
- `iam:*` (IAM role and policy management)
- `ec2:*` (EC2 instances, security groups, VPC)
- `kms:*` (KMS key management)
- `cloudformation:*` (CloudFormation stack management)

### Additional Permissions
- `ssm:*` (Systems Manager for node management)
- `logs:*` (CloudWatch logging)
- `autoscaling:*` (Auto Scaling groups)

## Step-by-Step Setup

### 1. Configure AWS CLI

First, configure your AWS CLI with appropriate credentials:

```bash
aws configure
```

Enter your:
- AWS Access Key ID
- AWS Secret Access Key
- Default region (e.g., `us-west-2`)
- Default output format (e.g., `json`)

### 2. Run the Prerequisites Setup Script

We've created an automated script to set up most AWS resources:

```bash
cd scripts/
./setup-aws-prerequisites.sh
```

This script will:
- ✅ Check AWS CLI configuration
- ✅ Install eksctl if not present
- ✅ Create EC2 key pair for SSH access
- ✅ Create KMS key for EKS encryption
- ✅ Create IAM roles and policies
- ✅ Generate updated cluster configuration

### 3. Build Your NixOS AMI

Before creating the EKS cluster, you need to build your custom NixOS AMI:

```bash
cd nixos/
./scripts/build-ami.sh
```

This will:
- Build the NixOS configuration
- Create a custom AMI using nixos-eks-ami
- Output the AMI ID for use in the cluster configuration

### 4. Update Cluster Configuration

After building the AMI, update the cluster configuration:

```bash
# Get the AMI ID from the build output
AMI_ID=$(cat /tmp/ami-id.txt)

# Update the cluster configuration
sed -i "s/ami-xxxxxxxxx/$AMI_ID/g" kubernetes/eks-cluster.yaml
```

### 5. Create the EKS Cluster

Now create your EKS cluster with NixOS worker nodes:

```bash
eksctl create cluster -f kubernetes/eks-cluster.yaml
```

This will:
- Create the EKS control plane
- Launch NixOS worker nodes using your custom AMI
- Configure networking and security groups
- Set up IAM roles and policies
- Install required add-ons

### 6. Verify the Cluster

Verify your cluster is running correctly:

```bash
# Get cluster info
eksctl get cluster

# Get node info
kubectl get nodes

# Check node details
kubectl describe nodes
```

## Manual AWS Resource Creation

If you prefer to create resources manually or need to customize them:

### 1. Create EC2 Key Pair

```bash
aws ec2 create-key-pair \
    --key-name eks-nixos-key \
    --region us-west-2 \
    --query 'KeyMaterial' \
    --output text > eks-nixos-key.pem

chmod 400 eks-nixos-key.pem
```

### 2. Create KMS Key

```bash
# Create KMS key
KEY_ID=$(aws kms create-key \
    --description "EKS encryption key" \
    --region us-west-2 \
    --query 'KeyMetadata.KeyId' \
    --output text)

# Create alias
aws kms create-alias \
    --alias-name alias/eks-nixos-encryption \
    --target-key-id $KEY_ID \
    --region us-west-2
```

### 3. Create IAM Roles

#### EKS Cluster Service Role

```bash
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
    --role-name EKS-Cluster-Service-Role \
    --assume-role-policy-document file://cluster-trust-policy.json

# Attach policy
aws iam attach-role-policy \
    --role-name EKS-Cluster-Service-Role \
    --policy-arn arn:aws:iam::aws:policy/AmazonEKSClusterPolicy
```

#### EKS Node Group Role

```bash
# Create trust policy
cat > node-trust-policy.json << EOF
{
  "Version": "2012-10-17",
  "Statement": [
    {
      "Effect": "Allow",
      "Principal": {
        "Service": "ec2.amazonaws.com"
      },
      "Action": "sts:AssumeRole"
    }
  ]
}
EOF

# Create role
aws iam create-role \
    --role-name EKS-NodeGroup-Role \
    --assume-role-policy-document file://node-trust-policy.json

# Attach policies
aws iam attach-role-policy \
    --role-name EKS-NodeGroup-Role \
    --policy-arn arn:aws:iam::aws:policy/AmazonEKSWorkerNodePolicy

aws iam attach-role-policy \
    --role-name EKS-NodeGroup-Role \
    --policy-arn arn:aws:iam::aws:policy/AmazonEKS_CNI_Policy

aws iam attach-role-policy \
    --role-name EKS-NodeGroup-Role \
    --policy-arn arn:aws:iam::aws:policy/AmazonEC2ContainerRegistryReadOnly
```

## Cost Considerations

### EKS Control Plane
- **Cost**: $0.10 per hour per cluster
- **Minimum**: 1 cluster = ~$72/month

### EC2 Instances (Worker Nodes)
- **t3.medium**: ~$30/month per instance
- **t3.large**: ~$60/month per instance
- **t3.xlarge**: ~$120/month per instance

### Additional Costs
- **EBS Volumes**: ~$0.10/GB/month
- **Data Transfer**: Varies by usage
- **CloudWatch Logs**: ~$0.50/GB ingested

### Cost Optimization Tips
1. Use Spot Instances for non-critical workloads
2. Implement cluster autoscaling
3. Use smaller instance types for development
4. Monitor and optimize resource usage

## Security Best Practices

### Network Security
- Use private subnets for worker nodes
- Implement network policies
- Use security groups effectively
- Enable VPC Flow Logs

### Access Control
- Use IAM roles instead of access keys
- Implement least privilege access
- Enable MFA for AWS console access
- Use AWS Organizations for multi-account setup

### Monitoring and Logging
- Enable CloudTrail for API logging
- Use CloudWatch for monitoring
- Implement centralized logging
- Set up alerting for security events

## Troubleshooting

### Common Issues

#### 1. AMI Not Found
```
Error: The specified AMI does not exist
```
**Solution**: Ensure the AMI ID is correct and exists in your region.

#### 2. Insufficient Permissions
```
Error: User is not authorized to perform eks:CreateCluster
```
**Solution**: Check your IAM permissions and ensure you have the required
policies.

#### 3. Key Pair Not Found
```
Error: The specified key pair does not exist
```
**Solution**: Ensure the key pair exists in the correct region.

#### 4. VPC CIDR Conflicts
```
Error: The specified CIDR block conflicts with an existing CIDR block
```
**Solution**: Choose a different CIDR block or use an existing VPC.

### Debugging Commands

```bash
# Check AWS credentials
aws sts get-caller-identity

# List available AMIs
aws ec2 describe-images --owners self --region us-west-2

# Check EKS clusters
aws eks list-clusters --region us-west-2

# Get cluster details
aws eks describe-cluster --name nixos-eks-cluster --region us-west-2

# Check node groups
aws eks list-nodegroups --cluster-name nixos-eks-cluster --region us-west-2
```

## Next Steps

After setting up your AWS resources and creating the EKS cluster:

1. **Deploy Core Services**: Set up NGINX Ingress and cert-manager
2. **Configure Monitoring**: Deploy Prometheus and Grafana
3. **Deploy Your Application**: Create and deploy your Haskell application
4. **Set up CI/CD**: Configure GitHub Actions for automated deployments

## Support

If you encounter issues:

1. Check the troubleshooting section above
2. Review AWS CloudFormation events in the console
3. Check EKS cluster logs in CloudWatch
4. Verify your NixOS AMI is working correctly

For additional help, refer to:
- [AWS EKS Documentation](https://docs.aws.amazon.com/eks/)
- [eksctl Documentation](https://eksctl.io/)
- [NixOS Documentation](https://nixos.org/manual/nixos/stable/)
