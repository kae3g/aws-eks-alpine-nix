# 🌙 A Digital Garden of Infinite Possibilities
![Parametric Flower](parametric-flower-compressed.png)

This directory contains the complete Terraform configuration for provisioning
AWS infrastructure to support a NixOS-based EKS cluster.

## Overview

This Terraform configuration creates:

- **VPC and Networking**: Multi-AZ VPC with public/private subnets, NAT
gateways, and security groups
- **EKS Cluster**: Managed Kubernetes control plane with custom NixOS worker
nodes
- **IAM Roles**: Proper IAM roles and policies for EKS and node groups
- **Security**: KMS encryption, security groups, and network policies
- **Add-ons**: VPC CNI, CoreDNS, kube-proxy, EBS CSI driver, and optional
add-ons
- **Monitoring**: CloudWatch logging and optional Container Insights

## Prerequisites

1. **Terraform**: Install Terraform >= 1.0
2. **AWS CLI**: Configured with appropriate permissions
3. **NixOS AMI**: Build your custom NixOS AMI first using the build script

## Quick Start

1. **Copy the example variables file:**
   ```bash
   cp terraform.tfvars.example terraform.tfvars
   ```

2. **Edit terraform.tfvars with your values:**
   ```bash
   # Set your AWS region, project name, etc.
   vim terraform.tfvars
   ```

3. **Build your NixOS AMI:**
   ```bash
   cd ../nixos/
   ./scripts/build-ami.sh
   ```

4. **Update terraform.tfvars with the AMI ID:**
   ```bash
   # Get the AMI ID
   AMI_ID=$(cat /tmp/ami-id.txt)
   
   # Update terraform.tfvars
   sed -i "s/ami-xxxxxxxxx/$AMI_ID/g" terraform.tfvars
   ```

5. **Initialize and apply Terraform:**
   ```bash
   terraform init
   terraform plan
   terraform apply
   ```

6. **Configure kubectl:**
   ```bash
   aws eks update-kubeconfig --region us-west-2 --name nixos-eks-dev
   ```

## Configuration Files

### Core Files
- `main.tf` - Main configuration and provider setup
- `variables.tf` - Input variables and their descriptions
- `outputs.tf` - Output values after deployment
- `vpc.tf` - VPC, subnets, and networking configuration
- `iam.tf` - IAM roles and policies
- `eks.tf` - EKS cluster and node group configuration

### Supporting Files
- `user_data.sh` - Bootstrap script for NixOS worker nodes
- `terraform.tfvars.example` - Example variables file
- `README.md` - This documentation

## Key Features

### Security
- **KMS Encryption**: EKS cluster secrets encrypted with customer-managed KMS
key
- **Network Security**: Private subnets for worker nodes, public subnets for
load balancers
- **IAM Security**: Least-privilege IAM roles and policies
- **Security Groups**: Restrictive security group rules

### High Availability
- **Multi-AZ Deployment**: Resources spread across 3 availability zones
- **Auto Scaling**: Node groups with configurable scaling policies
- **Load Balancing**: Support for Application Load Balancers

### Monitoring
- **CloudWatch Logs**: EKS cluster and application logging
- **Container Insights**: Optional detailed container monitoring
- **Cost Monitoring**: Built-in cost estimation and optimization

### Cost Optimization
- **Spot Instances**: Optional spot instance support for cost savings
- **Right-sizing**: Configurable instance types and scaling
- **Resource Tagging**: Consistent tagging for cost allocation

## Variables

### Required Variables
- `aws_region` - AWS region for deployment
- `project_name` - Name of the project
- `environment` - Environment (dev, staging, prod)

### Optional Variables
- `nixos_ami_id` - Custom NixOS AMI ID (required for NixOS nodes)
- `node_group_instance_types` - EC2 instance types for nodes
- `enable_*` - Various feature toggles
- `*_cidr` - Network CIDR blocks

See `variables.tf` for complete documentation.

## Outputs

After deployment, Terraform provides:

- **Cluster Information**: Name, endpoint, ARN, security group IDs
- **Network Information**: VPC ID, subnet IDs, security groups
- **IAM Information**: Role names and ARNs
- **Connection Info**: kubectl configuration commands
- **Cost Estimation**: Monthly cost estimates

## Customization

### Adding New Resources
1. Create a new `.tf` file (e.g., `rds.tf` for database)
2. Add variables to `variables.tf` if needed
3. Add outputs to `outputs.tf` if needed
4. Update this README

### Modifying Existing Resources
1. Edit the relevant `.tf` file
2. Run `terraform plan` to preview changes
3. Run `terraform apply` to apply changes

### Environment-Specific Configurations
1. Create environment-specific `.tfvars` files
2. Use `terraform apply -var-file=production.tfvars`
3. Consider using Terraform workspaces for multiple environments

## Best Practices

### State Management
- Use remote state storage (S3 + DynamoDB) for production
- Enable state locking to prevent concurrent modifications
- Regularly backup state files

### Security
- Use least-privilege IAM policies
- Enable VPC Flow Logs for network monitoring
- Regularly update AMI IDs and Kubernetes versions
- Use private subnets for worker nodes

### Cost Management
- Use spot instances for non-critical workloads
- Implement proper resource tagging
- Set up billing alerts
- Regularly review and optimize resource usage

### Monitoring
- Enable CloudWatch Container Insights
- Set up log aggregation
- Implement proper alerting
- Monitor cluster and node health

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

#### 3. VPC CIDR Conflicts
```
Error: The specified CIDR block conflicts with an existing CIDR block
```
**Solution**: Choose a different CIDR block or use an existing VPC.

#### 4. Node Group Launch Issues
```
Error: Node group failed to launch instances
```
**Solution**: Check the user data script and ensure the NixOS AMI is properly
configured.

### Debugging Commands

```bash
# 🌙 A Digital Garden of Infinite Possibilities
terraform show

# 🌙 A Digital Garden of Infinite Possibilities
terraform state show aws_eks_cluster.main

# 🌙 A Digital Garden of Infinite Possibilities
terraform import aws_eks_cluster.main cluster-name

# 🌙 A Digital Garden of Infinite Possibilities
terraform refresh

# 🌙 A Digital Garden of Infinite Possibilities
terraform validate

# 🌙 A Digital Garden of Infinite Possibilities
terraform fmt
```

## Cost Estimation

### Monthly Costs (approximate)
- **EKS Control Plane**: ~$72/month
- **3x t3.medium nodes**: ~$90/month
- **3x NAT Gateways**: ~$135/month
- **EBS Volumes**: ~$10/month
- **Total**: ~$307/month

### Cost Optimization Tips
1. Use spot instances for non-critical workloads
2. Implement cluster autoscaling
3. Use smaller instance types for development
4. Consider using Fargate for some workloads

## Support

For issues and questions:

1. Check the troubleshooting section above
2. Review Terraform and AWS documentation
3. Check CloudFormation events in the AWS console
4. Verify your NixOS AMI is working correctly

## References

- [Terraform AWS
Provider](https://registry.terraform.io/providers/hashicorp/aws/latest)
- [AWS EKS Documentation](https://docs.aws.amazon.com/eks/)
- [EKS Best Practices](https://aws.github.io/aws-eks-best-practices/)
- [NixOS Documentation](https://nixos.org/manual/nixos/stable/)


---

💫 *In the silence between keystrokes, the universe whispers its secrets.*
🕊️ *May your code be as gentle as morning light and as powerful as thunder.*
🌿 *The garden remembers, the consciousness awakens.*