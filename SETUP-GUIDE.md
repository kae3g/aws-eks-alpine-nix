# Minimal NixOS AWS Setup Guide

**Updated for 2025-09-11** - Note: Some versions may need verification as of this date.

## 🎯 Goal

Deploy a minimal NixOS instance on AWS with:
- NixOS 24.11 (latest stable as of 2025-09-11)
- Zsh with Home Manager
- Haskell toolchain
- Mosh for persistent connections
- Industry-standard security practices
- Ephemeral, not eternal infrastructure

## 📋 Prerequisites

### 1. macOS Setup
```bash
# Install Homebrew if not already installed
/bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"

# Install required tools
brew update
brew install awscli terraform mosh
```

### 2. Generate SSH Key
```bash
# Create a strong SSH key (no passphrase for automation)
ssh-keygen -t ed25519 -a 100 -C "aws-nixos" -f ~/.ssh/id_ed25519_aws_nixos

# Set proper permissions
chmod 600 ~/.ssh/id_ed25519_aws_nixos
chmod 644 ~/.ssh/id_ed25519_aws_nixos.pub
```

### 3. AWS Account Setup

#### 3.1 Create AWS Account & Enable IAM Identity Center
1. **Create new AWS account** at https://aws.amazon.com
2. **Enable IAM Identity Center** with AWS Organizations:
   - Go to IAM Identity Center console
   - Enable with AWS Organizations
   - Note your Organization instance ID (e.g., `7223ed32f18fae8a`)
   - Enable identity-enhanced sessions

#### 3.2 Set Up User & Permissions
1. **Create admin group**:
   - Go to IAM Identity Center → Groups → Create group
   - Name: `admin`
   - Description: `Administrative access group for AWS EKS NixOS development`

2. **Create user**:
   - Go to IAM Identity Center → Users → Add user
   - Username: `kae3g` (or your preferred username)
   - Email: Use your main email address
   - Add user to `admin` group

3. **Create permission set**:
   - Go to IAM Identity Center → Permission sets → Create permission set
   - Name: `AdministratorAccess`
   - Attach AWS managed policy: `AdministratorAccess`

4. **Assign permissions**:
   - Go to IAM Identity Center → AWS accounts
   - Select your management account
   - Assign users or groups: select `admin` group
   - Assign permission set: select `AdministratorAccess`
   - Reprovision the account

5. **Set up MFA**:
   - User will receive email to set up password
   - Register authenticator app for MFA
   - Complete user setup

#### 3.3 Configure AWS CLI
```bash
# Configure AWS CLI to use IAM Identity Center
aws configure sso

# Follow prompts:
# - SSO start URL: https://your-org.awsapps.com/start
# - SSO region: us-east-1 (or your region)
# - Account ID: 0595-4915-4267 (your account ID)
# - Role name: AdministratorAccess
# - Default region: us-west-2
# - Default output format: json

# Import your SSH public key to AWS
aws ec2 import-key-pair \
  --key-name "nixos-key" \
  --public-key-material "fileb://~/.ssh/id_ed25519_aws_nixos.pub"
```

### 4. Set Up Billing Alerts (CRITICAL)
1. Go to AWS Console → Billing → Billing Preferences
2. Enable billing alerts
3. Go to CloudWatch → Alarms → Create billing alarm
4. Set threshold (e.g., $10) to avoid surprise charges

## 🚀 Deployment

### 1. Clone and Setup
```bash
git clone <your-repo>
cd aws-eks-nixos-config
git checkout dev-minimal
```

### 2. Configure Terraform
```bash
cd terraform-minimal
cp terraform.tfvars.example terraform.tfvars

# Edit terraform.tfvars with your values
vim terraform.tfvars
```

### 3. Deploy Infrastructure
```bash
# Initialize Terraform
terraform init

# Review the plan
terraform plan

# Deploy (this will take 5-10 minutes)
terraform apply
```

### 4. Connect to Your Instance
```bash
# Get the connection commands
terraform output

# SSH connection
ssh -i ~/.ssh/id_ed25519_aws_nixos nixos@$(terraform output -raw instance_public_ip)

# Mosh connection (persistent)
mosh -ssh="ssh -i ~/.ssh/id_ed25519_aws_nixos" nixos@$(terraform output -raw instance_public_ip)
```

## 🧪 Testing

### 1. Verify NixOS
```bash
# Check NixOS version
nixos-version

# Check Zsh is working
echo $SHELL
zsh --version
```

### 2. Verify Haskell
```bash
# Check Haskell installation
ghc --version
cabal --version
stack --version
```

### 3. Verify Mosh
```bash
# Test Mosh connection
mosh -ssh="ssh -i ~/.ssh/id_ed25519_aws_nixos" nixos@$(terraform output -raw instance_public_ip)
```

### 4. Test Container
```bash
# Build and run the minimal container
cd docker/
docker build -f Dockerfile.minimal -t nixos-minimal .
docker run --rm -it nixos-minimal ghc --version
```

## 🔒 Security Features

### Host Security
- ✅ No root password (disabled)
- ✅ SSH keys only (no password auth)
- ✅ Ephemeral hostnames
- ✅ Firewall configured for SSH and Mosh
- ✅ Non-root user with sudo access

### Container Security
- ✅ Non-root user (appuser)
- ✅ No privilege escalation
- ✅ Immutable images
- ✅ Minimal attack surface

## 🧹 Cleanup

### Destroy Infrastructure
```bash
# Always destroy when done to avoid costs
terraform destroy
```

### Verify Cleanup
```bash
# Check AWS Console to ensure resources are deleted
aws ec2 describe-instances --query 'Reservations[*].Instances[*].[InstanceId,State.Name]' --output table
```

## 📚 Next Steps

1. **Test the minimal setup** - Ensure everything works perfectly
2. **Merge to main** - When you're confident it's solid
3. **Consult dev-advanced** - Use the advanced branch for inspiration
4. **Build web application** - Start with a simple Haskell web app
5. **Add distributed features** - Implement message passing and state sharing

## 🐛 Troubleshooting

### Common Issues

#### 1. SSH Connection Failed
```bash
# Check key permissions
chmod 600 ~/.ssh/id_ed25519_aws_nixos

# Check security group
aws ec2 describe-security-groups --group-names "*nixos*"
```

#### 2. Terraform Apply Failed
```bash
# Check AWS credentials
aws sts get-caller-identity

# Check region
aws configure get region
```

#### 3. NixOS Rebuild Failed
```bash
# SSH into instance and check logs
sudo journalctl -u nixos-rebuild
sudo nixos-rebuild switch --show-trace
```

### Debug Commands
```bash
# Check instance status
aws ec2 describe-instances --instance-ids $(terraform output -raw instance_id)

# Check security groups
aws ec2 describe-security-groups --group-ids $(terraform output -raw security_group_id)

# Check NixOS configuration
ssh -i ~/.ssh/id_ed25519_aws_nixos nixos@$(terraform output -raw instance_public_ip) "sudo nixos-rebuild dry-run"
```

## 💡 Philosophy

**Infrastructure should be ephemeral, not eternal - everything should be disposable and replaceable!**

This minimal setup focuses on getting the fundamentals right before adding complexity. Once we have a solid foundation, we can build up to the full EKS setup with confidence.

**Less is more, but make it work perfectly!** ✨

## ⚠️ Version Notes

**As of 2025-09-11:**
- NixOS: 24.11 (latest stable)
- Kubernetes: 1.31+ (latest stable)
- Haskell GHC: 9.8+ (latest stable)
- Terraform: 1.6+ (latest stable)
- AWS Provider: 5.0+ (latest stable)

**Note to future self:** Some versions may need verification as of 2025-09-11. Check official documentation for the most current versions.
