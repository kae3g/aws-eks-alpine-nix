# Setup Guide 💛

A calm, step-by-step guide to preparing your environment and deploying your first declarative NixOS instance on AWS. This guide will take you from zero to a running, secure NixOS system in the cloud. 💛

## Phase 0: Local Preparation 💛

### 1. Install Required Tools

Please ensure these tools are present on your system. We'll use Homebrew for macOS:

```bash
# Update Homebrew
brew update

# Install essential tools
brew install awscli terraform mosh

# Verify installations
aws --version
terraform --version
mosh --version
```

### 2. Generate an SSH Key Pair

This key will be your secure means of access to your NixOS instances. We'll use ED25519 for maximum security:

```bash
# Create a strong SSH key (no passphrase for automation)
ssh-keygen -t ed25519 -a 100 -C "aws-nixos" -f ~/.ssh/id_ed25519_aws_nixos

# Set proper permissions
chmod 600 ~/.ssh/id_ed25519_aws_nixos
chmod 644 ~/.ssh/id_ed25519_aws_nixos.pub

# Verify your key
ls -la ~/.ssh/id_ed25519_aws_nixos*
```

## Phase 1: AWS Account Setup 💛

### 1.1 Create AWS Account & Enable IAM Identity Center

1. **Create new AWS account** at https://aws.amazon.com
2. **Enable IAM Identity Center** with AWS Organizations:
   - Go to IAM Identity Center console
   - Enable with AWS Organizations
   - Note your Organization instance ID (e.g., `7223ed32f18fae8a`)
   - Enable identity-enhanced sessions

### 1.2 Set Up User & Permissions

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

### 1.3 Configure AWS CLI

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

# Verify it worked
aws sts get-caller-identity
# Should show your account and role information
```

### 1.4 Import Your SSH Public Key

AWS must be aware of your public key to grant you access:

```bash
aws ec2 import-key-pair \
  --key-name "nixos-key" \
  --public-key-material "fileb://~/.ssh/id_ed25519_aws_nixos.pub"
```

### 1.5 Establish a Billing Alert (CRITICAL!)

A simple and important measure for cost awareness:

1. In the AWS Console, navigate to **Billing > Billing Preferences** and enable **"Receive Billing Alerts"**.
2. In **CloudWatch > Alarms**, create a new alarm for the **"Total Estimated Charge"** metric.
3. Set a threshold that suits your comfort level (e.g., $10).
4. Provide an email address to receive notifications and confirm the subscription.

## Phase 2: Deployment 💛

### 2.1 Navigate and Configure

Enter the project directory and establish your variables:

```bash
cd terraform-minimal
cp terraform.tfvars.example terraform.tfvars
```

Edit the `terraform.tfvars` file to ensure the key name matches what you imported to AWS:

```hcl
ssh_key_name = "nixos-key"
```

### 2.2 Pre-Flight Checklist & Troubleshooting 💛

Before running `terraform apply`, run through this checklist. Most errors are caused by missing these steps:

1. **✅ AWS CLI Authenticated:** Run `aws sts get-caller-identity`. Does it return your IAM user info?
2. **✅ SSH Key Imported:** Run `aws ec2 describe-key-pairs --key-name nixos-key`. Does it return without an error?
3. **✅ Terraform Variables Set:** Have you copied `terraform.tfvars.example` to `terraform.tfvars` and set the `ssh_key_name` variable?
4. **✅ Billing Alarm Active:** Did you check your email and confirm the SNS subscription for your billing alarm?

**Common Errors:**
*   `Error: The key pair 'nixos-key' does not exist`
    *   **Solution:** You skipped the `aws ec2 import-key-pair` command. Go back to Phase 1.4 and complete it.
*   `Error: configuring Terraform AWS Provider: unauthorized operation`
    *   **Solution:** Your AWS CLI credentials are wrong or missing. Run `aws configure sso` again.
*   `Error: error creating EC2 Instance: The key pair 'nixos-key' does not exist`
    *   **Solution:** Double-check that you've imported your SSH key with the exact name `nixos-key`.

### 2.3 Initialize and Apply

Execute the Terraform commands to bring your configuration to life. This is the moment of truth—seeing your declarative infrastructure take form! 💛

```bash
# Initialize Terraform and download the required AWS provider
terraform init

# Perform a dry-run to see what Terraform will create
# This is a critical check for any errors before making changes
terraform plan

# If the plan looks correct and only shows actions to 'add' resources, proceed
# This command will create the actual resources in your AWS account
terraform apply
```

You will be prompted to review the planned actions and confirm by typing `yes`. Observe the output carefully—this is your infrastructure being born!

### 2.4 Connect and Celebrate! 💛

Upon successful completion, Terraform will present the public IP address of your new instance.

Connect to it using SSH or Mosh:

```bash
# SSH connection
ssh -i ~/.ssh/id_ed25519_aws_nixos nixos@$(terraform output -raw instance_public_ip)

# Mosh connection (persistent)
mosh -ssh="ssh -i ~/.ssh/id_ed25519_aws_nixos" nixos@$(terraform output -raw instance_public_ip)
```

## Phase 3: Validation 💛

### 3.1 Verify Your NixOS System

Once connected, verify that everything is working as expected. This validates that your declarative configuration has been applied correctly:

```bash
# Confirm the operating system
cat /etc/os-release

# Check NixOS version
nixos-version

# Check that Zsh is your default shell
echo $SHELL
zsh --version

# Verify your declared packages are present
which ghc
which mosh

# Check Haskell installation
ghc --version
cabal --version
stack --version
```

**What to observe:** Each command should return the expected results, confirming that your NixOS configuration has been applied successfully and all declared packages are available.

### 3.2 Test Container Environment

```bash
# Build and run the minimal container
cd docker/
docker build -f Dockerfile.minimal -t nixos-minimal .
docker run --rm -it nixos-minimal ghc --version
```

## Phase 4: Completion 💛

When your work is complete, you may dissolve the resources to conclude the session:

```bash
terraform destroy
```

## Security Features 💛

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

## Troubleshooting 💛

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

---

*This guide is designed to be your companion on the journey from zero to a running NixOS system. Take your time, read carefully, and don't hesitate to pause and understand each step. The goal is not just to get it working, but to understand why it works.* 💛