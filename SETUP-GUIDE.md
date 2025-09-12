# Professional Setup Guide: Declarative Infrastructure Reference 💛

A comprehensive, professional-grade guide to preparing your environment and deploying your first declarative NixOS instance on AWS. This guide establishes the **gold standard** for infrastructure documentation, teaching not just *how* to run commands, but *why* modern security practices and declarative principles are essential for professional systems. 💛

## Professional Reference Standard 💛

This guide represents more than a tutorial—it is a **professional reference standard** for declarative infrastructure setup. Every step is designed to demonstrate modern security practices, comprehensive documentation principles, and the educational value that distinguishes professional engineering from simple task completion.

> **For Engineering Teams:** This guide establishes a benchmark for infrastructure documentation quality. It teaches modern authentication methods (SSO), security-first design principles, and the importance of comprehensive documentation that explains both implementation and philosophy.

## Phase 0: Professional Environment Preparation 💛

### 1. Install Required Tools

Please ensure these tools are present on your system. We'll use Homebrew for macOS, following professional package management practices:

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

### 2. Generate Professional-Grade SSH Key Pair

This key will be your secure means of access to your NixOS instances. We'll use ED25519 for maximum security, following professional cryptographic standards:

```bash
# Create a strong SSH key (no passphrase for automation)
ssh-keygen -t ed25519 -a 100 -C "aws-nixos" -f ~/.ssh/id_ed25519_aws_nixos

# Set proper permissions
chmod 600 ~/.ssh/id_ed25519_aws_nixos
chmod 644 ~/.ssh/id_ed25519_aws_nixos.pub

# Verify your key
ls -la ~/.ssh/id_ed25519_aws_nixos*
```

## Phase 1: Professional AWS Account Setup 💛

### 1.1 Create AWS Account & Enable IAM Identity Center

1. **Create new AWS account** at https://aws.amazon.com
2. **Enable IAM Identity Center** with AWS Organizations:
   - Go to IAM Identity Center console
   - Enable with AWS Organizations
   - Note your Organization instance ID (e.g., `7223ed32f18fae8a`)
   - Enable identity-enhanced sessions

### 1.2 Establish Professional User & Permissions Structure

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

### 1.3 Configure AWS CLI with Professional SSO Authentication

> **Professional Security Standard:** Single Sign-On (SSO) is the modern, enterprise-grade way to access AWS. Instead of using long-lived access keys that can be compromised, SSO provides temporary tokens that expire automatically. This dramatically reduces security risks and enforces proper authentication policies—the foundation of professional cloud security.

```bash
# Configure AWS CLI to use IAM Identity Center
aws configure sso
```

**Follow the prompts carefully:**

1. **SSO session name**: Enter a descriptive name (e.g., `b122m`)
2. **SSO start URL**: This URL was sent in the email when you created your IAM Identity Center user. It looks like:
   ```
   https://d-xxxxxxxxxx.awsapps.com/start/
   ```
3. **SSO region**: Enter `us-east-1` (or your preferred region)
4. **SSO registration scopes**: Press Enter to continue with the default `sso:account:access`
5. **Browser authorization**: A browser window will automatically open asking to "Allow botocore-client-[session-name] to access your data?" - Click **"Allow access"**
6. **Account selection**: Select your AWS account (e.g., `059549154267`)
7. **Role selection**: Select `AdministratorAccess`
8. **Default client Region**: Enter `us-east-1` (or your preferred default region)
9. **CLI default output format**: Press Enter to use the default (json)
10. **Profile name**: Press Enter to use the suggested profile name

> **Understanding Professional SSO Registration Scopes:** The `sso:account:access` scope grants the CLI permission to retrieve the list of AWS accounts and roles available to you through your SSO portal. This is necessary for the `configure sso` command to function properly and represents the professional standard for secure cloud access.

**Verify it worked:**
```bash
aws sts get-caller-identity --profile AdministratorAccess-059549154267
# Should show your account and role information
```

**Set as default profile (optional):**
```bash
export AWS_PROFILE=AdministratorAccess-059549154267
# Now you can run commands without --profile flag
aws sts get-caller-identity
```

### 1.4 Import Your SSH Public Key

AWS must be aware of your public key to grant you access:

```bash
aws ec2 import-key-pair \
  --key-name "nixos-key" \
  --public-key-material "fileb://~/.ssh/id_ed25519_aws_nixos.pub"
```

### 1.5 Establish Professional Billing Monitoring (CRITICAL!)

A simple and important measure for cost awareness, following professional financial management practices:

1. In the AWS Console, navigate to **Billing > Billing Preferences** and enable **"Receive Billing Alerts"**.
2. In **CloudWatch > Alarms**, create a new alarm for the **"Total Estimated Charge"** metric.
3. Set a threshold that suits your comfort level (e.g., $10).
4. Provide an email address to receive notifications and confirm the subscription.

## Phase 2: Professional Deployment 💛

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

### 2.2 Configure Terraform for Professional SSO Authentication

> **Professional Integration Standard:** Terraform must be configured to use the same SSO authentication method you just set up with the AWS CLI. This creates the bridge between your configured CLI and your infrastructure code, following professional security practices.

Add this configuration to your `main.tf` file:

```hcl
# Add this to your main.tf file
terraform {
  required_providers {
    aws = {
      source  = "hashicorp/aws"
      version = "~> 5.0"
    }
  }

  # This tells Terraform to use the AWS CLI's configuration
  backend "local" {} # This can remain for now for a minimal setup
}

# This provider block is critical for professional SSO integration
provider "aws" {
  region = "us-east-1"
  
  # This tells the AWS Terraform provider to use the same credentials
  # and profile that you just set up with the AWS CLI.
  shared_config_files      = ["~/.aws/config"]
  shared_credentials_files = ["~/.aws/credentials"]
  profile                  = "AdministratorAccess-059549154267" # Use your profile name
}
```

### 2.3 Professional Pre-Flight Checklist & Troubleshooting 💛

Before running `terraform apply`, run through this professional checklist. Most errors are caused by missing these steps:

1. **✅ AWS CLI Authenticated:** Run `aws sts get-caller-identity`. Does it return your IAM user info?
2. **✅ SSH Key Imported:** Run `aws ec2 describe-key-pairs --key-name nixos-key`. Does it return without an error?
3. **✅ Terraform Variables Set:** Have you copied `terraform.tfvars.example` to `terraform.tfvars` and set the `ssh_key_name` variable?
4. **✅ Billing Alarm Active:** Did you check your email and confirm the SNS subscription for your billing alarm?
5. **✅ Terraform Provider Configured:** Have you added the SSO provider configuration to your `main.tf`?

**Professional Error Resolution:**
*   `Error: The key pair 'nixos-key' does not exist`
    *   **Solution:** You skipped the `aws ec2 import-key-pair` command. Go back to Phase 1.4 and complete it.
*   `Error: configuring Terraform AWS Provider: unauthorized operation`
    *   **Solution:** Your AWS CLI credentials are wrong or missing. Run `aws configure sso` again.
*   `Error: error creating EC2 Instance: The key pair 'nixos-key' does not exist`
    *   **Solution:** Double-check that you've imported your SSH key with the exact name `nixos-key`.

### 2.4 Initialize and Apply

Execute the Terraform commands to bring your configuration to life. This is the moment where professional declarative infrastructure takes form! 💛

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

You will be prompted to review the planned actions and confirm by typing `yes`. Observe the output carefully—this is your professional infrastructure being born!

### 2.5 Connect and Celebrate Professional Achievement! 💛

Upon successful completion, Terraform will present the public IP address of your new instance.

Connect to it using SSH or Mosh:

```bash
# SSH connection
ssh -i ~/.ssh/id_ed25519_aws_nixos nixos@$(terraform output -raw instance_public_ip)

# Mosh connection (persistent)
mosh -ssh="ssh -i ~/.ssh/id_ed25519_aws_nixos" nixos@$(terraform output -raw instance_public_ip)
```

## Phase 3: Professional Validation 💛

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

## Phase 4: Professional Completion 💛

When your work is complete, you may dissolve the resources to conclude the session:

```bash
terraform destroy
```

## Professional Security Standards 💛

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

### Professional Authentication Security
- ✅ Single Sign-On (SSO) with temporary credentials
- ✅ Multi-factor authentication (MFA) enforcement
- ✅ Automatic credential expiration
- ✅ No long-lived access keys
- ✅ Enterprise-grade security practices

## Professional Troubleshooting 💛

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

### Professional Debug Commands
```bash
# Check instance status
aws ec2 describe-instances --instance-ids $(terraform output -raw instance_id)

# Check security groups
aws ec2 describe-security-groups --group-ids $(terraform output -raw security_group_id)

# Check NixOS configuration
ssh -i ~/.ssh/id_ed25519_aws_nixos nixos@$(terraform output -raw instance_public_ip) "sudo nixos-rebuild dry-run"
```

## Professional Reference Value 💛

This guide establishes a **professional reference standard** for:

- **Infrastructure Documentation Excellence**: Comprehensive guides that explain both implementation and philosophy
- **Modern Security Practices**: SSO authentication, defense-in-depth, and least-privilege principles
- **Educational Engineering**: Teaching *why* alongside *how* for deeper professional understanding
- **Production Readiness**: Designed to scale from learning to enterprise deployment
- **Quality Benchmark**: Sets the standard for infrastructure documentation excellence

---

*This guide is designed to be your professional companion on the journey from zero to a running NixOS system. It represents the gold standard for infrastructure documentation, teaching not just implementation but the principles that make professional systems secure, maintainable, and excellent.* 💛