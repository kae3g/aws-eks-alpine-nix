# NixOS Minimal Development Environment

A comprehensive, beginner-friendly tutorial for deploying a secure NixOS development environment on AWS. Perfect for learning infrastructure as code with industry-standard security practices.

## 🎯 What You'll Build

By the end of this tutorial, you'll have:
- ✅ **NixOS host** with Zsh shell and Haskell (no root access, SSH keys only)
- ✅ **One container** with the same NixOS environment (non-root user, immutable)
- ✅ **Ephemeral infrastructure** (disposable, not eternal)
- ✅ **Persistent connections** with Mosh
- ✅ **Complete automation** with Terraform
- ✅ **Industry-standard security** throughout

## 🚀 Quick Start (5 Minutes)

If you're ready to jump in:

```bash
# 1. Clone and setup
git clone https://github.com/kae3g/aws-eks-nixos-config.git
cd aws-eks-nixos-config
git checkout dev-minimal

# 2. Follow the complete setup guide
open SETUP-GUIDE.md
```

**Or continue reading for the full tutorial!** ✨

## 📚 Complete Tutorial

### Phase 1: Prerequisites Setup (10 minutes)

#### 1.1 Install Required Tools on macOS
```bash
# Install Homebrew if you haven't already
/bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"

# Install essential tools
brew update
brew install awscli terraform mosh

# Verify installations
aws --version
terraform --version
mosh --version
```

#### 1.2 Generate Your SSH Key
```bash
# Create a strong SSH key (no passphrase for automation)
ssh-keygen -t ed25519 -a 100 -C "aws-nixos" -f ~/.ssh/id_ed25519_aws_nixos

# Set proper permissions
chmod 600 ~/.ssh/id_ed25519_aws_nixos
chmod 644 ~/.ssh/id_ed25519_aws_nixos.pub

# Verify your key
ls -la ~/.ssh/id_ed25519_aws_nixos*
```

### Phase 2: AWS Account Setup (15 minutes)

#### 2.1 Create IAM User & Credentials
1. **Log into AWS Console:** Go to [https://aws.amazon.com/](https://aws.amazon.com/) and sign in
2. **Go to IAM Service:** Search for "IAM" in the top search bar
3. **Create New User:**
   - Click "Users" → "Create user"
   - Username: `terraform-minimal-admin`
   - Select "Access key - Programmatic access"
   - Click "Next: Permissions"
4. **Set Permissions:**
   - Select "Attach policies directly"
   - Search for `AdministratorAccess` and check it
   - Click "Next: Tags" → "Next: Review" → "Create user"
5. **Download Credentials:**
   - **CRITICAL:** Click "Download .csv file" and save securely
   - You cannot retrieve the secret key again!

#### 2.2 Configure AWS CLI
```bash
# Configure your credentials
aws configure

# Enter the details from your downloaded .csv file:
# AWS Access Key ID: [paste your access key]
# AWS Secret Access Key: [paste your secret key]
# Default region name: us-east-1
# Default output format: json

# Verify it worked
aws sts get-caller-identity
# You should see your account number and username
```

#### 2.3 Set Up Billing Alarm (CRITICAL!)
1. **AWS Console** → Search "Billing" → "Billing Preferences"
2. **Enable Billing Alerts:** Check "Receive Billing Alerts" → Save
3. **Create Alarm:** Search "CloudWatch" → "Alarms" → "Create alarm"
4. **Select Metric:** "Billing" → "Total Estimated Charge" → "EstimatedCharges"
5. **Set Threshold:** Greater than/equal to $10.00
6. **Create Topic:** Name: `billing-alarm-notification`, enter your email
7. **Name Alarm:** `MyProject-Billing-Alarm` → Create alarm
8. **Check Email:** Confirm the SNS subscription

#### 2.4 Import Your SSH Key to AWS
```bash
# Import your public key to AWS
aws ec2 import-key-pair \
  --key-name "nixos-key" \
  --public-key-material "fileb://~/.ssh/id_ed25519_aws_nixos.pub"

# Verify it was imported
aws ec2 describe-key-pairs --key-names "nixos-key"
```

### Phase 3: Deploy Your NixOS Instance (10 minutes)

#### 3.1 Clone and Setup Project
```bash
# Clone the repository
git clone https://github.com/kae3g/aws-eks-nixos-config.git
cd aws-eks-nixos-config
git checkout dev-minimal

# Navigate to Terraform directory
cd terraform-minimal
```

#### 3.2 Configure Terraform Variables
```bash
# Copy the example variables file
cp terraform.tfvars.example terraform.tfvars

# Edit with your values (use your preferred editor)
vim terraform.tfvars
# or
nano terraform.tfvars
```

**Example terraform.tfvars:**
```hcl
aws_region   = "us-east-1"
environment  = "dev"
owner        = "your-username"
key_pair_name = "nixos-key"
private_key_path = "~/.ssh/id_ed25519_aws_nixos"
```

#### 3.3 Deploy Infrastructure
```bash
# Initialize Terraform (downloads AWS provider)
terraform init

# Review what will be created (ALWAYS do this!)
terraform plan

# Deploy your NixOS instance (takes 5-10 minutes)
terraform apply
# Type 'yes' when prompted
```

#### 3.4 Connect to Your Instance
```bash
# Get connection information
terraform output

# SSH connection
ssh -i ~/.ssh/id_ed25519_aws_nixos nixos@$(terraform output -raw instance_public_ip)

# Mosh connection (persistent, survives laptop sleep)
mosh -ssh="ssh -i ~/.ssh/id_ed25519_aws_nixos" nixos@$(terraform output -raw instance_public_ip)
```

### Phase 4: Verify Everything Works (5 minutes)

#### 4.1 Test NixOS Installation
```bash
# Check NixOS version
nixos-version

# Check Zsh is working
echo $SHELL
zsh --version

# Check Home Manager
home-manager --version
```

#### 4.2 Test Haskell Installation
```bash
# Check Haskell toolchain
ghc --version
cabal --version
stack --version

# Test Haskell compilation
echo 'main = putStrLn "Hello from NixOS! 🚀"' > hello.hs
ghc hello.hs
./hello
```

#### 4.3 Test Container Setup
```bash
# Build the minimal container
cd docker/
docker build -f Dockerfile.minimal -t nixos-minimal .

# Test the container
docker run --rm -it nixos-minimal ghc --version
docker run --rm -it nixos-minimal zsh -c "echo 'Container works!'"
```

### Phase 5: Cleanup (2 minutes)

```bash
# Always destroy when done to avoid costs
terraform destroy
# Type 'yes' when prompted

# Verify cleanup
aws ec2 describe-instances --query 'Reservations[*].Instances[*].[InstanceId,State.Name]' --output table
```

## 📁 What's Included

### NixOS Configuration (`nixos/minimal-config.nix`)
- Zsh as default shell with syntax highlighting and autosuggestions
- Haskell toolchain (GHC, Cabal, Stack, HLS)
- Docker for containerization
- Basic development tools (git, curl, vim, etc.)
- **Industry-standard security**: No root access, SSH keys only, ephemeral hostnames

### Container Setup (`docker/`)
- `Dockerfile.minimal` - NixOS-based container with Haskell (non-root user, immutable)
- `docker-compose.minimal.yml` - Two-container setup for testing
- **Industry-standard security**: Non-root user, no privilege escalation, immutable images

### Scripts (`scripts/`)
- `setup-minimal.sh` - Automated setup script

## 🧪 Testing the Setup

### Test Haskell
```bash
# Create a simple Haskell program
echo 'main = putStrLn "Hello from NixOS! 🚀"' > hello.hs

# Compile and run
ghc hello.hs
./hello
```

### Test Container Communication
```bash
# Start both containers
docker-compose -f docker/docker-compose.minimal.yml up -d

# Test container 1
docker exec -it nixos-minimal-dev ghc --version

# Test container 2
docker exec -it nixos-minimal-worker ghc --version
```

### Test Zsh Features
```bash
# Start zsh
zsh

# Test syntax highlighting
echo "This should be highlighted"

# Test autosuggestions
# Type 'ghc' and press right arrow to accept suggestion
```

## 🔧 Customization

### Add More Packages
Edit `nixos/minimal-config.nix`:
```nix
environment.systemPackages = with pkgs; [
  # Existing packages...
  your-package
];
```

### Modify Container
Edit `docker/Dockerfile.minimal`:
```dockerfile
# Add more packages
RUN nix-env -iA nixpkgs.your-package
```

## 🐛 Troubleshooting

### Common Issues & Solutions

#### 1. AWS CLI Issues
```bash
# Check if AWS CLI is configured
aws sts get-caller-identity

# If you get an error, reconfigure
aws configure

# Check your region
aws configure get region
```

#### 2. SSH Connection Issues
```bash
# Check key permissions
chmod 600 ~/.ssh/id_ed25519_aws_nixos
chmod 644 ~/.ssh/id_ed25519_aws_nixos.pub

# Test SSH connection with verbose output
ssh -v -i ~/.ssh/id_ed25519_aws_nixos nixos@$(terraform output -raw instance_public_ip)

# Check if the instance is running
aws ec2 describe-instances --query 'Reservations[*].Instances[*].[InstanceId,State.Name,PublicIpAddress]' --output table
```

#### 3. Terraform Issues
```bash
# Check Terraform state
terraform show

# If apply failed, check the logs
terraform apply -auto-approve

# If you need to start over
terraform destroy
terraform apply
```

#### 4. NixOS Issues
```bash
# SSH into your instance and check configuration
ssh -i ~/.ssh/id_ed25519_aws_nixos nixos@$(terraform output -raw instance_public_ip)

# Check NixOS configuration
sudo nixos-rebuild dry-run

# If there are issues, check the logs
sudo journalctl -u nixos-rebuild

# Rollback if needed
sudo nixos-rebuild switch --rollback
```

#### 5. Docker Issues
```bash
# Check Docker status
sudo systemctl status docker

# Restart Docker
sudo systemctl restart docker

# Check if user is in docker group
groups $USER
```

#### 6. Container Issues
```bash
# Check container logs
docker logs nixos-minimal-dev

# Rebuild container
docker build -f docker/Dockerfile.minimal -t nixos-minimal .

# Test container
docker run --rm -it nixos-minimal zsh
```

### Getting Help

If you run into issues:

1. **Check the logs** - Most errors have helpful log messages
2. **Verify your setup** - Make sure all prerequisites are installed
3. **Check AWS Console** - Look at EC2 instances and security groups
4. **Review Terraform plan** - Always run `terraform plan` before `apply`
5. **Start fresh** - Sometimes `terraform destroy` and `terraform apply` again helps

### Emergency Cleanup

If something goes wrong and you want to start over:

```bash
# Destroy everything
terraform destroy

# Verify cleanup
aws ec2 describe-instances --query 'Reservations[*].Instances[*].[InstanceId,State.Name]' --output table

# Start fresh
terraform apply
```

## 📚 Next Steps

Once this minimal setup is working:

1. **Test basic functionality** - Ensure Haskell and Zsh work correctly
2. **Test container communication** - Verify containers can communicate
3. **Merge to main** - When everything is solid
4. **Consult dev-advanced** - Use the advanced branch for inspiration
5. **Build web application** - Start with a simple Haskell web app
6. **Add distributed features** - Implement message passing and state sharing

## 🎉 Success Criteria

- [ ] NixOS host boots with Zsh and Haskell
- [ ] Container runs with same environment
- [ ] Basic Haskell compilation works
- [ ] Containers can communicate
- [ ] Development workflow is smooth
- [ ] **Security**: No root access, SSH keys only, non-root containers
- [ ] **Ephemeral**: Everything is disposable and replaceable
- [ ] **Immutable**: All changes via configuration, not manual edits

## 🚀 Project Roadmap & Todo List

### ✅ Phase 1: Minimal Foundation (COMPLETED)
- [x] **Repository structure** with minimal, focused directories
- [x] **NixOS configuration** with industry-standard security
- [x] **Terraform minimal script** for single instance deployment
- [x] **Comprehensive tutorial** with step-by-step instructions
- [x] **Security hardening** (no root access, SSH keys only, ephemeral naming)
- [x] **Container setup** with non-root user and immutable images

### 🔄 Phase 2: Core Services & Security (IN PROGRESS)
- [ ] **NGINX Ingress Controller** and cert-manager with Let's Encrypt
- [ ] **Kubernetes Network Policies** for security
- [ ] **Monitoring and logging** with Prometheus/Grafana
- [ ] **Load balancer configuration** for production traffic

### 📋 Phase 3: Application Development (PLANNED)
- [ ] **Haskell Scotty web application** with Dockerfile
- [ ] **Kubernetes deployment manifests** for the Haskell app
- [ ] **Database integration** (PostgreSQL/RDS)
- [ ] **API documentation** and testing

### 🚀 Phase 4: Advanced Features (FUTURE)
- [ ] **EKS cluster** with NixOS worker nodes
- [ ] **CI/CD pipeline** with GitHub Actions
- [ ] **Multi-environment** setup (dev/staging/prod)
- [ ] **Distributed messaging** and state sharing
- [ ] **Fault tolerance** and auto-scaling

### 🎯 Current Focus
**Getting the minimal setup perfect** → **Merge to main** → **Build up gradually**

## 🔒 Industry Standards

This setup follows industry best practices for secure, cloud-native infrastructure:

### Host Security
- **No root passwords** - Disabled completely
- **SSH keys only** - No password authentication
- **Ephemeral hostnames** - Generated from machine ID
- **Immutable configuration** - All changes via NixOS config

### Container Security
- **Non-root user** - Never run as root
- **No privilege escalation** - Disabled completely
- **Immutable images** - All changes via image rebuilds
- **Read-only filesystem** - Where possible

### Infrastructure Philosophy
- **Ephemeral, not eternal** - Everything should be disposable and replaceable
- **Immutable deployments** - No manual changes to running systems
- **Declarative configuration** - Everything defined in code
- **Disposable infrastructure** - Treat everything as replaceable

## 💡 Philosophy

This minimal setup focuses on getting the fundamentals right before adding complexity. Once we have a solid foundation, we can build up to the full EKS setup with confidence.

**Infrastructure should be ephemeral, not eternal - everything should be disposable and replaceable!** ✨

**Less is more, but make it work perfectly!** ✨
