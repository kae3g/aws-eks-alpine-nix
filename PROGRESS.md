# AWS EKS NixOS Setup Progress 🚀

Tracking our journey through the AWS EKS NixOS configuration setup.

## ✅ Completed Steps

### Step 1: Install Required Tools ✅
- [x] Update Homebrew
- [x] Install AWS CLI (v2.29.1)
- [x] Install Terraform (v1.5.7)
- [x] Install Mosh (v1.4.0)
- [x] Verify installations

**Status**: All tools successfully installed and verified!

### Step 1.5: Create SSH Key for Automation ✅
- [x] Generate ED25519 SSH key with 100 rounds of KDF
- [x] Set proper permissions (600 for private, 644 for public)
- [x] Verify key creation and permissions

**Status**: SSH key created successfully for AWS NixOS automation!

## 🔄 Current Step

**Step 2: Configure AWS Account & CLI (In Progress)**

AWS Account Setup:
- ✅ Created new AWS account
- ✅ Enabled IAM Identity Center with AWS Organizations
- ✅ Organization instance created: 7223ed32f18fae8a
- ✅ Enabled identity-enhanced sessions
- 🔄 **Current**: Setting up identity source in IAM Identity Center
- ⏳ Next: Configure user access to AWS accounts with permission sets
- ⏳ Next: Configure AWS CLI with credentials

## 📋 Next Steps

- Step 2: Configure AWS CLI
- Step 3: Set up Terraform backend
- Step 4: Deploy EKS cluster
- Step 5: Configure NixOS worker nodes

## 🐛 Issues & Notes

### Step 1 Notes:
- Terraform v1.5.7 installed (latest open-source version before BUSL license change)
- All tools verified and working correctly
- Mosh was already installed and up-to-date

### Step 1.5 Notes:
- SSH key created with ED25519 algorithm and 100 rounds of KDF for security
- No passphrase set for automation compatibility
- Key fingerprint: SHA256:N0+nXZjpHydCUR6s2/YrBqTlxhmZj3/v912XyWnC4xQ
- Proper permissions set (600 for private key, 644 for public key)

### Step 2 Notes (AWS Account Setup):
- New AWS account created successfully
- IAM Identity Center enabled with AWS Organizations
- Organization instance ID: 7223ed32f18fae8a
- Identity-enhanced sessions enabled
- **Current Challenge**: Need to configure identity source in IAM Identity Center
  - Options: External identity provider, Active Directory, or IAM Identity Center managed users
  - Then set up user access to AWS accounts with permission sets

---

*Last updated: September 11, 2025*
