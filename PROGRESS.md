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

### Step 2: Configure AWS Account & CLI ✅
- [x] Created new AWS account
- [x] Enabled IAM Identity Center with AWS Organizations
- [x] Organization instance created: 7223ed32f18fae8a
- [x] Enabled identity-enhanced sessions
- [x] Created 'admin' group in IAM Identity Center
- [x] Added user 'kae3g' to 'admin' group
- [x] Created 'AdministratorAccess' permission set
- [x] Reprovisioned AWS account 'kj3x39 management account' with updated permission set
- [x] Created user 'kae3g' in IAM Identity Center
- [x] Registered authenticator app (MFA device) for user 'kae3g'

**Status**: AWS account and IAM Identity Center setup complete, user 'kae3g' configured with admin access and MFA!

## 🔄 Current Step

**Step 3: Set up Terraform backend**

Next, we will configure the Terraform backend to store our state remotely, typically in an S3 bucket.

## 📋 Next Steps

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

### Step 2 Notes (AWS Account & CLI Setup):
- New AWS account created successfully (kj3x39 management account)
- IAM Identity Center enabled with AWS Organizations (instance ID: 7223ed32f18fae8a)
- Identity-enhanced sessions enabled
- 'admin' group created and user 'kae3g' added successfully
- 'AdministratorAccess' permission set created and applied to management account
- User 'kae3g' created with MFA (authenticator app) registered successfully
- **Next**: Configure AWS CLI to use IAM Identity Center credentials

---

*Last updated: September 11, 2025*
