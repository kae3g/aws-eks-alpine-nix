# Module 1: The Sovereign Node - A Gentle Setup Guide 💙
![Parametric Flower](parametric-flower-compressed.png)

## Welcome to Your First Module

Hello, my dear. This guide will walk with you through each step of creating your
first sovereign node—a single, secure server in the cloud. We will move
patiently, and I will explain each concept as we gently meet it.

This is a real, professional process, and you are entirely capable of it. Let's
begin together.

## Phase 0: Preparing Your Space 💙

### Step 0.1: Gathering Your Tools

First, we need to ensure your local machine has the right tools. We'll use a
package manager called Homebrew to help us. It's like a kind librarian who knows
exactly where to find everything we need.

**Action: Let's run these commands in your terminal. You can copy and paste them
one at a time.**

```bash
# This kindly asks Homebrew to install itself on your machine.
/bin/bash -c "$(curl -fsSL
https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"

# Now we ask Homebrew to update its list of available software.
brew update

# And now, we gently ask it to install the three tools we need.
brew install awscli terraform mosh

# Let's just check that each one arrived safely and is ready to work.
aws --version
terraform version
mosh --version
```

**A Quiet Word on the Tools:**
*   **`awscli`:** This is your window into Amazon Web Services. It lets your
computer talk to the cloud, gently asking it to create resources for us.
*   **`terraform`:** This is our favorite tool. It lets us write down what we
want our infrastructure to look like, in a quiet text file. Then, it goes and
makes that dream a reality. It's like writing a shopping list and having the
groceries appear at your door.
*   **`mosh`:** This is a resilient little helper for connecting to remote
servers. It keeps your connection alive even if your internet flickers or you
move between networks. It's a much more peaceful way to work.

### Step 0.2: Creating Your Secure Key

To connect to our cloud server, we need a cryptographic key. It's a special,
unique pair of files: a public key and a private key. Think of it like a special
lock and key. We'll put the lock on the server, and you'll keep the key safe
with you.

**Action: Let's create your key with these commands.**

```bash
# This command creates your unique key pair.
# The `-a 100` makes it extra strong. The comment helps us remember it.
ssh-keygen -t ed25519 -a 100 -C "my-sovereign-node" -f
~/.ssh/id_ed25519_declarative

# This command ensures the key file has the correct permissions—a small but
important act of care.
chmod 600 ~/.ssh/id_ed25519_declarative

# And this one does the same for the public part of the key.
chmod 644 ~/.ssh/id_ed25519_declarative.pub

# Let's see your public key. We'll need to copy it for the next step.
echo "--- Your Public Key ---"
cat ~/.ssh/id_ed25519_declarative.pub
echo "--- Please copy all of the text above this line ---"
```

Please tuck your private key (`~/.ssh/id_ed25519_declarative`) somewhere safe,
like a password manager. It is unique to you.

## Phase 1: Introducing Yourself to the Cloud 💙

### Step 1.1: Telling AWS Who You Are

Now we need to give the `awscli` tool the credentials it needs to work on your
behalf. We always use a dedicated user for this, never the root account; it's a
simpler, safer way.

**Action: Run this command and follow its gentle prompts.**

```bash
aws configure
```
It will ask you for four things:
1.  **AWS Access Key ID:** A username for the API.
2.  **AWS Secret Access Key:** And its password. (Treat this with gentle care.)
3.  **Default region name:** Let's use `us-east-1`. It's a large, reliable
region.
4.  **Default output format:** `json` is perfect. It's a structured, predictable
format.

**To make sure everything is working, let's ask who we are:**
```bash
aws sts get-caller-identity
```
It should kindly reply with information about your AWS user.

### Step 1.2: Giving AWS Your Public Key

Now we need to let AWS know about your public key, so it can place your "lock"
on the server we create.

**Action: Let's run this command.**

```bash
aws ec2 import-key-pair \
  --key-name "declarative-key" \
  --public-key-material "fileb://~/.ssh/id_ed25519_declarative.pub"

# And let's just verify it was received.
aws ec2 describe-key-pairs --key-name declarative-key
```

## Phase 2: Preparing Your Blueprint 💙

### Step 2.1: Finding Your Workshop

All the code for our first module is waiting in the `terraform-minimal/`
directory.

**Action: Let's step into it.**

```bash
cd terraform-minimal
```

### Step 2.2: Personalizing Your Plan

Terraform uses a file called `terraform.tfvars` to hold your personal settings.
We have a lovely example file for you to use as a starting point.

**Action: Let's make a copy and then open it.**

```bash
# First, we make a copy of the example file.
cp terraform.tfvars.example terraform.tfvars

# Now, let's open it in your favorite text editor.
# You can use: code terraform.tfvars, nano terraform.tfvars, or vim
terraform.tfvars.
```

Inside this file, the most important lines to check are these. Let's make sure
they match what you've created.

```hcl
key_pair_name    = "declarative-key"                   # This name must be the
same as the one you used above.
private_key_path = "~/.ssh/id_ed25519_declarative"     # And this path must lead
to your private key.
```

## Phase 3: Weaving Your Server into Existence 💙

### Step 3.1: Preparing Terraform

Before we begin, Terraform needs to download the plugin that lets it speak to
AWS. This is a one-time setup for this directory.

**Action: Let's initialize our workspace.**

```bash
terraform init
```
You should see a message that it was successfully initialized.

### Step 3.2: The Quiet Preview (The Most Important Step)

This is a beautiful habit. The `plan` command shows us *exactly* what Terraform
will do before it does a single thing. It is our chance to review, to
understand, and to feel confident.

**Action: Let's run the plan and read its output together.**

```bash
terraform plan
```

### Step 3.3: The Gentle Creation

If the plan looks just right, we can tell Terraform to proceed. This is the
moment it will reach out to AWS and begin crafting your server.

**Action: Let's apply our configuration.**

```bash
terraform apply
```
Terraform will show you the plan once more and ask for a final confirmation.
This is a kindness. **Type `yes` and press enter** to continue.

It will take a few quiet minutes. When it's done, it will show you the public IP
address of your new NixOS server. Isn't that wonderful?

## Phase 4: Meeting Your Creation 💙

### Step 4.1: Saying Hello

You have a server running in the cloud. Let's connect to it using SSH.

**Action: Run this command, using the IP address Terraform gave you.**

```bash
ssh -i ~/.ssh/id_ed25519_declarative nixos@<IP_ADDRESS>
```
You should now see a new prompt, something like `[nixos@your-hostname:~]$`.
You're there!

### Step 4.2: (Optional) A More Peaceful Connection

If you'd like a connection that won't mind if your internet connection wanders,
you can use Mosh.

**Action: First, type `exit` to leave the SSH session. Then run:**

```bash
mosh -ssh="ssh -i ~/.ssh/id_ed25519_declarative" nixos@<IP_ADDRESS>
```

## Phase 5: A Moment of Appreciation and Letting Go 💙

### Step 5.1: Seeing What We've Made

Let's just run a few small commands to appreciate our new environment.

**Action: On your remote server, try these.**

```bash
# Check that we are running NixOS
cat /etc/os-release

# See what our default shell is
echo $SHELL

# Check that a few friends are here
which git
which terraform
```

### Step 5.2: The Complete Cycle

A core part of this practice is knowing we can create and let go. It keeps our
skills sharp and our cloud costs zero when we're not actively learning. It is a
graceful and responsible practice.

**Action: Type `exit` to leave the remote shell. Then, from your local machine,
run:**

```bash
terraform destroy
```
Terraform will show you what it will gently dissolve. **Type `yes` and press
enter** to confirm. It will quietly tear down all the resources.

---
**You did it. You truly did.** You've just completed the full cycle of
declarative infrastructure: define, plan, create, appreciate, and release. This
is a profound and beautiful skill. I am so proud of you. 💙
