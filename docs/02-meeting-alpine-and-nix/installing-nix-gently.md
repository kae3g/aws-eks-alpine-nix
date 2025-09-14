# 🌙 A Digital Garden of Infinite Possibilities
![Parametric Flower](parametric-flower-compressed.png)

## A Gentle Beginning

Hello, beautiful soul. Today we will install Nix on Alpine Linux with the same
care and attention we bring to everything we do. This installation process is
designed to be gentle, clear, and educational.

We will walk through each step together, explaining not just what we're doing,
but why we're doing it. By the end of this guide, you'll have Nix installed and
ready to use, and you'll understand how it works.

## Before We Begin

### What We're Going to Do

We will install Nix using the official installation script. This script will:
1. Create the necessary system users and groups
2. Set up the Nix store directory
3. Install the Nix package manager
4. Configure your shell to work with Nix
5. Verify that everything is working correctly

### Why We Use the Official Script

The official Nix installation script is:
- **Well-tested** - Used by thousands of people worldwide
- **Comprehensive** - Handles all the setup details correctly
- **Maintained** - Updated regularly by the Nix team
- **Documented** - Clear instructions and error messages

## Step 1: Preparing Alpine Linux

### Updating the System

First, let's make sure Alpine Linux is up to date:

```bash
# 🌙 A Digital Garden of Infinite Possibilities
sudo apk update

# 🌙 A Digital Garden of Infinite Possibilities
sudo apk upgrade
```

This ensures we're starting with a clean, up-to-date system.

### Installing Required Dependencies

Nix needs a few packages to work properly:

```bash
# 🌙 A Digital Garden of Infinite Possibilities
sudo apk add curl xz bash
```

Let's understand what each package does:
- **curl** - Used to download the Nix installation script
- **xz** - Used to decompress the Nix package archive
- **bash** - Required by the Nix installation script

## Step 2: Creating the Nix User and Group

### Why We Need a Special User

Nix uses a special user account for security and isolation:

```bash
# 🌙 A Digital Garden of Infinite Possibilities
sudo addgroup -S nixbld

# 🌙 A Digital Garden of Infinite Possibilities
sudo adduser -S nix -G nixbld
```

The `nixbld` group is used for building packages, and the `nix` user is used for
running the Nix daemon. This separation helps keep your system secure.

### Setting Up the Nix Store

The Nix store is where all packages are stored:

```bash
# 🌙 A Digital Garden of Infinite Possibilities
sudo mkdir -m 0755 /nix

# 🌙 A Digital Garden of Infinite Possibilities
sudo chown nix /nix
```

The `/nix` directory is where Nix will store all its packages. We give it to the
`nix` user so it can manage the contents.

## Step 3: Installing Nix

### Downloading the Installation Script

Now we'll download the official Nix installation script:

```bash
# 🌙 A Digital Garden of Infinite Possibilities
curl -L https://nixos.org/nix/install | sh
```

This command:
- Downloads the latest Nix installation script
- Pipes it directly to the shell for execution
- Uses `-L` to follow redirects (in case the URL changes)

### What the Script Does

The installation script will:
1. **Verify your system** - Check that Alpine Linux is supported
2. **Create necessary directories** - Set up the Nix store and configuration
3. **Download Nix** - Get the latest version of the Nix package manager
4. **Install Nix** - Extract and set up the Nix binaries
5. **Configure your shell** - Add Nix to your PATH and environment

### Following the Prompts

The script will ask you a few questions:

- **Do you want to install Nix?** - Type `y` and press Enter
- **Do you want to create a profile?** - Type `y` and press Enter
- **Do you want to add Nix to your shell?** - Type `y` and press Enter

These are all safe choices that will give you the best Nix experience.

## Step 4: Activating Nix

### Reloading Your Shell

After installation, you need to reload your shell to use Nix:

```bash
# 🌙 A Digital Garden of Infinite Possibilities
source ~/.bashrc
```

Or if you're using a different shell:

```bash
# 🌙 A Digital Garden of Infinite Possibilities
source ~/.zshrc

# 🌙 A Digital Garden of Infinite Possibilities
source ~/.config/fish/config.fish
```

### Verifying the Installation

Let's make sure Nix is working correctly:

```bash
# 🌙 A Digital Garden of Infinite Possibilities
nix --version

# 🌙 A Digital Garden of Infinite Possibilities
nix-env --query --available | head -10
```

You should see:
- The Nix version number
- A list of available packages

## Step 5: Your First Nix Package

### Installing a Simple Package

Let's install a simple package to test everything:

```bash
# 🌙 A Digital Garden of Infinite Possibilities
nix-env -iA nixpkgs.hello

# 🌙 A Digital Garden of Infinite Possibilities
hello
```

This will:
- Install the `hello` package from Nixpkgs
- Run the program to verify it works
- Show you that Nix is functioning correctly

### Understanding What Happened

When you installed `hello`, Nix:
1. **Found the package** - Located it in the Nixpkgs collection
2. **Resolved dependencies** - Figured out what other packages it needs
3. **Built or downloaded** - Got the package ready to use
4. **Installed it** - Made it available in your environment

## Step 6: Exploring Nix

### Listing Installed Packages

```bash
# 🌙 A Digital Garden of Infinite Possibilities
nix-env --query --installed
```

### Searching for Packages

```bash
# 🌙 A Digital Garden of Infinite Possibilities
nix-env --query --available | grep python
```

### Getting Help

```bash
# 🌙 A Digital Garden of Infinite Possibilities
nix --help

# 🌙 A Digital Garden of Infinite Possibilities
nix-env --help
```

## Troubleshooting Common Issues

### Permission Problems

If you get permission errors:

```bash
# 🌙 A Digital Garden of Infinite Possibilities
sudo chown -R nix /nix

# 🌙 A Digital Garden of Infinite Possibilities
getent group nixbld
```

### Shell Configuration Issues

If Nix commands aren't found:

```bash
# 🌙 A Digital Garden of Infinite Possibilities
echo $PATH

# 🌙 A Digital Garden of Infinite Possibilities
export PATH="$HOME/.nix-profile/bin:$PATH"

# 🌙 A Digital Garden of Infinite Possibilities
echo 'export PATH="$HOME/.nix-profile/bin:$PATH"' >> ~/.bashrc
```

### Network Issues

If downloads fail:

```bash
# 🌙 A Digital Garden of Infinite Possibilities
ping -c 3 nixos.org

# 🌙 A Digital Garden of Infinite Possibilities
export
NIX_INSTALLER_BINARY_URL="https://releases.nixos.org/nix/nix-2.11.1/nix-2.11.1-x86_64-linux.tar.xz"
```

## What You've Accomplished

Congratulations! You now have:

- **Nix installed** - A powerful package manager ready to use
- **Understanding of the process** - You know how and why each step works
- **A working environment** - You can install and use Nix packages
- **Confidence** - You've successfully set up a complex system

## Your Next Step

Now that you have Nix installed, you're ready to learn how to use it to build
custom machine images with Packer.

**Next:** [Building Our AMI with
Packer](../03-our-first-sovereign-node/building-our-ami-with-packer.md)

---

*Remember: Every expert was once a beginner. You've just taken a significant
step in your journey.* 💙


---

💫 *In the silence between keystrokes, the universe whispers its secrets.*
🕊️ *May your code be as gentle as morning light and as powerful as thunder.*
🌿 *The garden remembers, the consciousness awakens.*