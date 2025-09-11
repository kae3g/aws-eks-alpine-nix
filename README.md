# NixOS Minimal Development Environment

A super simple NixOS setup focused on getting the basics right: one host with Zsh + Haskell, and one container with the same environment.

## 🎯 Goal

Get a minimal NixOS development environment running with:
- ✅ NixOS host with Zsh shell and Haskell
- ✅ One container with the same NixOS environment
- ✅ Basic container orchestration for testing

## 🚀 Quick Start

### Option 1: Full NixOS Setup
```bash
# Run the setup script
./scripts/setup-minimal.sh
```

### Option 2: Docker-Only Setup
```bash
# Build and run containers
cd docker/
docker-compose -f docker-compose.minimal.yml up -d

# Enter the development container
docker exec -it nixos-minimal-dev zsh
```

## 📁 What's Included

### NixOS Configuration (`nixos/minimal-config.nix`)
- Zsh as default shell with syntax highlighting and autosuggestions
- Haskell toolchain (GHC, Cabal, Stack, HLS)
- Docker for containerization
- Basic development tools (git, curl, vim, etc.)

### Container Setup (`docker/`)
- `Dockerfile.minimal` - NixOS-based container with Haskell
- `docker-compose.minimal.yml` - Two-container setup for testing

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

### NixOS Issues
```bash
# Check configuration
sudo nixos-rebuild build

# Rollback if needed
sudo nixos-rebuild switch --rollback
```

### Docker Issues
```bash
# Check Docker status
sudo systemctl status docker

# Restart Docker
sudo systemctl restart docker
```

### Container Issues
```bash
# Check container logs
docker logs nixos-minimal-dev

# Rebuild container
docker-compose -f docker/docker-compose.minimal.yml build --no-cache
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

## 💡 Philosophy

This minimal setup focuses on getting the fundamentals right before adding complexity. Once we have a solid foundation, we can build up to the full EKS setup with confidence.

**Less is more, but make it work perfectly!** ✨
