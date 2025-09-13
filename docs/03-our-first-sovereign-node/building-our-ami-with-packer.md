# Building Our AMI with Packer: Creating Custom Machine Images ��

## A Gentle Introduction to Packer

Hello, beautiful soul. Today we will use Packer to build our first custom machine image—a beautiful Alpine Linux system with Nix Package Manager installed and ready to use.

Packer is a tool that helps us create machine images (AMIs) in a repeatable, automated way. Instead of manually setting up servers, we describe what we want in code, and Packer builds it for us.

## What We're Going to Build

### Our Custom AMI

We will create an Amazon Machine Image (AMI) that contains:

- **Alpine Linux** - Our minimal, secure base operating system
- **Nix Package Manager** - Installed and configured for declarative package management
- **EKS Worker Dependencies** - Everything needed to join a Kubernetes cluster
- **Security Hardening** - SSH keys, firewall rules, and secure defaults
- **Custom Configuration** - Tailored for our specific needs

### Why We Build Custom AMIs

Instead of using standard Amazon Linux or Ubuntu images, we build our own because:

- **Consistency** - Every server starts with exactly the same configuration
- **Security** - We control exactly what's installed and how it's configured
- **Performance** - No unnecessary packages or services running
- **Reproducibility** - We can rebuild the exact same image anytime
- **Customization** - We can include exactly what we need for our applications

## Understanding Packer

### What is Packer?

Packer is a tool for creating machine images from a single source configuration. It's like having a perfect recipe that you can follow to create identical servers every time.

### How Packer Works

Packer follows a simple process:

1. **Configure** - You describe what you want in a Packer template
2. **Build** - Packer creates a temporary server and configures it
3. **Package** - Packer captures the configured server as a machine image
4. **Cleanup** - Packer destroys the temporary server
5. **Deliver** - You have a new machine image ready to use

### The Packer Template

A Packer template is a JSON or HCL file that describes:
- **Builders** - What type of machine image to create (AMI, Docker, etc.)
- **Provisioners** - How to configure the machine (shell scripts, configuration files)
- **Post-processors** - What to do with the finished image

## Our Packer Template

### The Complete Template

Here's our Packer template for building an Alpine+Nix AMI:

```hcl
# alpine-nix-ami.pkr.hcl
variable "aws_region" {
  type    = string
  default = "us-east-1"
}

variable "ssh_key_name" {
  type    = string
  default = "alpine-nix-key"
}

source "amazon-ebs" "alpine-nix" {
  region          = var.aws_region
  source_ami      = "ami-0a5c5f1b567a5a355" # Alpine Linux AMI
  instance_type   = "t3.micro"
  ssh_username    = "alpine"
  ami_name        = "alpine-nix-eks-worker-{{timestamp}}"
  
  tags = {
    Name        = "Alpine+Nix EKS Worker"
    Environment = "Development"
    Purpose     = "Kubernetes Worker Node"
    BuiltBy     = "Packer"
  }
}

build {
  sources = ["source.amazon-ebs.alpine-nix"]

  # Update the system
  provisioner "shell" {
    inline = [
      "sudo apk update",
      "sudo apk upgrade"
    ]
  }

  # Install required dependencies
  provisioner "shell" {
    inline = [
      "sudo apk add curl xz bash",
      "sudo apk add docker",
      "sudo apk add iptables"
    ]
  }

  # Create Nix user and group
  provisioner "shell" {
    inline = [
      "sudo addgroup -S nixbld",
      "sudo adduser -S nix -G nixbld",
      "sudo mkdir -m 0755 /nix",
      "sudo chown nix /nix"
    ]
  }

  # Install Nix Package Manager
  provisioner "shell" {
    inline = [
      "sudo -u nix curl -L https://nixos.org/nix/install | sh",
      "sudo -u nix . /home/nix/.nix-profile/etc/profile.d/nix.sh"
    ]
  }

  # Install EKS worker dependencies via Nix
  provisioner "shell" {
    inline = [
      "sudo -u nix nix-env -iA nixpkgs.aws-cni",
      "sudo -u nix nix-env -iA nixpkgs.kubelet",
      "sudo -u nix nix-env -iA nixpkgs.kubectl"
    ]
  }

  # Configure Docker
  provisioner "shell" {
    inline = [
      "sudo rc-update add docker default",
      "sudo service docker start"
    ]
  }

  # Configure iptables for Kubernetes
  provisioner "shell" {
    inline = [
      "sudo iptables -t nat -A POSTROUTING -o eth0 -j MASQUERADE",
      "sudo iptables -A FORWARD -i eth0 -o eth0 -j ACCEPT"
    ]
  }

  # Set up hostname
  provisioner "shell" {
    inline = [
      "sudo setup-hostname alpine-nix-worker"
    ]
  }

  # Clean up
  provisioner "shell" {
    inline = [
      "sudo apk cache clean",
      "sudo rm -rf /tmp/*",
      "sudo rm -rf /var/cache/apk/*"
    ]
  }
}
```

### Understanding Each Section

Let's break down what each part does:

#### Variables
```hcl
variable "aws_region" {
  type    = string
  default = "us-east-1"
}
```
This defines a variable we can change without modifying the template.

#### Source
```hcl
source "amazon-ebs" "alpine-nix" {
  region          = var.aws_region
  source_ami      = "ami-0a5c5f1b567a5a355"
  instance_type   = "t3.micro"
  ssh_username    = "alpine"
  ami_name        = "alpine-nix-eks-worker-{{timestamp}}"
}
```
This tells Packer to create an Amazon EBS-backed AMI using Alpine Linux as the base.

#### Build Section
The build section contains all the provisioners that configure our machine:

1. **System Updates** - Keep Alpine Linux current
2. **Dependencies** - Install required packages
3. **Nix Setup** - Create users and install Nix
4. **EKS Tools** - Install Kubernetes worker tools
5. **Configuration** - Set up Docker and networking
6. **Cleanup** - Remove temporary files

## Building the AMI

### Prerequisites

Before we can build the AMI, we need:

1. **AWS CLI configured** - With appropriate permissions
2. **SSH key pair** - For connecting to the build instance
3. **Packer installed** - The tool itself
4. **Alpine Linux AMI ID** - A base image to start from

### Finding the Right Alpine AMI

We need to find a recent Alpine Linux AMI:

```bash
# Search for Alpine Linux AMIs
aws ec2 describe-images \
  --owners 099720109477 \
  --filters "Name=name,Values=alpine-*" \
  --query 'Images[*].[ImageId,Name,CreationDate]' \
  --output table
```

The owner ID `099720109477` is Canonical's AWS account ID, which maintains official Alpine Linux AMIs.

### Building the AMI

Once we have the template and prerequisites:

```bash
# Initialize Packer
packer init alpine-nix-ami.pkr.hcl

# Validate the template
packer validate alpine-nix-ami.pkr.hcl

# Build the AMI
packer build alpine-nix-ami.pkr.hcl
```

### What Happens During the Build

Packer will:

1. **Launch a temporary EC2 instance** using the base Alpine AMI
2. **Connect via SSH** using your key pair
3. **Run each provisioner** in sequence to configure the system
4. **Create an AMI** from the configured instance
5. **Terminate the temporary instance**
6. **Return the AMI ID** for you to use

## Using Your Custom AMI

### In Terraform

Once you have the AMI ID, you can use it in your Terraform configuration:

```hcl
data "aws_ami" "alpine_nix" {
  most_recent = true
  owners      = ["self"]

  filter {
    name   = "name"
    values = ["alpine-nix-eks-worker-*"]
  }
}

resource "aws_instance" "worker" {
  ami           = data.aws_ami.alpine_nix.id
  instance_type = "t3.medium"
  
  # Your other configuration...
}
```

### In EKS Node Groups

For EKS worker nodes:

```hcl
resource "aws_eks_node_group" "alpine_nix" {
  cluster_name    = aws_eks_cluster.main.name
  node_group_name = "alpine-nix"
  node_role_arn   = aws_iam_role.node.arn
  subnet_ids      = aws_subnet.private[*].id

  ami_type = "CUSTOM"
  image_id = data.aws_ami.alpine_nix.id

  scaling_config {
    desired_size = 2
    max_size     = 4
    min_size     = 1
  }
}
```

## Best Practices

### Template Organization

- **Use variables** for values that might change
- **Add comments** to explain complex sections
- **Test incrementally** - Build and test each provisioner
- **Version control** - Keep your templates in Git

### Security Considerations

- **Use specific AMI IDs** - Don't rely on "latest" tags
- **Minimize installed packages** - Only install what you need
- **Regular updates** - Rebuild AMIs regularly with security updates
- **Access control** - Limit who can build and use your AMIs

### Performance Optimization

- **Use appropriate instance types** - t3.micro for testing, larger for production
- **Parallel builds** - Build multiple AMIs simultaneously if needed
- **Caching** - Use local caches for dependencies when possible
- **Cleanup** - Remove unnecessary files to reduce AMI size

## Troubleshooting

### Common Issues

**Build fails with SSH connection errors:**
- Check your security group allows SSH access
- Verify your SSH key is correctly configured
- Ensure the base AMI is accessible

**Provisioner scripts fail:**
- Test scripts manually on a running instance
- Check for typos and syntax errors
- Verify all required packages are available

**AMI creation fails:**
- Check AWS permissions for AMI creation
- Verify you have sufficient storage quota
- Ensure the build instance can access required services

### Getting Help

- **Packer documentation** - Comprehensive guides and examples
- **Community forums** - Ask questions and share solutions
- **GitHub issues** - Report bugs and request features
- **AWS support** - For AWS-specific problems

## What You've Accomplished

Congratulations! You now have:

- **A custom AMI** - Built exactly to your specifications
- **Understanding of Packer** - How to automate machine image creation
- **Reproducible infrastructure** - You can rebuild the same image anytime
- **Foundation for EKS** - A worker node image ready for Kubernetes

## Your Next Step

Now that you have a custom AMI, you're ready to learn how to connect to your infrastructure securely and start building your EKS cluster.

**Next:** [Connecting with Love and SSH](./connecting-with-love-and-ssh.md)

---

*Remember: Every custom AMI is a small work of art—a carefully crafted system built with intention and care.* 💙
