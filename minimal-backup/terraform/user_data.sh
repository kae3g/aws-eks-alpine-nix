#!/bin/bash
# User data script for NixOS EKS worker nodes
# This script configures the node to join the EKS cluster

set -euo pipefail

# Variables passed from Terraform
CLUSTER_NAME="${cluster_name}"
CLUSTER_ENDPOINT="${cluster_endpoint}"
CLUSTER_CA="${cluster_ca}"

# Log function
log() {
    echo "[$(date +'%Y-%m-%d %H:%M:%S')] $1" | tee -a /var/log/eks-bootstrap.log
}

log "Starting EKS node bootstrap process..."

# Create kubelet configuration directory
mkdir -p /etc/kubernetes/kubelet

# Create kubeconfig for kubelet
cat > /etc/kubernetes/kubelet/kubeconfig << EOF
apiVersion: v1
kind: Config
clusters:
- cluster:
    certificate-authority-data: ${CLUSTER_CA}
    server: ${CLUSTER_ENDPOINT}
  name: kubernetes
contexts:
- context:
    cluster: kubernetes
    user: kubelet
  name: kubelet
current-context: kubelet
users:
- name: kubelet
  user:
    exec:
      apiVersion: client.authentication.k8s.io/v1beta1
      command: /usr/bin/aws
      args:
        - eks
        - get-token
        - --cluster-name
        - ${CLUSTER_NAME}
        - --region
        - ${AWS_REGION:-us-west-2}
EOF

# Set proper permissions
chmod 600 /etc/kubernetes/kubelet/kubeconfig

# Create kubelet configuration
cat > /etc/kubernetes/kubelet/kubelet-config.yaml << EOF
apiVersion: kubelet.config.k8s.io/v1beta1
kind: KubeletConfiguration
authentication:
  anonymous:
    enabled: false
  webhook:
    enabled: true
  x509:
    clientCAFile: /etc/kubernetes/pki/ca.crt
authorization:
  mode: Webhook
clusterDomain: cluster.local
clusterDNS:
  - 10.100.0.10
cgroupDriver: systemd
containerRuntimeEndpoint: unix:///run/containerd/containerd.sock
evictionHard:
  imagefs.available: 15%
  memory.available: 100Mi
  nodefs.available: 10%
  nodefs.inodesFree: 5%
evictionSoft:
  imagefs.available: 15%
  memory.available: 100Mi
  nodefs.available: 10%
  nodefs.inodesFree: 5%
evictionSoftGracePeriod:
  imagefs.available: 2m
  memory.available: 2m
  nodefs.available: 2m
  nodefs.inodesFree: 2m
evictionMaxPodGracePeriod: 30
featureGates:
  RotateKubeletServerCertificate: true
healthzBindAddress: 127.0.0.1
healthzPort: 10248
httpCheckFrequency: 20s
imageMinimumGCAge: 2m
imageGCHighThresholdPercent: 85
imageGCLowThresholdPercent: 80
iptablesDropBit: 15
iptablesMasqueradeBit: 15
kubeAPIBurst: 10
kubeAPIQPS: 5
makeIPTablesUtilChains: true
maxOpenFiles: 1000000
maxPods: 110
nodeStatusUpdateFrequency: 10s
oomScoreAdj: -999
podCIDR: 10.100.0.0/16
resolvConf: /run/systemd/resolve/resolv.conf
rotateCertificates: true
runtimeRequestTimeout: 2m
serializeImagePulls: true
serverTLSBootstrap: true
streamingConnectionIdleTimeout: 4h
syncFrequency: 1m
volumeStatsAggPeriod: 1m
EOF

# Create systemd service for kubelet
cat > /etc/systemd/system/kubelet.service << EOF
[Unit]
Description=Kubernetes Kubelet
Documentation=https://github.com/kubernetes/kubernetes
After=containerd.service
Requires=containerd.service

[Service]
ExecStart=/usr/bin/kubelet \\
  --config=/etc/kubernetes/kubelet/kubelet-config.yaml \\
  --kubeconfig=/etc/kubernetes/kubelet/kubeconfig \\
  --container-runtime=remote \\
  --container-runtime-endpoint=unix:///run/containerd/containerd.sock \\
  --cgroup-driver=systemd \\
  --pod-manifest-path=/etc/kubernetes/manifests \\
  --resolv-conf=/run/systemd/resolve/resolv.conf \\
  --node-ip=\$(curl -s http://169.254.169.254/latest/meta-data/local-ipv4) \\
  --register-with-taints=node.kubernetes.io/os=nixos:NoSchedule \\
  --node-labels=node.kubernetes.io/os=nixos,node.kubernetes.io/arch=amd64

Restart=always
RestartSec=5
StartLimitInterval=0

[Install]
WantedBy=multi-user.target
EOF

# Enable and start kubelet
systemctl daemon-reload
systemctl enable kubelet
systemctl start kubelet

log "Kubelet service started successfully"

# Wait for node to be ready
log "Waiting for node to join cluster..."
timeout=300
elapsed=0
while [ $elapsed -lt $timeout ]; do
    if systemctl is-active --quiet kubelet; then
        log "Node successfully joined the cluster"
        break
    fi
    sleep 5
    elapsed=$((elapsed + 5))
done

if [ $elapsed -ge $timeout ]; then
    log "ERROR: Node failed to join cluster within $timeout seconds"
    exit 1
fi

log "EKS node bootstrap completed successfully"
