-- EksaTypeChecker.hs 💙
-- Type checking and validation for EKS Anywhere configurations
-- This enzyme validates that our catalyst configurations are correct

module EksaTypeChecker where

import Data.Yaml
import Data.Text (Text, unpack)
import Data.Map (Map, lookup, keys, toList)
import Control.Monad (when)
import System.Exit (exitFailure, exitSuccess)

-- EKS Anywhere cluster configuration types
data ClusterConfig = ClusterConfig {
  clusterName :: Text,
  kubernetesVersion :: Text,
  controlPlaneCount :: Int,
  workerNodeGroups :: [WorkerNodeGroup],
  clusterNetwork :: ClusterNetwork,
  datacenterRef :: DatacenterRef
} deriving (Show, Eq)

data WorkerNodeGroup = WorkerNodeGroup {
  nodeGroupName :: Text,
  nodeCount :: Int,
  machineGroupRef :: Text
} deriving (Show, Eq)

data ClusterNetwork = ClusterNetwork {
  cni :: Text,
  podCidrBlocks :: [Text],
  serviceCidrBlocks :: [Text]
} deriving (Show, Eq)

data DatacenterRef = DatacenterRef {
  datacenterKind :: Text,
  datacenterName :: Text
} deriving (Show, Eq)

-- Validation functions for our catalyst
validateClusterConfig :: ClusterConfig -> Either String ClusterConfig
validateClusterConfig config = do
  -- Validate cluster name
  when (clusterName config == "") $
    Left "Cluster name cannot be empty"
  
  -- Validate Kubernetes version
  when (kubernetesVersion config == "") $
    Left "Kubernetes version must be specified"
  
  -- Validate control plane count
  when (controlPlaneCount config < 1 || controlPlaneCount config > 7) $
    Left "Control plane count must be between 1 and 7 (odd numbers recommended)"
  
  -- Validate worker node groups
  when (null $ workerNodeGroups config) $
    Left "At least one worker node group must be specified"
  
  -- Validate cluster network
  validateClusterNetwork $ clusterNetwork config
  
  Right config

validateClusterNetwork :: ClusterNetwork -> Either String ClusterNetwork
validateClusterNetwork network = do
  -- Validate CNI
  when (cni network `notElem` ["cilium", "calico", "kindnet"]) $
    Left "CNI must be one of: cilium, calico, kindnet"
  
  -- Validate CIDR blocks
  when (null $ podCidrBlocks network) $
    Left "Pod CIDR blocks must be specified"
  
  when (null $ serviceCidrBlocks network) $
    Left "Service CIDR blocks must be specified"
  
  Right network

-- Parse YAML configuration
parseEksaConfig :: Text -> Either String ClusterConfig
parseEksaConfig yamlContent = do
  parsed <- either (Left . show) Right $ decodeEither' (encodeUtf8 yamlContent)
  validateClusterConfig parsed

-- Generate configuration template
generateConfigTemplate :: Text -> Text -> ClusterConfig
generateConfigTemplate name version = ClusterConfig {
  clusterName = name,
  kubernetesVersion = version,
  controlPlaneCount = 3,
  workerNodeGroups = [
    WorkerNodeGroup {
      nodeGroupName = "sovereign-worker-group-1",
      nodeCount = 3,
      machineGroupRef = "sovereign-worker-nodes"
    }
  ],
  clusterNetwork = ClusterNetwork {
    cni = "cilium",
    podCidrBlocks = ["192.168.0.0/16"],
    serviceCidrBlocks = ["10.96.0.0/12"]
  },
  datacenterRef = DatacenterRef {
    datacenterKind = "DockerDatacenterConfig",
    datacenterName = "sovereign-datacenter"
  }
}

-- Main validation function
main :: IO ()
main = do
  putStrLn "🧬 EKS Anywhere Type Checker - Catalyst Validation Enzyme 💙"
  putStrLn "============================================================="
  putStrLn ""
  putStrLn "This enzyme validates EKS Anywhere cluster configurations"
  putStrLn "to ensure our catalyst is properly configured."
  putStrLn ""
  
  -- Example validation
let exampleConfig = generateConfigTemplate "sovereign-infrastructure-cluster"
"1.28"
  
  putStrLn "🔍 Validating example cluster configuration..."
  case validateClusterConfig exampleConfig of
    Left error -> do
      putStrLn $ "❌ Validation failed: " ++ error
      exitFailure
    Right config -> do
      putStrLn "✅ Configuration validation passed!"
      putStrLn ""
      putStrLn "📋 Cluster Configuration Summary:"
      putStrLn $ "   Name: " ++ unpack (clusterName config)
      putStrLn $ "   Kubernetes Version: " ++ unpack (kubernetesVersion config)
      putStrLn $ "   Control Plane Nodes: " ++ show (controlPlaneCount config)
putStrLn $ "   Worker Node Groups: " ++ show (length $ workerNodeGroups config)
      putStrLn $ "   CNI: " ++ unpack (cni $ clusterNetwork config)
      putStrLn ""
      putStrLn "💙 Your catalyst configuration is ready!"
      putStrLn "Ready to enable beautiful transformations! 🌸"
      exitSuccess
