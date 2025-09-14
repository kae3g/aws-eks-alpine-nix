-- CursorParser.hs 💙
-- Parser for Cursor execution instructions
-- This enzyme transforms natural language into executable commands

module CursorParser where

import Data.Text (Text, unpack, pack)
import Data.List (intercalate, isPrefixOf)
import Control.Monad (when)
import System.Process (callCommand)

-- Types for parsing Cursor instructions
data CursorInstruction = CursorInstruction {
  instructionType :: InstructionType,
  parameters :: [Text],
  description :: Text
} deriving (Show, Eq)

data InstructionType = 
    CreateDirectory Text
  | CreateFile Text Text
  | RunCommand Text
  | InstallPackage Text
  | ConfigureService Text
  | DeployApplication Text
  deriving (Show, Eq)

-- Parse natural language instructions into executable commands
parseCursorInstructions :: Text -> [CursorInstruction]
parseCursorInstructions input = 
  let lines = map pack $ lines $ unpack input
  in map parseInstructionLine $ filter (not . null) lines

parseInstructionLine :: Text -> CursorInstruction
parseInstructionLine line
  | "mkdir" `isPrefixOf` unpack line = 
    CursorInstruction (CreateDirectory $ extractPath line) [] "Create directory"
  | "touch" `isPrefixOf` unpack line = 
    CursorInstruction (CreateFile (extractPath line) "") [] "Create file"
  | "cat >" `isPrefixOf` unpack line = 
CursorInstruction (CreateFile (extractPath line) (extractContent line)) []
"Create file with content"
  | "chmod" `isPrefixOf` unpack line = 
    CursorInstruction (RunCommand line) [] "Change file permissions"
  | "cd" `isPrefixOf` unpack line = 
    CursorInstruction (RunCommand line) [] "Change directory"
  | otherwise = 
    CursorInstruction (RunCommand line) [] "Execute command"

extractPath :: Text -> Text
extractPath line = 
  let words = map pack $ words $ unpack line
  in if length words > 1 then words !! 1 else ""

extractContent :: Text -> Text
extractContent line = 
  let content = unpack line
  in pack $ dropWhile (/= '>') content

-- Execute parsed instructions
executeInstructions :: [CursorInstruction] -> IO ()
executeInstructions instructions = do
  putStrLn "🧬 Cursor Parser - Instruction Execution Enzyme 💙"
  putStrLn "=================================================="
  putStrLn ""
  putStrLn "This enzyme transforms natural language into executable commands"
  putStrLn "and executes them with gentle precision."
  putStrLn ""
  
  sequence_ $ map executeInstruction instructions
  
  putStrLn ""
  putStrLn "💙 All instructions executed successfully!"
  putStrLn "Your commands have been transformed into action! 🌸"

executeInstruction :: CursorInstruction -> IO ()
executeInstruction (CursorInstruction instructionType _ description) = do
  putStrLn $ "🔄 " ++ unpack description ++ "..."
  
  case instructionType of
    CreateDirectory path -> do
      putStrLn $ "📁 Creating directory: " ++ unpack path
      callCommand $ "mkdir -p " ++ unpack path
      
    CreateFile path content -> do
      putStrLn $ "📄 Creating file: " ++ unpack path
      writeFile (unpack path) (unpack content)
      
    RunCommand cmd -> do
      putStrLn $ "⚡ Executing: " ++ unpack cmd
      callCommand $ unpack cmd
      
    InstallPackage pkg -> do
      putStrLn $ "📦 Installing package: " ++ unpack pkg
      callCommand $ "sudo apt-get install -y " ++ unpack pkg
      
    ConfigureService service -> do
      putStrLn $ "⚙️  Configuring service: " ++ unpack service
      callCommand $ "sudo systemctl enable " ++ unpack service
      
    DeployApplication app -> do
      putStrLn $ "🚀 Deploying application: " ++ unpack app
      callCommand $ "kubectl apply -f " ++ unpack app

-- Example usage with the EKS Anywhere setup instructions
exampleEksaInstructions :: Text
exampleEksaInstructions = pack $ intercalate "\n" [
  "mkdir -p eks-anywhere",
  "mkdir -p docs/07-enzyme-catalyst-theory", 
  "mkdir -p haskell",
  "touch eks-anywhere/eksa-cluster.yaml",
  "touch eks-anywhere/custom-components.yaml",
  "touch eks-anywhere/nixos-node-template.yaml",
  "touch eks-anywhere/install-eksa.sh",
  "chmod +x eks-anywhere/install-eksa.sh",
  "touch docs/07-enzyme-catalyst-theory/README.md",
  "touch docs/07-enzyme-catalyst-theory/eksa-as-catalyst.md",
  "touch docs/07-enzyme-catalyst-theory/applications-as-enzymes.md",
  "touch docs/07-enzyme-catalyst-theory/workflow-examples.md",
  "touch haskell/EksaTypeChecker.hs",
  "touch haskell/CursorParser.hs",
  "touch haskell/compile-type-checker.sh"
  ]

-- Main function for testing
main :: IO ()
main = do
  putStrLn "🧬 Cursor Parser - Natural Language to Code Enzyme 💙"
  putStrLn "====================================================="
  putStrLn ""
  putStrLn "This enzyme demonstrates how natural language instructions"
  putStrLn "can be transformed into executable commands."
  putStrLn ""
  
  let instructions = parseCursorInstructions exampleEksaInstructions
  
  putStrLn "📋 Parsed Instructions:"
  mapM_ (putStrLn . show) instructions
  
  putStrLn ""
  putStrLn "💙 Ready to execute EKS Anywhere setup instructions!"
  putStrLn "This enzyme can transform any natural language into action! 🌸"
