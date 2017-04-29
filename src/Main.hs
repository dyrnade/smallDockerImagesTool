{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.Bits
import           Data.List
import           System.FilePath
import           System.Directory (getCurrentDirectory, getTemporaryDirectory)
import           Data.String
import qualified System.Environment as SE
import qualified System.Exit        as SX
import qualified System.Process     as SP
import Data.Yaml as Y
import qualified Data.ByteString as BS
import Data.Aeson
import Data.Aeson.Types
import Control.Applicative

-- Data EntryPoint
data EntryPointPath = EntryPointPath {
 entryPointScript :: String
  } deriving (Show)
-- deriveJSON defaultOptions ''EntryPointPath

instance FromJSON EntryPointPath where
         parseJSON (Object e) = EntryPointPath
          <$> e .: "entryPointScript"
         parseJSON invalid    = typeMismatch "EntryPointPath" invalid
         
-- Data Config
data ImageConfig = ImageConfig {
         cmd :: Maybe String,
         entrypoint :: Maybe String,
         ports :: Maybe [String],
         workingdir :: Maybe String,
         volumes :: Maybe [String]
  } deriving (Show)

instance FromJSON ImageConfig where
  parseJSON (Object i) = 
    ImageConfig <$>
     i .:? "cmd" <*>
     i .:? "entrypoint" <*>
     i .:? "ports" <*>
     i .:? "workingdir" <*>
     i .:? "volumes" 
  parseJSON invalid    = typeMismatch "Config" invalid
  
-- Data ImageInfo
data ImageInformation = ImageInformation {
  name :: String,
  runAsRoot :: Maybe String,
  contents :: Maybe [String],
  config :: ImageConfig
  } deriving (Show)

instance FromJSON ImageInformation where
  parseJSON (Object o) = do
    i <- o .: "image"
    ImageInformation <$>
     i .: "name" <*>
     i .:? "runAsRoot" <*>
     i .:? "contents" <*>
     i .: "config"
  parseJSON invalid    = typeMismatch "ImageInformation" invalid


-- Data Extraction starts here ^^
-- EntryPoint
useEntryPointPath :: IO (Maybe EntryPointPath)
useEntryPointPath = do
  results <- Y.decodeFile "docker.yml" :: IO (Maybe EntryPointPath)
  return (results)

takeEntryPointScript :: Maybe EntryPointPath -> String
takeEntryPointScript a = case a of
             Just b -> entryPointScript b
             Nothing -> ""

-- ImageInformation
useImageInformation :: IO ImageInformation
useImageInformation =
  either (error . show) id <$>
  Y.decodeFileEither "docker.yml" 

useImageConfig :: IO (ImageConfig)
useImageConfig = do
  image <- useImageInformation
  let imageConfig = config image
  return (imageConfig)

readEntryScriptContent :: FilePath -> IO String
readEntryScriptContent = readFile

initConfigFile :: FilePath -> IO ()
initConfigFile filePath = do
  let content = unlines [
       "---",
       "#Extra Documentation",
       "#NixOS Manual about Docker: https://nixos.org/nixpkgs/manual/#sec-pkgs-dockerTools",
       "#Docker Image Specification: https://github.com/docker/docker/blob/master/image/spec/v1.md#docker-image-specification-v100",
       "# YAML for Docker Image",
       "",
       "entryPointScript: entrypoint.sh # Entrypoint script you want when container initialized",
       "image:",
       "  name: #image name # Only field that is absolute REQUIRED.",
       "  runAsRoot: | # commands to run as root user ",
       "    #Item1",
       "    #Item2",
       "    #...",
       "",
       "  contents: [\"list of items\"]#",
       "  config:",
       "    cmd: #command to run",
       "    entrypoint: entrypoint # if you define entryPointScript, this is required",
       "    ports: [\"list of items\"] # ports to be used ",
       "    workingdir: # Working Directory",
       "    volumes: [\"list of items\"] # Volumes to mount in",
       "..."
       ]
  writeFile filePath content
  putStrLn "Initial configuration-file(docker.yml) is created."

main = do
  
  currentDir <- getCurrentDirectory
  epPath <- useEntryPointPath
  let entryScript = takeEntryPointScript epPath
  entryScriptContent <- case epPath of { Nothing -> return "empty"; Just path -> readEntryScriptContent (currentDir </> (entryPointScript path)) }
  
  -- | TODO: if entryScriptContent empty then skip this part
  let entryPoint = "let\n  entrypoint = writeScript \"entrypoint.sh\" ''\n    #!${stdenv.shell}\n" ++ (unlines $ map (\e -> "    " ++ e) (lines entryScriptContent)) ++ "  '';\nin"
  --let fullPathEntryScript = currentDir </> entryScript
  --entryScriptContent <- readEntryScriptContent fullPathEntryScript


  imageInfo <- useImageInformation

  let
       iConfig = config imageInfo
       iName = name imageInfo

       iRunAsRoot = case (runAsRoot imageInfo) of
         Just rAr -> "  runAsRoot = ''\n    #!${stdenv.shell}\n    ${dockerTools.shadowSetup}\n" ++ (unlines $ map (\r -> "    " ++ r) (lines rAr)) ++ "\n  '';"
         Nothing  -> "empty"
         
       iContents = case (contents imageInfo) of
         Just c  -> "  contents = [ "++ (unwords c) ++ " ];"
         Nothing -> "empty"

       configCmd = case (cmd iConfig) of
         Just cm -> "    Cmd = [ "++ "\"" ++ cm ++ "\"" ++ " ];"
         Nothing -> "empty"

  
       configEntryPoint
          | entryScript /= ""   = case (entrypoint iConfig) of
                                   Just ep -> "    Entrypoint = [ " ++ ep ++" ];"
                                   Nothing -> "empty"
          | otherwise = ""

       -- configWorkingDir = workingdir iConfig
       configWorkingDir = case (workingdir iConfig) of
         Just wDir -> "    WorkingDir = " ++ "\"" ++ wDir ++ "\"" ++ ";"     
         Nothing   -> "empty"       

       -- If ports are present, add some literals for each     
       maybePorts :: Maybe [String] -> String
       maybePorts (Just ports) =  "    ExposedPorts = {\n" ++ (unwords $ map (\port -> "      " ++ port ++ " = {};\n") $ map show ports) ++ "    };"
       maybePorts Nothing = "empty"
       configPorts = maybePorts $ ports iConfig

       maybeVolumes :: Maybe [String] -> String
       maybeVolumes (Just volumes) =  "    Volumes = {\n" ++ (unwords $ map (\volume ->"      " ++  volume ++ " = {};\n") $ map show volumes) ++ "    };"
       maybeVolumes Nothing = "empty"
       configVolumes = maybeVolumes $ volumes iConfig
       

       defaultNix = unlines $ filter (\x -> x /= "empty") [
             "{ pkgs ? import <nixpkgs> {} }:",
             "with pkgs;",
             entryPoint,
             "dockerTools.buildImage {",
             "  name = " ++ "\"" ++ iName ++ "\"" ++ ";",
             iRunAsRoot,
             "",
             iContents,
             "",
             "  config = {",

             configCmd,
             configEntryPoint,
             configPorts,
             configWorkingDir,            
             configVolumes,
             
             "  };",
             "}"
            ]

  tmpDir <- getTemporaryDirectory
  let tmpDefaultNix = tmpDir </> "default.nix"
  writeFile tmpDefaultNix defaultNix

  let buildCommand = "nix-build " ++ tmpDefaultNix 
  let initCommand  = initConfigFile (currentDir </> "docker.yml")
  
  args <- SE.getArgs    
  case args of
        ["help"]  -> putStrLn help
        --["build",Just extra] -> SP.system (buildCommand ++ " " ++  fromMaybe extra)  >>= SX.exitWith
        ["build"] -> SP.system buildCommand  >>= SX.exitWith
        ["init"]  -> initCommand
        ["print"] -> putStrLn defaultNix
        _         -> SX.die "Unknown argument"

-- Help menu items
help :: String
help = unlines [
          "Usage: sdit [OPTION]",
          "Create DOCKER IMAGE(s) from configuration-file.",
          "",
          "\thelp     Displays this help menu.",
          "\tbuild    Builds the docker image.",
          "\tinit     Creates initial configuration-file(docker.yml)",
          "\tprint    Displays created Nix from configuration-file(docker.yml)",
          "",
          "Homepage and help: https://github.com/dyrnade/smallDockerImagesTool"
      ]
