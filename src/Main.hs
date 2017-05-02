{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
module Main where

import           Control.Applicative
import           Data.Aeson
import           Data.Aeson.Types
import           Data.Bits
import qualified Data.ByteString     as BS
import           Data.List
import           Data.String
import           Data.Yaml           as Y
import           System.Directory    (getCurrentDirectory,
                                      getTemporaryDirectory, removeFile)
import qualified System.Environment  as SE
import qualified System.Exit         as SX
import           System.FilePath
import qualified System.Process      as SP
import Data.Maybe

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
         cmd        :: Maybe String,
         entrypoint :: Maybe String,
         ports      :: Maybe [String],
         workingdir :: Maybe String,
         volumes    :: Maybe [String]
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
  name      :: String,
  runAsRoot :: Maybe String,
  contents  :: Maybe [String],
  config    :: ImageConfig
  } deriving (Show)

instance FromJSON ImageInformation where
  parseJSON (Object o) = do
    i <- o .: "image"
    ImageInformation <$>
     i .: "name"       <*>
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
             Just b  -> entryPointScript b
             Nothing -> ""

-- ImageInformation
useImageInformation :: IO ImageInformation
useImageInformation =
  either (error . show) id <$>
  Y.decodeFileEither "docker.yml"

-- ImageConfig
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
  putStrLn "Initial configuration-file(docker.yml) is created..."

main = do

  -- Indentation Functions
  let
    addIndentationToEach :: String -> String
    addIndentationToEach items = unlines $ map ("    " ++) (lines items)

    addIndentationToListItems :: [String] -> String
    addIndentationToListItems items = unwords $ map (\item -> "      " ++ item ++ " = {};\n") $ map show items

  currentDir <- getCurrentDirectory
  epPath <- useEntryPointPath
  let entryScript = takeEntryPointScript epPath
  entryScriptContent <- case epPath of { Nothing -> return "empty"; Just path -> readEntryScriptContent (currentDir </> (entryPointScript path)) }

  imageInfo <- useImageInformation

  let
       -- Extract ImageConfig Data out of ImageInformation Data
       iConfig = config imageInfo

       iName = name imageInfo -- This is necessary

       -- RunAsRoot part
       iRunAsRoot = "  runAsRoot = ''\n    #!${stdenv.shell}\n    export PATH=/bin:/usr/bin:/sbin:/usr/sbin:$PATH\n    ${dockerTools.shadowSetup}\n" ++ (maybe "empty" addIndentationToEach (runAsRoot imageInfo))++ "\n  '';"

       -- Contents
       iContents = "  contents = [ " ++  (maybe "empty" unwords (contents imageInfo)) ++ " ];"

       -- Adds Cmd of Config part
       configCmd = "    Cmd = [ "++ "\"" ++ (fromMaybe "empty" (cmd iConfig)) ++ "\"" ++ " ];"

       -- If entryPointScript is present, puts it with ..
       configEntryPoint
          | entryScript /= ""   = "    Entrypoint = [ " ++ (fromMaybe "empty" (entrypoint iConfig)) ++ " ];"
          | otherwise    = ""

       -- If entryPointScript is present, puts it with ..
       entryPoint
          | entryScript /= "" = "let\n  entrypoint = writeScript \"entrypoint.sh\" ''\n    #!${stdenv.shell}\n" ++ addIndentationToEach entryScriptContent ++ "  '';\nin"
          | otherwise    = ""

       -- Working Directory
       configWorkingDir = "    WorkingDir = " ++ "\"" ++ (fromMaybe "empty" (workingdir iConfig)) ++ "\"" ++ ";"

       -- Port
       configPorts = "    ExposedPorts = {\n" ++ (maybe "empty" addIndentationToListItems (ports iConfig)) ++ "    };"

       -- Volume
       configVolumes = "    Volumes = {\n" ++ (maybe "empty" addIndentationToListItems (volumes iConfig)) ++ "    };"

       -- Creates default.nix with above functions result
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
  let dockerLoadCommand = "docker load < " ++ currentDir </> "result"
  let initCommand  = initConfigFile (currentDir </> "docker.yml")
  let buildImage :: IO ()
      buildImage = do
          putStrLn "\nNIX-BUILD RELATED EVENTS WILL BE SHOWN...\n"
          buildExitCode <- SP.system $ buildCommand
          if buildExitCode == SX.ExitSuccess
            then do
                 dockerLoadExitCode <- SP.system dockerLoadCommand
                 if dockerLoadExitCode == SX.ExitSuccess
                   then do
                        removeFile (currentDir </> "result")
                        putStrLn ("\nYOUR IMAGE " ++ iName ++ " IS CREATED...")
                   else putStrLn "\nYOUR IMAGE BUILD FAILED...\n\nPLEASE BE SURE, You used with sudo... otherwise you can add your user to docker group or use sudo..."
          else putStrLn "\nYOUR IMAGE BUILD FAILED...\n"

  args <- SE.getArgs
  case args of
        ["help"]  -> putStrLn help
        ["build"] -> buildImage
        ["init"]  -> initCommand
        ["print"] -> putStrLn defaultNix
        _         -> SX.die "Unknown argument"

-- Help menu items
help :: String
help = unlines [
          "Usage: sdit [OPTION]",
          "Create DOCKER IMAGE(s) from configuration-file.\n",
          "\thelp              Displays this help menu.",
          "\tbuild             Builds the docker image.",
          "\tinit              Creates initial configuration-file(docker.yml)",
          "\tprint             Displays created Nix from configuration-file(docker.yml)\n",
          "Homepage and help: https://github.com/dyrnade/smallDockerImagesTool"
      ]
