{-# LANGUAGE OverloadedStrings #-}

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
import           Data.Maybe
import           Control.Exception

-- Data EntryPoint
data EntryPointPath = EntryPointPath {
 entryPointScript :: Maybe String
  } deriving (Show)
-- deriveJSON defaultOptions ''EntryPointPath

instance FromJSON EntryPointPath where
         parseJSON (Object e) = EntryPointPath
          <$> e .:? "entryPointScript"
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
  config    :: Maybe ImageConfig
  } deriving (Show)

instance FromJSON ImageInformation where
  parseJSON (Object o) = do
    i <- o .: "image"
    ImageInformation <$>
     i .: "name"       <*>
     i .:? "runAsRoot" <*>
     i .:? "contents" <*>
     i .:? "config"
  parseJSON invalid    = typeMismatch "ImageInformation" invalid


-- Data Extraction starts here ^^
-- EntryPoint
useEntryPointPath :: IO EntryPointPath
useEntryPointPath = do
  result <- Y.decodeFileEither "docker.yml"
  case result of
       Left err -> fail $ Y.prettyPrintParseException err
       Right entrypoint -> return $ entrypoint

-- takeEntryPointScript :: IO EntryPointPath -> String
-- takeEntryPointScript a = case a of
--              Just b  -> entryPointScript b
--              Nothing -> ""

-- ImageInformation
useImageInformation :: IO ImageInformation
useImageInformation =
  do result <- Y.decodeFileEither "docker.yml"
     case result of
       Left err -> fail $ Y.prettyPrintParseException err
       Right imageinformation  -> return $ imageinformation

-- ImageConfig
useImageConfig :: IO (Maybe ImageConfig)
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

buildImage :: IO ()
buildImage = do

  -- Indentation Functions
  let
    addIndentationToEach :: String -> String
    addIndentationToEach items = unlines $ map ("    " ++) (lines items)

    addIndentationToListItems :: [String] -> String
    addIndentationToListItems items = unwords $ map (\item -> "      " ++ item ++ " = {};\n") $ map show items

  currentDir <- getCurrentDirectory
  epPath <- useEntryPointPath
  let entryScript = entryPointScript epPath
  entryScriptContent <- case entryScript of { Nothing -> return "empty"; Just path -> readEntryScriptContent (currentDir </> path) }

  imageInfo <- useImageInformation

  let
       -- Extract ImageConfig Data out of ImageInformation Data
       tmpConfig = config imageInfo
       iConfig = case tmpConfig of
                 Just x -> x
                 Nothing -> ImageConfig {cmd=Nothing,entrypoint=Nothing,ports=Nothing,workingdir=Nothing,volumes=Nothing}

       iName = name imageInfo -- This is necessary

       -- RunAsRoot part
       iRunAsRoot = case (maybe "empty" addIndentationToEach (runAsRoot imageInfo)) of { "empty" -> ""; rAr -> "  runAsRoot = ''\n    #!${stdenv.shell}\n    export PATH=/bin:/usr/bin:/sbin:/usr/sbin:$PATH\n    ${dockerTools.shadowSetup}\n" ++ rAr ++ "\n  '';" }

       -- Contents
       iContents = case (maybe "empty" unwords (contents imageInfo)) of { "empty" -> ""; cnt ->"  contents = [ " ++ cnt  ++ " ];" }

       -- Adds Cmd of Config part
       configCmd = case (fromMaybe "empty" (cmd iConfig)) of { "empty" -> ""; cMd -> "    Cmd = [ "++ "\"" ++ cMd ++ "\"" ++ " ];" }

       -- If entryPointScript is present, puts it with ..
       configEntryPoint
          | entryScript /= Nothing   = "    Entrypoint = [ " ++ (fromMaybe "empty" (entrypoint iConfig)) ++ " ];"
          | otherwise    = ""

       -- If entryPointScript is present, puts it with ..
       entryPoint
          | entryScript /= Nothing = "let\n  entrypoint = writeScript \"entrypoint.sh\" ''\n    #!${stdenv.shell}\n" ++ addIndentationToEach entryScriptContent ++ "  '';\nin"
          | otherwise    = ""

       -- Working Directory
       configWorkingDir = case (fromMaybe "empty" (workingdir iConfig)) of { "empty" -> ""; wDir -> "    WorkingDir = " ++ "\"" ++ wDir ++ "\"" ++ ";" }

       -- Port
       configPorts = case (maybe "empty" addIndentationToListItems (ports iConfig)) of { "empty" -> ""; cP -> "    ExposedPorts = {\n" ++ cP ++ "    };" }

       -- Volume
       configVolumes = case (maybe "empty" addIndentationToListItems (volumes iConfig)) of { "empty" -> ""; cV -> "    Volumes = {\n" ++ cV ++ "    };" }

       -- Creates default.nix with above functions result
       defaultNix = unlines [
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
  removeFile tmpDefaultNix

main :: IO ()
main = do
  currentDir <- getCurrentDirectory
  args <- SE.getArgs
  case args of
        ["help"]  -> putStrLn help
        ["build"] -> buildImage
        ["init"]  -> initConfigFile (currentDir </> "docker.yml")
        _         -> SX.die "Unknown command. Please use \"sdit help\" to see available commands."

-- Help menu items
help :: String
help = unlines [
          "Usage: sdit [OPTION]",
          "Create DOCKER IMAGE(s) from configuration-file.\n",
          "\thelp              Displays this help menu.",
          "\tbuild             Builds the docker image.",
          "\tinit              Creates initial configuration-file(docker.yml)\n",
          "Homepage and help: https://github.com/dyrnade/smallDockerImagesTool"
      ]
