{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
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
import Data.Yaml.Pretty as YP
import Control.Lens
import qualified Data.ByteString as BS
import Data.Aeson
import Data.Aeson.TH
import Data.Aeson.Types
import Control.Applicative
import qualified Control.Monad.IO.Class as X
import Control.Monad
import Data.Maybe
-- import Ghc.Generics


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
  parseJSON (Object i) = -- do
    --i <- o .: "config"
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
  Y.decodeFileEither "docker.yml" -- :: IO (Maybe ImageInformation)
  -- #return (info)

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
       " ---",
       "# YAML for Docker Image",
       "\n",
       "entryPointScript: entrypoint.sh # Entrypoint script you want when container initialized",
       "image:",
         "name: #image name",
         "runAsRoot: | # commands to run as root user ",
         "\n",
         "contents: #",
         "config:",
          "cmd: #command to run",
          "entrypoint: entrypoint",
          "ports: \"\" # ports to use ",
          "workingdir: # Working Directory",
          "volumes: # Volumes to be mounted",
          "..."
       ]
  writeFile filePath content

-- initConfigFile :: FilePath -> String -> IO ()
-- initConfigFile xfilePath bs = do
--   let content = [
--        " ---",
--        "# YAML for Docker Image",
--        "\n",
--        "entryPointScript: entrypoint.sh # Entrypoint script you want when container initialized",
--        "image:",
--          "name: #image name",
--          "runAsRoot: | # commands to run as root user ",
--          "\n",
--          "contents: #",
--          "config:",
--           "cmd: #command to run",
--           "entrypoint: entrypoint",
--           "ports: \"\" # ports to use ",
--           "workingdir: # Working Directory",
--           "volumes: # Volumes to be mounted",
--           "..."
--        ]
--    writeFile xfilePath bs
main = do
  
  currentDir <- getCurrentDirectory
  epPath <- useEntryPointPath
  let entryScript = takeEntryPointScript epPath
  entryScriptContent <- case epPath of { Nothing -> return "empty"; Just path -> readEntryScriptContent (currentDir </> (entryPointScript path)) }

  let entryf = "let\n  entrypoint = writeScript \"entrypoint.sh\" ''\n    #!${stdenv.shell}\n" ++ (unlines $ map (\e -> "    " ++ e) (lines entryScriptContent)) ++ "  '';\nin"
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

  
       -- configEntryPoint = if entryScript /= ""
       --   then  case (entrypoint iConfig) of
       --          Just ep -> "    Entrypoint = [ " ++ ep ++" ];"
       --          Nothing -> "empty"
       
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
             entryf,
             "dockerTools.buildImage {",
             "  name = " ++ "\"" ++ iName ++ "\"" ++ ";",
             iRunAsRoot,
             "",
             iContents,
             "",
             "  config = {",
             configCmd,
             --"    Entrypoint = [ " ++ configEntryPoint ++" ];",
             configEntryPoint,
             configPorts,

             --"    WorkingDir = " ++ "\"" ++ configWorkingDir ++ "\"" ++ ";",
             configWorkingDir,            
             configVolumes,
             
             "  };",
             "}"
            ]

  tmpDir <- getTemporaryDirectory
  let tmpDefaultNix = tmpDir </> "default.nix"
  writeFile tmpDefaultNix defaultNix

  let
    extraArgsForBuildCommand :: String -> String
    extraArgsForBuildCommand a = a
    --extraArgsForBuildCommand "" = ""
  let buildCommand = "nix-build " ++ tmpDefaultNix 
  let initCommand  = initConfigFile (currentDir </> "f.yml")
  
  args <- SE.getArgs    
  case args of
        ["help"]  -> putStrLn help
        --["build",Just extra] -> SP.system (buildCommand ++ " " ++  fromMaybe extra)  >>= SX.exitWith
        ["build"] -> SP.system buildCommand  >>= SX.exitWith
        ["init"]  -> initCommand
        ["print"] -> putStrLn defaultNix
        ["f"]     -> print $ unlines $ lines entryScriptContent
        _         -> SX.die "Unknown argument"
-- Help menu items
help :: String
help = unlines [
          "help     Displays this help menu.",
          "build    Builds the docker image.",
          "init     Creates initial configuration(docker.yml)"
      ]
