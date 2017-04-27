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
         cmd :: String,
         entrypoint :: String,
         ports :: String,
         workingdir :: String,
         volumes :: String
  } deriving (Show)

instance FromJSON ImageConfig where
  parseJSON (Object i) = -- do
    --i <- o .: "config"
    ImageConfig <$>
     i .: "cmd" <*>
     i .: "entrypoint" <*>
     i .: "ports" <*>
     i .: "workingdir" <*>
     i .: "volumes" 
  parseJSON invalid    = typeMismatch "Config" invalid
  
-- Data ImageInfo
data ImageInformation = ImageInformation {
  name :: String,
  runAsRoot :: String,
  contents :: String,
  config :: ImageConfig
  } deriving (Show)

instance FromJSON ImageInformation where
  parseJSON (Object o) = do
    i <- o .: "image"
    ImageInformation <$>
     i .: "name" <*>
     i .: "runAsRoot" <*>
     i .: "contents" <*>
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

--trm :: Config
--trm = "asdsa"

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
  let fullPathEntryScript = currentDir </> entryScript
  entryScriptContent <- readEntryScriptContent fullPathEntryScript
  
  imageInfo <- useImageInformation
  let iConfig = config imageInfo
  let iName = name imageInfo
  let iRunAsRoot = runAsRoot imageInfo
  let iContents = contents imageInfo

  let configCmd = cmd iConfig
  let configEntryPoint = entrypoint iConfig
  let configPorts = ports iConfig
  let configWorkingDir = workingdir iConfig
  let configVolumes = volumes iConfig
  

  let defaultNix = unlines [
         "{ pkgs ? import <nixpkgs> {} }:",
         "with pkgs;",
         "let",
            "entrypoint = writeScript \"entrypoint.sh\" ''",
            " #!${stdenv.shell}",
              entryScriptContent ++
            "'';",
         "in",
         "dockerTools.buildImage {",
            "name = " ++ "\"" ++ iName ++ "\"" ++ ";",
             "runAsRoot = ''",
               iRunAsRoot ++
             "'';",

             "contents = [ "++ iContents ++" ];",

             "config = {",
                "Cmd = [ "++ "\"" ++ configCmd ++ "\"" ++ " ];",
                "Entrypoint = [ " ++ configEntryPoint ++" ];",
                "ExposedPorts = {",
                   "\"" ++ configPorts ++ "\"" ++" = {};",
                "};",
                "WorkingDir = " ++ "\"" ++ configWorkingDir ++ "\"" ++ ";",
                "Volumes = {",
                  "\"" ++ configVolumes ++ "\"" ++" = {};",
                "};",
             "};",
           "}"
        ]

  tmpDir <- getTemporaryDirectory
  let tmpDefaultNix = tmpDir </> "default.nix"
  writeFile tmpDefaultNix defaultNix
  
  let buildCommand = "nix-build " ++ tmpDefaultNix
  -- initCommand <- initConfigFile (currentDir </> "f.yml") fze
  
  args <- SE.getArgs    
  case args of
        ["help"]  -> putStrLn help
        ["build"] -> SP.system buildCommand  >>= SX.exitWith
        ["init"]  -> putStrLn "tmp"
        _         -> SX.die "Unknown argument"
  
-- Help menu items
help :: String
help = unlines [
          "-h     Displays this help menu.",
          "-b     Builds the docker image."
      ]
