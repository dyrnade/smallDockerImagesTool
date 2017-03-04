module Main where

import           Data.String
import qualified System.Environment as SE
import qualified System.Exit        as SX
import qualified System.Process     as SP

main = do
  args <- SE.getArgs
  case args of
        ["-h"] -> putStrLn help
        ["-b"] -> SP.system "nix-build ./default.nix" >>= SX.exitWith
        _      -> SX.die "Unknown argument"

-- Help menu items
help :: String
help = unlines [
          "-h     Displays this help menu.",
          "-b     Builds the docker image."
      ]
