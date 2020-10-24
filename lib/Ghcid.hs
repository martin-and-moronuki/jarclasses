module Ghcid where

import Prelude
import System.Process

main :: IO ()
main = callProcess "ghcid" ["--command=runhaskell -ilib RunHaskell GhciInNixShell", "--ignore-loaded", "--run=Run.main", "--warnings", "--reload=lib", "--restart=shell.nix"]
