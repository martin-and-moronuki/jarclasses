module Ghcid where

import Prelude
import System.Process

main :: IO ()
main = callProcess "ghcid" ["--command=runhaskell -ilib RunHaskell GhciInNixShell -ignore-dot-ghci", "--outputfile=ghcid.txt", "--color=always", "--ignore-loaded", "--run=Run.main", "--warnings", "--reload=lib", "--restart=shell.nix"]
