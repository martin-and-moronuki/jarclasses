module Ghcid where

import System.Process
import Prelude

main :: IO ()
main = callProcess "ghcid" ["--command=runhaskell -ilib RunHaskell Ghci mainForGhcidInNixShell", "--outputfile=ghcid.txt", "--color=always", "--ignore-loaded", "--run=Run.main", "--warnings", "--reload=lib", "--restart=shell.nix", "--restart=lib/Haskell.hs"]
