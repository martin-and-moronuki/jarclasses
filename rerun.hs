#! /usr/bin/env nix-shell
#! nix-shell -i runhaskell shell.nix

import System.Process

main :: IO ()
main = callProcess "ghcid" ["--command=ghci run.hs", "--run=:main", "--warnings"]
