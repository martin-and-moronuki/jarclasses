#! /usr/bin/env nix-shell
#! nix-shell -i runhaskell shell.nix

import System.Process

main :: IO ()
main = callProcess "ghcid" ["--command=ghci -ignore-dot-ghci -Wall -fdefer-typed-holes -ilib run.hs", "--run=:main", "--warnings", "--reload=lib", "--restart=rerun.hs"]
