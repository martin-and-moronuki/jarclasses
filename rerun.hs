#! /usr/bin/env nix-shell
#! nix-shell -i runhaskell shell.nix

import System.Process

main :: IO ()
main = callProcess "ghcid" ["--command=" <> command, "--run=Run.main", "--warnings", "--reload=lib", "--restart=shell.nix", "--restart=rerun.hs"]

command :: String
command = "nix-shell --command \"" <> ghciCommand <> "\""

ghciCommand :: String
ghciCommand = "ghci -ignore-dot-ghci -Wall -fdefer-typed-holes -ilib run.hs format.hs"
