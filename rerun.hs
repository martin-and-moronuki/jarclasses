#! /usr/bin/env nix-shell
#! nix-shell -i runhaskell shell.nix

import System.Process

main :: IO ()
main = callProcess "ghcid" ["--command=" <> command, "--run=Run.main", "--warnings", "--reload=lib", "--restart=shell.nix", "--restart=.ghci", "--restart=rerun.hs"]

command :: String
command = "nix-shell --command \"" <> ghciCommand <> "\""

ghciCommand :: String
ghciCommand = "ghci -Wall -fdefer-typed-holes -ilib run.hs buildAll.hs format.hs"
