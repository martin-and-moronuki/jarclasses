module GhciInNixShell where

import Prelude
import System.Process

main :: IO ()
main = callProcess "nix-shell" ["shell.nix", "--pure", "--command", "runhaskell -ilib RunHaskell Ghci"]
