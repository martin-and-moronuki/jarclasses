module GhciInNixShell where

import Prelude
import System.Environment
import System.Process

main :: IO ()
main =
  getArgs >>= \args ->
    callProcess "nix-shell" ["shell.nix", "--pure", "--command", unwords $ ["runhaskell -ilib RunHaskell Ghci"] <> args]
