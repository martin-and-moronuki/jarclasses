module Ghci where

import Haskell
import Path
import Path.IO
import Relude
import System.Environment
import System.Process

main :: IO ()
main =
  getTargets >>= \targets ->
    callProcess "ghci" (["-Wall", "-fdefer-typed-holes", "-ilib", "-ferror-spans", "-fdiagnostics-color=always"] <> extensionFlags <> targets)

mainForGhcidInNixShell :: IO ()
mainForGhcidInNixShell =
  getArgs >>= \args ->
    callProcess "nix-shell" ["shell.nix", "--pure", "--command", "runhaskell -ilib RunHaskell Ghci mainForGhcid"]

mainForGhcid :: IO ()
mainForGhcid =
  getTargets >>= \targets ->
    callProcess "ghci" (["-Wall", "-fdefer-typed-holes", "-ilib", "-ferror-spans", "-fdiagnostics-color=always", "-ignore-dot-ghci"] <> extensionFlags <> targets)

getTargets :: IO [String]
getTargets = fmap (\(_, xs) -> mapMaybe pathModule xs) $ listDirRecurRel [reldir|lib|]

pathModule :: Path Rel File -> Maybe String
pathModule x =
  case splitExtension x of
    Just (y, ".hs") -> Just $ map slashToDot $ toFilePath y
    _ -> Nothing

slashToDot :: Char -> Char
slashToDot = \case '/' -> '.'; c -> c
