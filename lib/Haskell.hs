{-# LANGUAGE QuasiQuotes #-}

module Haskell where

import Path
import Path.IO
import Relude
import System.Process

languageExtensions :: [String]
languageExtensions =
  [ "BlockArguments",
    "ConstraintKinds",
    "DerivingVia",
    "LambdaCase",
    "NoImplicitPrelude",
    "OverloadedStrings",
    "QuasiQuotes",
    "ScopedTypeVariables",
    "TypeFamilies",
    "ViewPatterns"
  ]

extensionFlags :: [String]
extensionFlags = map ("-X" <>) languageExtensions

run :: String -> String -> IO ()
run x y = callProcess "ghc" (["-ilib"] <> extensionFlags <> [x, "-e", y])

ghci :: IO ()
ghci =
  getTargets >>= \targets ->
    callProcess "ghci" (["-Wall", "-fdefer-typed-holes", "-ilib", "-ferror-spans", "-fdiagnostics-color=always"] <> extensionFlags <> targets)

ghcid :: IO ()
ghcid =
  callProcess "ghcid" ["--command=ghc -ilib Haskell -e mainForGhcidInNixShell", "--outputfile=ghcid.txt", "--color=always", "--ignore-loaded", "--run=Run.main", "--warnings", "--reload=lib", "--restart=shell.nix", "--restart=lib/Haskell.hs"]

mainForGhcidInNixShell :: IO ()
mainForGhcidInNixShell =
  callProcess "nix-shell" ["shell.nix", "--pure", "--command", "ghc -ilib Haskell -e mainForGhcid"]

mainForGhcid :: IO ()
mainForGhcid =
  getTargets >>= \targets ->
    callProcess "ghci" (["-Wall", "-fdefer-typed-holes", "-ilib", "-ferror-spans", "-fdiagnostics-color=always", "-ignore-dot-ghci"] <> extensionFlags <> targets)

getTargets :: IO [String]
getTargets =
  fmap (\(_, xs) -> mapMaybe pathModule xs) $
    listDirRecurRel [reldir|lib|]

pathModule :: Path Rel File -> Maybe String
pathModule x =
  case splitExtension x of
    Just (y, ".hs") -> Just $ map slashToDot $ toFilePath y
    _ -> Nothing

slashToDot :: Char -> Char
slashToDot c =
  case c of
    '/' -> '.'
    _ -> c
