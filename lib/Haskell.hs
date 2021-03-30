{-

This module is used to boot other modules, and so there are a few limitations:

  - Language pragmas are required for any extensions used in this module.
  - This module should only import from packages; it should not import any local modules.

-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Haskell where

import Path
import Path.IO
import Relude
import System.Process
import qualified Prelude

-- Directories containing Haskell source files
hsSourceDirs :: [Path Rel Dir]
hsSourceDirs =
  [ [reldir|file-layout|],
    [reldir|home|],
    [reldir|lib|],
    [reldir|menus|],
    [reldir|style|],
    [reldir|tags|],
    [reldir|test|],
    [reldir|versions|]
  ]

srcDirFlags :: [String]
srcDirFlags = map (\d -> "-i" <> toFilePath d) hsSourceDirs

languageExtensions :: [String]
languageExtensions =
  [ "BlockArguments",
    "ConstraintKinds",
    "DeriveAnyClass",
    "DeriveDataTypeable",
    "DeriveGeneric",
    "DerivingVia",
    "GeneralizedNewtypeDeriving",
    "LambdaCase",
    "MultiWayIf",
    "NamedFieldPuns",
    "NoImplicitPrelude",
    "OverloadedStrings",
    "QuasiQuotes",
    "ScopedTypeVariables",
    "TemplateHaskell",
    "TypeApplications",
    "TypeFamilies",
    "ViewPatterns"
  ]

extensionFlags :: [String]
extensionFlags = map ("-X" <>) languageExtensions

run :: String -> String -> IO ()
run x y =
  callProcess "ghc" $
    srcDirFlags
      <> extensionFlags
      <> [x, "-e", y]

ghci :: IO ()
ghci =
  getTargets >>= \targets ->
    callProcess "ghci" (["-Wall", "-fdefer-typed-holes"] <> srcDirFlags <> ["-ferror-spans", "-fdiagnostics-color=always"] <> extensionFlags <> targets)

ghcid :: IO ()
ghcid =
  getHsFiles >>= \hsFiles ->
    callProcess "ghcid" $
      ["--command=ghc " <> Prelude.unwords srcDirFlags <> " Haskell -e mainForGhcidInNixShell", "--outputfile=ghcid.txt", "--color=always", "--ignore-loaded", "--run=Run.main", "--warnings"] <> ghcidReloadFlags hsFiles <> ["--restart=shell.nix", "--restart=haskell.nix", "--restart=lib/Haskell.hs"]

ghcidReloadFlags :: [Path Rel File] -> [String]
ghcidReloadFlags hsFiles =
  map (\x -> "--reload=" <> toFilePath x) hsFiles

mainForGhcidInNixShell :: IO ()
mainForGhcidInNixShell =
  callProcess "nix-shell" ["shell.nix", "--pure", "--command", "ghc " <> Prelude.unwords srcDirFlags <> " Haskell -e mainForGhcid"]

mainForGhcid :: IO ()
mainForGhcid =
  getTargets >>= \targets ->
    callProcess "ghci" $
      ["-Wall", "-fdefer-typed-holes"]
        <> srcDirFlags
        <> ["-ferror-spans", "-fdiagnostics-color=always", "-ignore-dot-ghci"]
        <> extensionFlags
        <> targets

getHsFiles :: IO [Path Rel File]
getHsFiles =
  fmap (fold . fmap (\(_, xs) -> mapMaybe pathHsFile xs)) $
    traverse listDirRecurRel hsSourceDirs

pathHsFile :: Path Rel File -> Maybe (Path Rel File)
pathHsFile x =
  case splitExtension x of
    Just (_, ".hs") -> Just x
    _ -> Nothing

getTargets :: IO [String]
getTargets =
  fmap (fold . fmap (\(_, xs) -> mapMaybe pathModule xs)) $
    traverse listDirRecurRel hsSourceDirs

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
