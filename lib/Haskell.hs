module Haskell where

import Relude

languageExtensions :: [String]
languageExtensions =
  [ "BlockArguments",
    "ConstraintKinds",
    "LambdaCase",
    "NoImplicitPrelude",
    "OverloadedStrings",
    "QuasiQuotes",
    "ScopedTypeVariables",
    "ViewPatterns"
  ]

extensionFlags :: [String]
extensionFlags = map ("-X" <>) languageExtensions
