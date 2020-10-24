module Haskell where

import Relude

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
