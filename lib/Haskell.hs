module Haskell where

languageExtensions :: String
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

extensionFlags = map ("-X" <>) languageExtensions
