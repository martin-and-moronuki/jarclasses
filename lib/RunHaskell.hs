{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}

module RunHaskell where

import Haskell
import System.Environment
import System.Process
import Prelude

main :: IO ()
main =
  getArgs >>= \case
    [x, y] ->
      callProcess "ghc" (["-ilib"] <> extensionFlags <> [x, "-e", y])
