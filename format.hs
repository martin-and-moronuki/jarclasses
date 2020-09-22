#! /usr/bin/env nix-shell
#! nix-shell -i runhaskell shell.nix

import qualified Data.ByteString.Lazy as LBS
import Data.Foldable
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy.Encoding as LT
import qualified Data.Text.Lazy.IO as LT
import Ormolu
import Relude

main :: IO ()
main = traverse_ format files

files :: [FilePath]
files = ["format.hs", "run.hs", "rerun.hs", "lib/StringBuilding.hs", "lib/Style.hs"]

format :: FilePath -> IO ()
format fp =
  T.readFile fp >>= \input ->
    ormolu defaultConfig fp (toString input) >>= \output ->
      T.writeFile fp output
