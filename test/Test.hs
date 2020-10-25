module Test where

import Path
import Relude
import qualified ResourcePaths
import Scheme
import System.Directory (getCurrentDirectory)
import TestFramework

test :: Test
test = ResourcePaths.test scheme

main :: IO ()
main = getCwd >>= \cwd -> writeTestFiles test cwd
  where
    getCwd = getCurrentDirectory >>= parseAbsDir
