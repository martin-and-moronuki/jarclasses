module Test where

import Path
import Relude
import qualified ResourcePaths
import System.Directory (getCurrentDirectory)
import TestFramework
import Scheme

test :: Test
test = ResourcePaths.test scheme

main :: IO ()
main = getCwd >>= \cwd -> writeTestFiles test cwd
  where
    getCwd = getCurrentDirectory >>= parseAbsDir
