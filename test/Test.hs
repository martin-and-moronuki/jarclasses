module Test where

import FileLayout (Scheme)
import qualified FileLayout
import FileLayoutPro
import Path
import Relude
import System.Directory (getCurrentDirectory)
import TestFramework

test :: Scheme -> Test
test = FileLayout.test

main :: IO ()
main = getCwd >>= \cwd -> getScheme >>= \scheme -> writeTestFiles (test scheme) cwd
  where
    getCwd = getCurrentDirectory >>= parseAbsDir
