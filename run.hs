#! /usr/bin/env nix-shell
#! nix-shell -i runhaskell shell.nix

module Run where

import BlazeHtmlRendering
import FileWatch
import Logging
import Path (Dir, File, Path, Rel, reldir)
import qualified Path
import qualified Prosidy
import ProsidyHtml
import Relude hiding (head)
import ResourcePaths
import StateOfResources (StateOfResources)
import qualified StateOfResources
import ResourceBuilding
import Style
import Scheme
import System.Directory (getCurrentDirectory)
import Test
import TestFramework
import WebServer

dirsToWatch :: [Path Rel Dir]
dirsToWatch = [[reldir|menus|], [reldir|posts|]]

main :: IO ()
main =
  getCwd >>= \cwd ->
    initFiles cwd *> withLog \l ->
      StateOfResources.new >>= \rs ->
        fileWatch (logException l) (react l rs) cwd dirsToWatch $
          serve scheme (ensureResourceBuilt (writeToLog l) rs)
  where
    getCwd = getCurrentDirectory >>= Path.parseAbsDir
    initFiles cwd = makeStyles cwd *> writeTestFile (Test.test scheme) cwd

---  response to a file change  ---

react :: LogHandle -> StateOfResources Resource -> Path Rel File -> IO ()
react l rs fp =
  do
    case pathAsResourceInput scheme fp of
      Nothing -> pure ()
      Just r -> atomically (StateOfResources.clearResourceStatus rs r) *> ensureResourceBuilt (writeToLog l) rs r
    case pathAsResourceOutput scheme fp of
      Nothing -> pure ()
      Just r -> atomically (StateOfResources.clearResourceStatus rs r)
