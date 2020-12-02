module Run where

import FileLayout
import FileLayoutPro
import FileWatch
import Logging
import Path
import Relude hiding (head)
import Resource
import ResourceBuilding
import StateOfResources (StateOfResources)
import qualified StateOfResources
import Style
import System.Directory (getCurrentDirectory)
import Test
import TestFramework
import WebServer

main :: IO ()
main =
  getCwd >>= \cwd ->
    getScheme >>= \scheme ->
      initFiles cwd scheme *> withLog \l ->
        StateOfResources.new >>= \rs ->
          fileWatch (logException l) (react scheme l rs) cwd (dirsToWatch scheme) (filesToWatch scheme) $
            serve scheme (ensureResourceBuilt scheme (writeToLog l) rs)
  where
    getCwd = getCurrentDirectory >>= Path.parseAbsDir
    initFiles cwd scheme = makeStyles cwd *> writeTestFiles (Test.test scheme) cwd

---  response to a file change  ---

react :: Scheme -> LogHandle -> StateOfResources Resource -> Path Rel File -> IO ()
react scheme l rs fp =
  do
    case pathAsResourceInput scheme (InputPath fp) of
      Nothing -> pure ()
      Just r -> atomically (StateOfResources.clearResourceStatus rs r) *> ensureResourceBuilt scheme (writeToLog l) rs r
    case pathAsResourceOutput scheme (OutputPath fp) of
      Nothing -> pure ()
      Just r -> atomically (StateOfResources.clearResourceStatus rs r)
