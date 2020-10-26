module Run where

import FileWatch
import Logging
import Path
import Relude hiding (head)
import ResourceBuilding
import ResourcePaths
import Scheme
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
    initFiles cwd *> withLog \l ->
      StateOfResources.new >>= \rs ->
        fileWatch (logException l) (react l rs) cwd (dirsToWatch scheme) (filesToWatch scheme) $
          serve scheme (ensureResourceBuilt (writeToLog l) rs)
  where
    getCwd = getCurrentDirectory >>= Path.parseAbsDir
    initFiles cwd = makeStyles cwd *> writeTestFiles Test.test cwd

---  response to a file change  ---

react :: LogHandle -> StateOfResources Resource -> Path Rel File -> IO ()
react l rs fp =
  do
    case pathAsResourceInput scheme (InputPath fp) of
      Nothing -> pure ()
      Just r -> atomically (StateOfResources.clearResourceStatus rs r) *> ensureResourceBuilt (writeToLog l) rs r
    case pathAsResourceOutput scheme (OutputPath fp) of
      Nothing -> pure ()
      Just r -> atomically (StateOfResources.clearResourceStatus rs r)
