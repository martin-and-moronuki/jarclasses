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
import Style
import System.Directory (getCurrentDirectory)
import Test
import TestFramework
import WebServer

dirsToWatch :: [Path Rel Dir]
dirsToWatch = [[reldir|menus|], [reldir|posts|]]

scheme :: Scheme
scheme =
  Scheme
    { scheme_proHtmlDirs =
        fromList
          [ [reldir|menus|],
            [reldir|posts|]
          ],
      scheme_styleDirs =
        fromList
          [ [reldir|style|]
          ]
    }

main :: IO ()
main =
  getCwd >>= \cwd ->
    initFiles cwd *> withLog \l ->
      StateOfResources.new >>= \rs ->
        fileWatch (logException l) (react l rs) cwd dirsToWatch $
          serve scheme (ensureResourceBuilt l rs)
  where
    getCwd = getCurrentDirectory >>= Path.parseAbsDir
    initFiles cwd = makeStyles cwd *> writeTestFile (Test.test scheme) cwd

---  response to a file change  ---

react :: LogHandle -> StateOfResources Resource -> Path Rel File -> IO ()
react l rs fp =
  do
    case pathAsResourceInput scheme fp of
      Nothing -> pure ()
      Just r -> atomically (StateOfResources.clearResourceStatus rs r) *> ensureResourceBuilt l rs r
    case pathAsResourceOutput scheme fp of
      Nothing -> pure ()
      Just r -> atomically (StateOfResources.clearResourceStatus rs r)

---  building a resource  ---

ensureResourceBuilt :: LogHandle -> StateOfResources Resource -> Resource -> IO ()
ensureResourceBuilt l rs r =
  if isJust (resourceInputPath scheme r)
    then StateOfResources.ensureResourceBuilt (buildResource l r) rs r
    else pure ()

buildResource :: LogHandle -> Resource -> IO ()
buildResource l r =
  do
    writeToLog l $ "Building " <> show r
    fpIn <- maybe undefined pure $ resourceInputPath scheme r
    fpOut <- maybe undefined pure $ resourceOutputPath scheme r
    src <- decodeUtf8 <$> readFileBS (Path.toFilePath fpIn)
    doc <- either (fail . show) pure $ Prosidy.parseDocument (Path.toFilePath fpIn) src
    writeFileLBS (Path.toFilePath fpOut) $ encodeUtf8 $ toText $ renderHtml $ proHtml doc
