#! /usr/bin/env nix-shell
#! nix-shell -i runhaskell shell.nix

module Run where

import BlazeHtmlRendering
import FileWatch
import Logging
import Path (Abs, Dir, File, Path, Rel, reldir, relfile)
import qualified Path
import qualified Prosidy
import ProsidyHtml
import Relude hiding (head)
import ResourcePaths
import StateOfResources (StateOfResources)
import qualified StateOfResources
import qualified StmContainers.Map as STM.Map
import Style
import System.Directory (getCurrentDirectory)
import WebServer

dirsToWatch :: [Path Rel Dir]
dirsToWatch = [[reldir|menus|], [reldir|posts|]]

main :: IO ()
main =
  getCwd >>= \cwd ->
    initFiles cwd *> withLog \l ->
      atomically STM.Map.new >>= \rs ->
        fileWatch (logException l) (react l rs) cwd dirsToWatch $
          serve (ensureResourceBuilt l rs)
  where
    getCwd = getCurrentDirectory >>= Path.parseAbsDir
    initFiles cwd = makeStyles cwd *> writeTestFile cwd

---  response to a file change  ---

react :: LogHandle -> StateOfResources Resource -> Path Rel File -> IO ()
react l rs fp =
  do
    case pathAsResourceInput fp of
      Nothing -> pure ()
      Just r -> atomically (StateOfResources.clearResourceStatus rs r) *> ensureResourceBuilt l rs r
    case pathAsResourceOutput fp of
      Nothing -> pure ()
      Just r -> atomically (StateOfResources.clearResourceStatus rs r)

---  building a resource  ---

ensureResourceBuilt :: LogHandle -> StateOfResources Resource -> Resource -> IO ()
ensureResourceBuilt l rs r =
  if isJust (resourceInputPath r)
    then StateOfResources.ensureResourceBuilt (buildResource l r) rs r
    else pure ()

buildResource :: LogHandle -> Resource -> IO ()
buildResource l r =
  do
    writeToLog l $ "Building " <> show r
    fpIn <- maybe undefined pure $ resourceInputPath r
    fpOut <- maybe undefined pure $ resourceOutputPath r
    src <- decodeUtf8 <$> readFileBS (Path.toFilePath fpIn)
    doc <- either (fail . show) pure $ Prosidy.parseDocument (Path.toFilePath fpIn) src
    writeFileLBS (Path.toFilePath fpOut) $ encodeUtf8 $ toText $ renderHtml $ proHtml doc

---  tests  ---

writeTestFile :: Path Abs Dir -> IO ()
writeTestFile dir = writeFileLBS path (encodeUtf8 txt)
  where
    path = Path.toFilePath (dir Path.</> [relfile|test.txt|])
    txt = unlines (toList Run.test)

test :: Seq Text
test = ResourcePaths.test
