#! /usr/bin/env nix-shell
#! nix-shell -i runhaskell shell.nix

{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Run where

import BlazeHtmlRendering
import Control.Concurrent.Async (withAsync)
import qualified Control.Concurrent.STM as STM
import Control.Exception.Safe
import qualified Network.HTTP.Types as HTTP
import Network.Wai (Request, Response)
import qualified Network.Wai as WAI
import qualified Network.Wai.Handler.Warp as Warp
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
import qualified System.FSNotify as FSN

dirsToWatch :: [Path Rel Dir]
dirsToWatch = [[reldir|menus|], [reldir|posts|]]

main :: IO ()
main =
  getCwd >>= \cwd ->
    initFiles cwd *> withLog \l ->
      atomically STM.Map.new >>= \rs ->
        withNotification l cwd (react l rs) $
          serve l rs
  where
    getCwd = getCurrentDirectory >>= Path.parseAbsDir
    initFiles cwd = makeStyles cwd *> writeTestFile cwd
    withNotification l cwd r go =
      FSN.withManagerConf fsnConfig \man ->
        withWatches l man r cwd dirsToWatch go

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

---  dev web server  ---

serve :: LogHandle -> StateOfResources Resource -> IO ()
serve l rs = Warp.runEnv 8000 (webapp l rs)

webapp :: LogHandle -> StateOfResources Resource -> WAI.Application
webapp l rs request respond = ensureResourceBuilt l rs r *> go
  where
    r = requestResource request
    go = case (responseResource r) of
      Just response -> respond response
      Nothing -> undefined

requestResource :: Request -> Resource
requestResource = WAI.pathInfo

responseResource :: Resource -> Maybe Response
responseResource r =
  resourceOutputPath r >>= \fp ->
    pure $ WAI.responseFile HTTP.ok200 headers (Path.toFilePath fp) Nothing
  where
    headers = [(HTTP.hContentType, resourceContentType r)]

resourceContentType :: Resource -> ByteString
resourceContentType ("menus" : _) = "text/html; charset=utf-8"
resourceContentType ("posts" : _) = "text/html; charset=utf-8"
resourceContentType ("style" : _) = "text/css"

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

---  file watch setup  ---

fsnConfig :: FSN.WatchConfig
fsnConfig = FSN.defaultConfig {FSN.confDebounce = FSN.NoDebounce}

withWatches :: LogHandle -> FSN.WatchManager -> (Path Rel File -> IO ()) -> Path Abs Dir -> [Path Rel Dir] -> IO a -> IO a
withWatches l man act cwd = fix \r ->
  \case
    [] -> id
    fp : fps -> withWatch l man act cwd fp . r fps

withWatch :: LogHandle -> FSN.WatchManager -> (Path Rel File -> IO ()) -> Path Abs Dir -> Path Rel Dir -> IO a -> IO a
withWatch l man act cwd fp go =
  FSN.watchTree man (Path.toFilePath (cwd Path.</> fp)) (const True) action >>= \stop ->
    go `finally` stop
  where
    action :: FSN.Action
    action e = case g e of
      Just f ->
        -- https://github.com/haskell-fswatch/hfsnotify/issues/91
        act f `catchAny` (\e -> writeToLog l (displayException e))
      Nothing -> pure ()

    g = (Path.parseAbsFile >=> Path.stripProperPrefix cwd) . FSN.eventPath

---  logging  ---

type LogHandle = STM.TChan String

withLog :: (LogHandle -> IO a) -> IO a
withLog go =
  atomically STM.newTChan >>= \l ->
    withLogPrinting l (go l)

writeToLog :: LogHandle -> String -> IO ()
writeToLog l s = atomically (STM.writeTChan l s)

withLogPrinting :: LogHandle -> IO a -> IO a
withLogPrinting l go = withAsync (printLogs l) \_ -> go

printLogs :: LogHandle -> IO a
printLogs l = forever printOne
  where
    printOne = pop >>= putStrLn
    pop = atomically $ STM.readTChan l

---  tests  ---

writeTestFile :: Path Abs Dir -> IO ()
writeTestFile dir = writeFileLBS path (encodeUtf8 txt)
  where
    path = Path.toFilePath (dir Path.</> [relfile|test.txt|])
    txt = unlines (toList Run.test)

test :: Seq Text
test = ResourcePaths.test
