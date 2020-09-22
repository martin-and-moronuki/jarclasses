#! /usr/bin/env nix-shell
#! nix-shell -i runhaskell shell.nix

{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}

import Control.Concurrent.Async (withAsync)
import qualified Control.Concurrent.STM as STM
import Control.Exception.Safe
import Control.Lens
import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified Network.HTTP.Types as HTTP
import Network.Wai (Request, Response)
import qualified Network.Wai as WAI
import qualified Network.Wai.Handler.Warp as Warp
import Path (Abs, Dir, File, Path, Rel, reldir, relfile)
import qualified Path
import qualified Prosidy
import ProsidyHtml
import Relude hiding (head)
import StringBuilding
import Style
import System.Directory (getCurrentDirectory)
import qualified System.FSNotify as FSN

dirsToWatch :: [Path Rel Dir]
dirsToWatch = [[reldir|menus|], [reldir|posts|]]

main :: IO ()
main =
  getCwd >>= \cwd ->
    initFiles cwd *> withLog \l ->
      atomically (STM.newTVar mempty) >>= \rs ->
        withNotification l cwd (react l rs) $
          serve l rs
  where
    getCwd = getCurrentDirectory >>= Path.parseAbsDir
    initFiles cwd = makeStyles cwd *> writeTestFile cwd
    withNotification l cwd r go =
      FSN.withManagerConf fsnConfig \man ->
        withWatches l man r cwd dirsToWatch go

---  response to a file change  ---

react :: LogHandle -> ResourcesState -> Path Rel File -> IO ()
react l rs fp =
  do
    case pathAsResourceInput fp of
      Nothing -> pure ()
      Just r -> atomically (clearResourceStatus rs r) *> ensureResourceBuilt l rs r
    case pathAsResourceOutput fp of
      Nothing -> pure ()
      Just r -> atomically (clearResourceStatus rs r)

---  dev web server  ---

serve :: LogHandle -> ResourcesState -> IO ()
serve l rs = Warp.runEnv 8000 (webapp l rs)

webapp :: LogHandle -> ResourcesState -> WAI.Application
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

---  Mappings between resource name, source path, and output file path  ---

type Resource = [Text]

resourceOutputPath :: Resource -> Maybe (Path Rel File)
resourceOutputPath [] = Nothing
resourceOutputPath r@("style" : _) = resourceRelFileBase r
resourceOutputPath r = (resourceRelFileBase >=> Path.addExtension ".html") r

resourceInputPath :: Resource -> Maybe (Path Rel File)
resourceInputPath [] = Nothing
resourceInputPath ("style" : _) = Nothing
resourceInputPath r = (resourceRelFileBase >=> Path.addExtension ".pro") r

resourceRelFileBase :: Resource -> Maybe (Path Rel File)
resourceRelFileBase r =
  unsnoc r >>= \(dirTexts, fileText) ->
    traverse (Path.parseRelDir . toString) dirTexts >>= \dirs ->
      (Path.parseRelFile . toString) fileText >>= \file ->
        Just $ foldr (Path.</>) file dirs

pathAsResourceInput :: Path Rel File -> Maybe Resource
pathAsResourceInput =
  Path.splitExtension >=> \case
    (p, ".pro") -> Just (relFileBaseResource p)
    _ -> Nothing

pathAsResourceOutput :: Path Rel File -> Maybe Resource
pathAsResourceOutput =
  Path.splitExtension >=> \case
    (p, ".html") -> Just (relFileBaseResource p)
    _ -> Nothing

relFileBaseResource :: Path Rel File -> Resource
relFileBaseResource file = f (Path.parent file) `snoc` txtFile (Path.filename file)
  where
    f :: Path Rel Dir -> [Text]
    f p = if Path.parent p == p then [] else f (Path.parent p) `snoc` txtDir (Path.dirname p)
    txtFile = toText . Path.toFilePath
    txtDir = fromMaybe (error "dir should have a trailing slash") . Text.stripSuffix "/" . toText . Path.toFilePath

resourceFileNameTests :: [Text]
resourceFileNameTests =
  [ "pathAsResourceInput" <!> quo "./menus/2019-11-11.pro" <!> "=" <!> show (pathAsResourceInput [relfile|menus/2019-11-11.pro|]),
    "pathAsResourceOutput" <!> quo "./menus/2019-11-11.pro" <!> "=" <!> show (pathAsResourceOutput [relfile|menus/2019-11-11.pro|]),
    "pathAsResourceInput" <!> quo "./style/jarclasses.css" <!> "=" <!> show (pathAsResourceInput [relfile|style/jarclasses.css|]),
    "pathAsResourceOutput" <!> quo "./style/jarclasses.css" <!> "=" <!> show (pathAsResourceOutput [relfile|style/jarclasses.css|]),
    "resourceInputPath" <!> show ["menus", "2019-11-11"] <!> "=" <!> show (resourceInputPath ["menus", "2019-11-11"]),
    "resourceInputPath" <!> show ["style", "jarclasses.css"] <!> "=" <!> show (resourceInputPath ["style", "jarclasses.css"]),
    "resourceOutputPath" <!> show ["menus", "2019-11-11"] <!> "=" <!> show (resourceOutputPath ["menus", "2019-11-11"]),
    "resourceOutputPath" <!> show ["style", "jarclasses.css"] <!> "=" <!> show (resourceOutputPath ["style", "jarclasses.css"])
  ]

---  building a resource  ---

buildResource :: LogHandle -> Resource -> IO ()
buildResource l r =
  do
    writeToLog l $ "Building " <> show r
    fpIn <- maybe undefined pure $ resourceInputPath r
    fpOut <- maybe undefined pure $ resourceOutputPath r
    src <- decodeUtf8 <$> readFileBS (Path.toFilePath fpIn)
    doc <- either (fail . show) pure $ Prosidy.parseDocument (Path.toFilePath fpIn) src
    writeFileLBS (Path.toFilePath fpOut) $ encodeUtf8 $ toText $ renderHtml $ proHtml doc

---  build management  ---

data ResourceStatus = Building | Built deriving (Eq, Ord)

type ResourcesState = TVar (Map Resource ResourceStatus)

ensureResourceBuilt :: LogHandle -> ResourcesState -> Resource -> IO ()
ensureResourceBuilt l rs r =
  if isJust (resourceInputPath r)
    then bracketOnError lock (const clear) go
    else pure ()
  where
    lock :: IO Bool =
      atomically $
        readTVar rs >>= \m -> case (Map.lookup r m) of
          Nothing -> lockResourceBuilding rs r *> pure True
          Just Built -> pure False
          Just Building -> STM.retry
    clear :: IO () = atomically (clearResourceStatus rs r)
    go :: Bool -> IO () = \case
      False -> pure ()
      True -> buildResource l r *> atomically (recordResourceBuilt rs r)

clearResourceStatus :: ResourcesState -> Resource -> STM ()
clearResourceStatus rs r = STM.modifyTVar rs $ Map.delete r

recordResourceBuilt :: ResourcesState -> Resource -> STM ()
recordResourceBuilt rs r = STM.modifyTVar rs $ Map.insert r Built

lockResourceBuilding :: ResourcesState -> Resource -> STM ()
lockResourceBuilding rs r = STM.modifyTVar rs $ Map.insert r Building

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
    txt = unlines tests

tests :: [Text]
tests = resourceFileNameTests
