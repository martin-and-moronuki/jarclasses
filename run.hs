#! /usr/bin/env nix-shell
#! nix-shell -i runhaskell shell.nix

{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fdefer-typed-holes #-}

import Control.Concurrent.Async (withAsync)
import qualified Control.Concurrent.STM as STM
import Control.Exception.Safe (bracketOnError, finally)
import Control.Lens
import qualified Data.Map as Map
import qualified Network.HTTP.Types as HTTP
import Network.Wai (Request, Response)
import qualified Network.Wai as WAI
import qualified Network.Wai.Handler.Warp as Warp
import Path (Abs, Dir, File, Path, Rel, reldir)
import qualified Path
import qualified Prosidy
import Relude hiding (head)
import System.Directory (getCurrentDirectory)
import qualified System.FSNotify as FSN
import Text.Blaze.Html (Html, toHtml, toValue, (!))
import Text.Blaze.Html.Renderer.Utf8 (renderHtml)
import qualified Text.Blaze.Html5 as HTML
import qualified Text.Blaze.Html5.Attributes as Attr
import qualified Data.Text as Text

dirsToWatch :: [Path Rel Dir]
dirsToWatch = [[reldir|menus|], [reldir|posts|]]

main :: IO ()
main =
  (getCurrentDirectory >>= Path.parseAbsDir) >>= \cwd ->
    atomically STM.newTChan >>= \l ->
      atomically (STM.newTVar mempty) >>= \rs ->
        withLogPrinting l $
          FSN.withManagerConf fsnConfig \man ->
            withWatches man (react l rs) cwd dirsToWatch $
              serve l rs

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
    headers = [(HTTP.hContentType, "text/html; charset=utf-8")]

---  Mappings between resource name, source path, and output file path  ---

type Resource = [Text]

resourceOutputPath :: Resource -> Maybe (Path Rel File)
resourceOutputPath = resourceRelFileBase >=> Path.addExtension ".html"

resourceInputPath :: Resource -> Maybe (Path Rel File)
resourceInputPath = resourceRelFileBase >=> Path.addExtension ".pro"

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

---  building a resource  ---

buildResource :: LogHandle -> Resource -> IO ()
buildResource l r =
  do
    writeToLog l $ "Building " <> show r
    fpIn <- maybe undefined pure $ resourceInputPath r
    fpOut <- maybe undefined pure $ resourceOutputPath r
    src <- decodeUtf8 <$> readFileBS (Path.toFilePath fpIn)
    doc <- either (fail . show) pure $ Prosidy.parseDocument (Path.toFilePath fpIn) src
    writeFileLBS (Path.toFilePath fpOut) $ renderHtml $ proHtml doc

---  prosidy  ---

proHtml :: Prosidy.Document -> Html
proHtml doc = HTML.docTypeHtml ! Attr.lang "en" $ head <> body
  where
    head = HTML.head $ contentType <> title <> css
    contentType = HTML.meta ! Attr.httpEquiv "Content-Type" ! Attr.content "text/html; charset=utf-8"
    css = HTML.link ! Attr.rel "stylesheet" ! Attr.type_ "text/css" ! Attr.href "/style/jarclasses.css"
    title = foldMap (HTML.title . toHtml) (proTitle doc)
    body = HTML.body (proContentHtml doc)

proTitle :: Prosidy.Document -> Maybe Text
proTitle = view (Prosidy.atSetting "title")

proContentHtml :: Prosidy.Document -> Html
proContentHtml = foldMap proBlockHtml . view Prosidy.content

proBlockHtml :: Prosidy.Block -> Html
proBlockHtml = \case
  Prosidy.BlockLiteral x -> HTML.stringComment (show x)
  Prosidy.BlockParagraph x -> HTML.p (foldMap proInlineHtml (view Prosidy.content x))
  Prosidy.BlockTag x -> case (Prosidy.tagName x) of
    "day" -> HTML.h2 (foldMap proBlockHtml (view Prosidy.content x))
    "list" -> proListHtml x
    _ -> HTML.stringComment (show x)

proInlineHtml :: Prosidy.Inline -> Html
proInlineHtml = \case
  Prosidy.Break -> toHtml (" " :: String)
  Prosidy.InlineText x -> toHtml (Prosidy.fragmentText x)
  Prosidy.InlineTag x -> case (Prosidy.tagName x) of
    "dash" -> HTML.preEscapedToHtml ("&mdash;" :: Text)
    "emphatic" -> HTML.span ! Attr.class_ "font-style: italic" $ foldMap proInlineHtml (view Prosidy.content x)
    "title" -> HTML.span ! Attr.class_ "font-style: italic" $ foldMap proInlineHtml (view Prosidy.content x)
    "link" -> HTML.a ! (maybe mempty (Attr.href . toValue) $ view (Prosidy.atSetting "to") x) $ foldMap proInlineHtml (view Prosidy.content x)
    _ -> HTML.stringComment (show x)

proListHtml :: Prosidy.Tag (Prosidy.Series Prosidy.Block) -> Html
proListHtml x = HTML.ul $ foldMap itemHtml (view Prosidy.content x)
  where
    itemHtml = \case
      Prosidy.BlockTag i | Prosidy.tagName i == "item" -> HTML.li $ foldMap proBlockHtml (view Prosidy.content i)

---  build management  ---

data ResourceStatus = Building | Built deriving (Eq, Ord)

type ResourcesState = TVar (Map Resource ResourceStatus)

ensureResourceBuilt :: LogHandle -> ResourcesState -> Resource -> IO ()
ensureResourceBuilt l rs r = bracketOnError lock (const clear) go
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
fsnConfig = FSN.defaultConfig { FSN.confDebounce = FSN.NoDebounce }

withWatches :: FSN.WatchManager -> (Path Rel File -> IO ()) -> Path Abs Dir -> [Path Rel Dir] -> IO a -> IO a
withWatches man act cwd = fix \r ->
  \case
    [] -> id
    fp : fps -> withWatch man act cwd fp . r fps

withWatch :: FSN.WatchManager -> (Path Rel File -> IO ()) -> Path Abs Dir -> Path Rel Dir -> IO a -> IO a
withWatch man act cwd fp go =
  FSN.watchTree man (Path.toFilePath (cwd Path.</> fp)) (const True) action >>= \stop ->
    go `finally` stop
  where
    action :: FSN.Action
    action (FSN.Added (g -> Just f) _time False) = act f
    action (FSN.Modified (g -> Just f) _time False) = act f
    action _ = pure ()

    g = Path.parseAbsFile >=> Path.stripProperPrefix cwd

---  logging  ---

type LogHandle = STM.TChan String

writeToLog :: LogHandle -> String -> IO ()
writeToLog l s = atomically (STM.writeTChan l s)

withLogPrinting :: LogHandle -> IO a -> IO a
withLogPrinting l go = withAsync (printLogs l) \_ -> go

printLogs :: LogHandle -> IO a
printLogs l = forever printOne
  where
    printOne = pop >>= putStrLn
    pop = atomically $ STM.readTChan l
