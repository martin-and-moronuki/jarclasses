#! /usr/bin/env nix-shell
#! nix-shell -i runhaskell shell.nix

{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fdefer-typed-holes #-}

import Control.Concurrent.Async (withAsync)
import qualified Control.Concurrent.STM as STM
import Control.Exception.Safe (bracketOnError, finally)
import qualified Data.Map as Map
import qualified Network.HTTP.Types as HTTP
import Network.Wai (Request, Response)
import qualified Network.Wai as WAI
import qualified Network.Wai.Handler.Warp as Warp
import Path (File, Path, Rel)
import qualified Path
import qualified Prosidy
import Relude hiding (head)
import qualified System.FSNotify as FSN
import Text.Blaze.Html (Html, toHtml)
import Text.Blaze.Html.Renderer.Utf8 (renderHtml)
import qualified Text.Blaze.Html5 as HTML
import Control.Lens

main :: IO ()
main =
  atomically STM.newTChan >>= \l ->
    atomically (STM.newTVar mempty) >>= \rs ->
      withLogPrinting l $
        FSN.withManager \man ->
          withWatches man (react l rs) ["menus", "posts"] $
            serve l rs

react :: LogHandle -> ResourcesState -> FSN.Event -> IO ()
react l _rs e = writeToLog l (show e)

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

---  building a resource  ---

buildResource :: LogHandle -> Resource -> IO ()
buildResource l r =
  do
    fpIn <- maybe undefined pure $ resourceInputPath r
    fpOut <- maybe undefined pure $ resourceOutputPath r
    src <- decodeUtf8 <$> readFileBS (Path.toFilePath fpIn)
    doc <- either (fail . show) pure $ Prosidy.parseDocument (Path.toFilePath fpIn) src
    writeFileLBS (Path.toFilePath fpOut) $ renderHtml $ proHtml doc

---  prosidy  ---

proHtml :: Prosidy.Document -> Html
proHtml doc = HTML.docTypeHtml (head <> body)
  where
    head = HTML.head title
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
        "day" -> HTML.h2 (foldMap proBlockHtml' (view Prosidy.content x))
        "list" -> proListHtml x

proBlockHtml' :: Prosidy.Block -> Html
proBlockHtml' = \case
    Prosidy.BlockParagraph x -> foldMap proInlineHtml (view Prosidy.content x)

proInlineHtml :: Prosidy.Inline -> Html
proInlineHtml = \case
    Prosidy.Break -> toHtml (" " :: String)
    Prosidy.InlineText x -> toHtml (Prosidy.fragmentText x)
    Prosidy.InlineTag x ->
        -- todo: switch on the tagName and do interesting things
        foldMap proInlineHtml (view Prosidy.content x)

proListHtml :: Prosidy.Tag (Prosidy.Series Prosidy.Block) -> Html
proListHtml x = HTML.ul $ foldMap itemHtml (view Prosidy.content x)
  where
    itemHtml = \case
        Prosidy.BlockTag i | Prosidy.tagName i == "item" -> HTML.li $ foldMap proBlockHtml' (view Prosidy.content i)

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

withWatches :: FSN.WatchManager -> FSN.Action -> [FilePath] -> IO a -> IO a
withWatches man act = fix \r ->
  \case
    [] -> id
    fp : fps -> withWatch man act fp . r fps

withWatch :: FSN.WatchManager -> FSN.Action -> FilePath -> IO a -> IO a
withWatch man act fp go =
  FSN.watchTree man fp (const True) act >>= \stop ->
    go `finally` stop

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

-- ---  miscellania  ---

-- unsnoc :: [a] -> Maybe ([a], a)
-- unsnoc xs = uncons (reverse xs) >>= \(x, xs') -> Just (reverse xs', x)
