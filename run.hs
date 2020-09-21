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

import Clay (Css, (?), (<?))
import qualified Clay
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
import Relude hiding (head)
import System.Directory (getCurrentDirectory)
import qualified System.FSNotify as FSN
import Text.Blaze.Html (Html, toHtml, toValue, (!))
import qualified Text.Blaze.Html5 as HTML
import qualified Text.Blaze.Html5.Attributes as Attr
import Relude.Extra.Foldable1
import qualified Text.Blaze.Internal as Blaze
import qualified Text.Blaze.Renderer.String as Blaze

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

---  prosidy  ---

proHtml :: Prosidy.Document -> Html
proHtml doc = HTML.docTypeHtml ! Attr.lang "en" $ head <> body
  where
    head = HTML.head $ contentType <> title <> css
    contentType = HTML.meta ! Attr.httpEquiv "Content-Type" ! Attr.content "text/html; charset=utf-8"
    css = HTML.link ! Attr.rel "stylesheet" ! Attr.type_ "text/css" ! Attr.href "/style/jarclasses.css"
    title = foldMap (HTML.title . toHtml) $ proTitle doc
    body = HTML.body content
    content = HTML.main $ do
        foldMap (HTML.h1 . HTML.p . toHtml) $ proTitle doc
        foldMap proBlockHtml $ view Prosidy.content doc

proTitle :: Prosidy.Document -> Maybe Text
proTitle = view (Prosidy.atSetting "title")

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
    "emphatic" -> HTML.span ! Attr.class_ "emphatic" $ foldMap proInlineHtml (view Prosidy.content x)
    "title" -> HTML.span ! Attr.class_ "title" $ foldMap proInlineHtml (view Prosidy.content x)
    "link" -> HTML.a ! (maybe mempty (Attr.href . toValue) $ view (Prosidy.atSetting "to") x) $ foldMap proInlineHtml (view Prosidy.content x)
    _ -> HTML.stringComment (show x)

proListHtml :: Prosidy.Tag (Prosidy.Series Prosidy.Block) -> Html
proListHtml x = HTML.ul $ foldMap itemHtml (view Prosidy.content x)
  where
    itemHtml = \case
      Prosidy.BlockTag i | Prosidy.tagName i == "item" -> HTML.li $ foldMap proBlockHtml (view Prosidy.content i)

---  html rendering  ---

renderHtml :: Html -> String
renderHtml html = renderHtmlIndented html ""

-- forked from Text.Blaze.Renderer.Pretty
renderHtmlIndented :: Blaze.MarkupM b -> String -> String
renderHtmlIndented = go 0 id
  where
    contentInline :: Blaze.StaticString -> Bool
    contentInline open = Blaze.getString open "" `elem` ["<p", "<title"]

    go :: Int -> (String -> String) -> Blaze.MarkupM b -> String -> String

    go i attrs (Blaze.Parent _ open close content) | contentInline open =
        ind i . Blaze.getString open . attrs . (">" ++) . renderHtmlCompact content . Blaze.getString close .  ('\n' :)

    go i attrs (Blaze.Parent _ open close content) =
        ind i . Blaze.getString open . attrs . (">\n" ++) . go (inc i) id content
              . ind i . Blaze.getString close .  ('\n' :)
    go i attrs (Blaze.CustomParent tag content) =
        ind i . ('<' :) . Blaze.fromChoiceString tag . attrs . (">\n" ++) .
        go (inc i) id content . ind i . ("</" ++) . Blaze.fromChoiceString tag .
        (">\n" ++)
    go i attrs (Blaze.Leaf _ begin end _) =
        ind i . Blaze.getString begin . attrs . Blaze.getString end . ('\n' :)
    go i attrs (Blaze.CustomLeaf tag close _) =
        ind i . ('<' :) . Blaze.fromChoiceString tag . attrs .
        ((if close then " />\n" else ">\n") ++)
    go i attrs (Blaze.AddAttribute _ key value h) = flip (go i) h $
        Blaze.getString key . Blaze.fromChoiceString value . ('"' :) . attrs
    go i attrs (Blaze.AddCustomAttribute key value h) = flip (go i) h $
        (' ' : ) . Blaze.fromChoiceString key . ("=\"" ++) . Blaze.fromChoiceString value .
        ('"' :) .  attrs
    go i _ (Blaze.Content content _) = ind i . Blaze.fromChoiceString content . ('\n' :)
    go i _ (Blaze.Comment comment _) = ind i .
        ("<!-- " ++) . Blaze.fromChoiceString comment . (" -->\n" ++)
    go i attrs (Blaze.Append h1 h2) = go i attrs h1 . go i attrs h2
    go _ _ (Blaze.Empty _) = id

    -- Increase the indentation
    inc = (+) 2

    -- Produce appending indentation
    ind i = (replicate i ' ' ++)

-- forked from Text.Blaze.Renderer.String
renderHtmlCompact :: Blaze.MarkupM b -> String -> String
renderHtmlCompact = go id
  where
    go :: (String -> String) -> Blaze.MarkupM b -> String -> String
    go attrs (Blaze.Parent _ open close content) =
        Blaze.getString open . attrs . ('>' :) . go id content . Blaze.getString close
    go attrs (Blaze.CustomParent tag content) =
        ('<' :) . Blaze.fromChoiceString tag . attrs . ('>' :) .  go id content .
        ("</" ++) . Blaze.fromChoiceString tag . ('>' :)
    go attrs (Blaze.Leaf _ begin end _) = Blaze.getString begin . attrs . Blaze.getString end
    go attrs (Blaze.CustomLeaf tag close _) =
        ('<' :) . Blaze.fromChoiceString tag . attrs .
        (if close then (" />" ++) else ('>' :))
    go attrs (Blaze.AddAttribute _ key value h) = flip go h $
        Blaze.getString key . Blaze.fromChoiceString value . ('"' :) . attrs
    go attrs (Blaze.AddCustomAttribute key value h) = flip go h $
        (' ' :) . Blaze.fromChoiceString key . ("=\"" ++) . Blaze.fromChoiceString value .
        ('"' :) .  attrs
    go _ (Blaze.Content content _) = Blaze.fromChoiceString content
    go _ (Blaze.Comment comment _) =
        ("<!-- " ++) . Blaze.fromChoiceString comment . (" -->" ++)
    go attrs (Blaze.Append h1 h2) = go attrs h1 . go attrs h2
    go _ (Blaze.Empty _) = id

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

---  style  ---

makeStyles :: Path Abs Dir -> IO ()
makeStyles dir = writeFileLBS path (encodeUtf8 txt)
  where
    path = Path.toFilePath (dir Path.</> [relfile|style/jarclasses.css|])
    txt = Clay.renderWith Clay.pretty [] jarclassesStyle

jarclassesStyle :: Css
jarclassesStyle =
  do
    Clay.p ? do
        marginAll (Clay.px 0)
        paddingAll (Clay.px 0)
    Clay.body ? Clay.background (Clay.rgb 0xec 0xe4 0xd8)
    Clay.main_ ? do
        marginVertical (Clay.px 80)
        (Clay.p <> listTags <> headerTags) <? do
            Clay.maxWidth (Clay.px 506)
            marginHorizontal Clay.auto
        (Clay.p <> listTags) <? do
            Clay.color (Clay.rgb 0x54 0x49 0x43)
            Clay.fontSize (Clay.px 15)
            Clay.lineHeight (Clay.px 22.5)
            Clay.fontFamily ["Georgia", "Palatino", "Palatino Linotype", "Times", "Times New Roman"] [Clay.serif]
            foldMap1 (\cls -> Clay.span Clay.# Clay.byClass cls) ("emphatic" :| "title" : []) ? Clay.fontStyle Clay.italic
            Clay.a ? do
                Clay.textDecoration Clay.none
                Clay.color (Clay.rgb 0x41 0x70 0x90)
                Clay.hover Clay.& Clay.textDecoration Clay.underline
        Clay.p <? marginVertical (Clay.em 0.7)
        headerTags <? do
            Clay.fontFamily ["Open Sans", "Myriad", "Calibri"] [Clay.sansSerif]
            Clay.fontWeight Clay.bold
        (Clay.h1 <> Clay.h2) <? do
            Clay.color (Clay.rgb 0x7c 0x33 0x4f)
            Clay.borderBottomColor (Clay.rgb 0xd3 0xcc 0xc1)
            Clay.lineHeight (Clay.unitless 1.2)
            Clay.paddingBottom (Clay.px 10)
        Clay.h1 <? do
            Clay.fontSize (Clay.em 1.85)
            Clay.fontStyle Clay.italic
            Clay.textAlign Clay.center
            Clay.marginTop (Clay.em 1.1)
            Clay.marginBottom (Clay.em 0.38)
            Clay.borderBottomStyle Clay.double
            Clay.borderBottomWidth (Clay.em 0.2)
        Clay.h2 <? do
            Clay.fontSize (Clay.em 1.58)
            Clay.marginTop (Clay.em 0.95)
            Clay.marginBottom (Clay.em 0.5)
            Clay.borderBottomStyle Clay.solid
            Clay.borderBottomWidth (Clay.em 0.1)
  where
    marginAll = marginVertical <> marginHorizontal
    marginVertical = Clay.marginTop <> Clay.marginBottom
    marginHorizontal = Clay.marginLeft <> Clay.marginRight
    paddingAll = paddingVertical <> paddingHorizontal
    paddingVertical = Clay.paddingTop <> Clay.paddingBottom
    paddingHorizontal = Clay.paddingLeft <> Clay.paddingRight
    headerTags = Clay.h1 <> Clay.h2 <> Clay.h3 <> Clay.h4 <> Clay.h5 <> Clay.h6
    listTags = Clay.ul <> Clay.ol

---  tests  ---

writeTestFile :: Path Abs Dir -> IO ()
writeTestFile dir = writeFileLBS path (encodeUtf8 txt)
  where
    path = Path.toFilePath (dir Path.</> [relfile|test.txt|])
    txt = unlines tests

tests :: [Text]
tests = resourceFileNameTests

---  string building  ---

a <!> b = a <> " " <> b

quo x = "\"" <> x <> "\""
