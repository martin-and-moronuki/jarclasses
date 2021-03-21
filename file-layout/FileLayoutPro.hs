module FileLayoutPro where

import Control.Lens
import qualified Data.Map.Strict as Map
import FileLayout
import Path
import Prosidy
import Relude
import Resource
import StringBuilding

layoutProPath :: Path Rel File
layoutProPath = [relfile|file-layout/file-layout.pro|]

bsDoc :: ByteString -> IO Prosidy.Document
bsDoc =
  either (fail . show) pure
    . Prosidy.parseDocument (toFilePath layoutProPath)
    . decodeUtf8

getScheme :: IO Scheme
getScheme =
  do
    src <- readFileBS (toFilePath layoutProPath)
    doc <- bsDoc src
    case proDocScheme doc of
      ([], x) -> pure x
      (errors, _) -> failErrors errors

failErrors :: [Text] -> IO a
failErrors =
  fail . toString . unlines . (("Errors in" <!> show layoutProPath) :) . map (" ● " <>)

proDocScheme :: Prosidy.Document -> ([Text], Scheme)
proDocScheme = foldMap proBlockScheme . view content

proBlockScheme :: Block -> ([Text], Scheme)
proBlockScheme = \case
  BlockLiteral _ -> mempty
  BlockParagraph x -> foldMap proInlineScheme (view content x)
  BlockTag x -> proBlockTagScheme x

proBlockTagScheme :: Tag (Series Block) -> ([Text], Scheme)
proBlockTagScheme x =
  case (tagName x) of
    "page" ->
      case blockPage x of
        Just p -> pure $ mempty {scheme_otherProHtmlResources = one p}
        Nothing -> err x
    _ -> mempty

proInlineScheme :: Inline -> ([Text], Scheme)
proInlineScheme =
  \case
    Break -> mempty
    InlineText _ -> mempty
    InlineTag x -> proInlineTagScheme x

proInlineTagScheme :: Tag (Series Inline) -> ([Text], Scheme)
proInlineTagScheme x =
  case (tagName x) of
    "contentDir" ->
      case inlineDir x of
        Just p -> pure $ mempty {scheme_proHtmlDirs = one p}
        Nothing -> err x
    "styleDir" ->
      case inlineDir x of
        Just p -> pure $ mempty {scheme_styleDirs = one p}
        Nothing -> err x
    "rss" ->
      case inlineFile x of
        Just f -> pure $ mempty {scheme_rssLocation = one f}
        Nothing -> err x
    _ -> foldMap proInlineScheme (view content x)

inlineDir :: Tag (Series Inline) -> Maybe (Path Rel Dir)
inlineDir x =
  case toList (view content x) of
    [InlineText t] -> parseRelDir (toString (fragmentText t))
    _ -> Nothing

inlineFile :: Tag (Series Inline) -> Maybe (Path Rel File)
inlineFile x =
  case toList (view content x) of
    [InlineText t] -> parseRelFile (toString (fragmentText t))
    _ -> Nothing

blockPage :: Tag (Series Block) -> Maybe ProHtmlResource
blockPage x =
  case (foldMap blockMap (view content x)) of
    TagMap (Just m) ->
      do
        r <- Map.lookup "url" m >>= urlResource
        fpIn <- Map.lookup "input" m >>= (parseRelFile . toString) >>= pure . InputPath
        fpOut <- Map.lookup "output" m >>= (parseRelFile . toString) >>= pure . OutputPath
        fpDeploy <- Map.lookup "deploy" m >>= (parseRelFile . toString) >>= pure . DeployPath
        Just $ ProHtmlResource r fpIn fpOut fpDeploy
    _ -> Nothing

urlResource :: Text -> Maybe Resource
urlResource = fmap ResourceSlashList . r . toString
  where
    r ['/'] = Just []
    r ('/' : x) =
      case break (== '/') x of
        (y, []) -> Just [toText y]
        (y, z) -> fmap (toText y :) $ r z
    r _ = Nothing

newtype TagMap = TagMap (Maybe (Map Key Text))

instance Semigroup TagMap where
  TagMap (Just x) <> TagMap (Just y)
    | Map.disjoint x y = TagMap (Just (Map.union x y))
  _ <> _ = TagMap Nothing

instance Monoid TagMap where
  mempty = TagMap (Just mempty)

tagMapFail :: TagMap
tagMapFail = TagMap Nothing

tagMapSingleton :: Key -> Text -> TagMap
tagMapSingleton k v = TagMap (Just (Map.singleton k v))

blockMap :: Block -> TagMap
blockMap = \case
  BlockLiteral _ -> mempty
  BlockParagraph x -> foldMap inlineMap (view content x)
  BlockTag x -> foldMap blockMap (view content x)

inlineMap :: Inline -> TagMap
inlineMap = \case
  Break -> mempty
  InlineText _ -> mempty
  InlineTag x ->
    case toList (view content x) of
      [InlineText t] -> tagMapSingleton (tagName x) (fragmentText t)
      _ -> tagMapFail

err :: (Monoid b, Show a) => a -> ([Text], b)
err x = ([show x], mempty)
