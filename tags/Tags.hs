module Tags where

import Chronology
import Control.Lens
import qualified Data.Char as Char
import qualified Data.HashMap.Strict as HashMap
import Data.Time
import FileLayout
import qualified HtmlBuilding as H
import qualified HtmlTypes as H
import Path
import qualified Pipes as Pipes
import qualified Pipes.Prelude as Pipes
import Progress ()
import qualified Prosidy
import ProsidyHtml (BlockTagOpt (..), ProHtmlOpts, addBlockTag)
import qualified ProsidyHtml
import Relude hiding (head)
import Resource
import ResourceEnumeration
import qualified Text.Regex.Applicative as RE
import Title (prhTitleHtml)

proHtmlOpts :: Scheme -> Endo (ProHtmlOpts IO)
proHtmlOpts s =
  addBlockTag (listOfTagsTag s)

optsForTopicPage :: Scheme -> Tag -> Endo (ProHtmlOpts IO)
optsForTopicPage s t =
  addBlockTag synopsisTag
    <> addBlockTag (topicPageContent s t)

topicPageContent :: Scheme -> Tag -> BlockTagOpt IO
topicPageContent s t = BlockTagOpt $ \_ x ->
  case (Prosidy.tagName x) of
    "list-of-content" -> Just $ getListOfContentForTopic s t
    _ -> Nothing

getListOfContentForTopic :: Scheme -> Tag -> IO (H.Series H.Block)
getListOfContentForTopic s t =
  do
    tm <- getTagMap s
    let xs = fromMaybe mempty $ HashMap.lookup t tm
    return $ taggedsHtml xs

synopsisTag :: Monad m => BlockTagOpt m
synopsisTag = BlockTagOpt $ \o x ->
  case (Prosidy.tagName x) of
    "synopsis" -> Just $ ProsidyHtml.foldMapSequence (ProsidyHtml.proBlockHtml o) (view Prosidy.content x)
    _ -> Nothing

listOfTagsTag :: Scheme -> BlockTagOpt IO
listOfTagsTag s = BlockTagOpt $ \_ x ->
  case (Prosidy.tagName x) of
    "list-of-tags" -> Just $ Tags.listOfTags s
    _ -> Nothing

listOfTags :: Scheme -> IO (H.Series H.Block)
listOfTags scheme = fmap tagMapHtml $ getTagMap scheme

type TagMap = HashMap Tag (Seq Tagged)

data Tagged = Tagged {taggedResource :: Resource, taggedDay :: Day, taggedTitleHtml :: H.Series H.Inline}

tagMapHtml :: TagMap -> H.Series H.Block
tagMapHtml = foldMap f . sortBy (compare `on` (tagText . fst)) . HashMap.toList
  where
    f :: Foldable series => (Tag, series Tagged) -> H.Series H.Block
    f (t, rs) =
      (H.toBlocks $ H.H H.H2 $ H.toInlines $ tagText t)
        <> taggedsHtml rs

taggedsHtml :: Foldable series => series Tagged -> H.Series H.Block
taggedsHtml rs = foldMap g (sortBy (compare `on` taggedDay) (toList rs))
  where
    g :: Tagged -> H.Series H.Block
    g Tagged {taggedTitleHtml, taggedResource} =
      H.toBlocks H.Paragraph {H.paragraphContent, H.paragraphClasses}
      where
        paragraphClasses = mempty
        paragraphContent = H.toInlines $ H.Anchor {H.anchorContent, H.anchorHref, H.anchorName}
          where
            anchorContent = taggedTitleHtml
            anchorHref = Just (resourceUrl taggedResource)
            anchorName = Nothing

getTagMap :: Scheme -> IO TagMap
getTagMap scheme =
  Pipes.fold (HashMap.unionWith (<>)) mempty id $
    Pipes.enumerate $
      do
        phr@(ProHtmlResource taggedResource _ _ _) <- findProHtmlResources scheme
        tags <- lift $ resourceTags scheme taggedResource
        t <- Pipes.Select $ Pipes.each tags
        taggedTitleHtml <- Pipes.Select . Pipes.each =<< lift (prhTitleHtml phr)
        taggedDay <- Pipes.Select . Pipes.each =<< lift (proHtmlResourceDay phr)
        return $ one (t, one Tagged {taggedTitleHtml, taggedResource, taggedDay})

newtype Tag = Tag {tagText :: Text}
  deriving stock (Eq)
  deriving stock (Generic)
  deriving anyclass (Hashable)

resourceTags :: Scheme -> Resource -> IO (Seq Tag)
resourceTags s r =
  case resourceInputPath s r of
    Nothing -> return mempty
    Just p ->
      do
        docMay <- inputPathDoc p
        case docMay of
          Nothing -> return mempty
          Just doc ->
            case view (Prosidy.atSetting "tags") doc of
              Nothing -> return mempty
              Just tagsText -> return (fromMaybe mempty $ parseTagsText tagsText)

parseTagsText :: Text -> Maybe (Seq Tag)
parseTagsText = RE.match e . toString
  where
    e :: RE.RE Char (Seq Tag)
    e = s *> (fmap one t <> fmap fromList (RE.many (sep *> t))) <* s

    sep :: RE.RE Char ()
    sep = void $ s *> RE.sym ',' *> s

    t :: RE.RE Char Tag
    t = fmap (Tag . unwords) $ pure (:) <*> w <*> RE.many (s *> w)

    w :: RE.RE Char Text
    w = fmap toText $ RE.some (RE.psym $ \c -> c /= ',' && not (Char.isSpace c))

    s :: RE.RE Char ()
    s = void $ many (RE.psym Char.isSpace)

inputPathDoc :: InputPath -> IO (Maybe Prosidy.Document)
inputPathDoc (InputPath (toFilePath -> x)) = fmap (bsDoc x) (readFileBS x)

bsDoc :: FilePath -> ByteString -> Maybe Prosidy.Document
bsDoc fp = either (const Nothing) Just . Prosidy.parseDocument fp . decodeUtf8
