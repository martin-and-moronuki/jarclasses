module Menus where

import Chronology
import FileLayout
import qualified HtmlBuilding as H
import qualified HtmlTypes as H
import Pipes (ListT)
import qualified Pipes as Pipes
import qualified Pipes.Prelude as Pipes
import Progress
import qualified Prosidy
import ProsidyHtml (BlockTagOpt (..), ProHtmlOpts, addBlockTag)
import Relude hiding (head)
import Resource
import Title

proHtmlOpts :: Scheme -> Endo (ProHtmlOpts IO)
proHtmlOpts s = addBlockTag (listOfContentTag s)

listOfContentTag :: Scheme -> BlockTagOpt IO
listOfContentTag s =
  BlockTagOpt $ \_ x ->
    case (Prosidy.tagName x) of
      "list-of-content" -> Just $ listOfContent s
      _ -> Nothing

listOfContent :: Scheme -> IO (H.Series H.Block)
listOfContent scheme = displayContent $ getContent scheme

data Content = Content {contentResource :: Resource, contentTitle :: Maybe (H.Series H.Inline), contentProgress :: Maybe Progress}

getContent :: Scheme -> ListT IO Content
getContent scheme =
  do
    r <- findRecentResources scheme ([res|menus|] `Resource.isPrefixOf`)
    c <- lift $ resourceContent scheme r
    guard $ contentProgress c == Just Published
    return c

resourceContent :: Scheme -> Resource -> IO Content
resourceContent scheme r = Content r <$> resourceTitleHtml scheme r <*> resourceProgress scheme r

displayContent :: Monad m => ListT m Content -> m (H.Series H.Block)
displayContent = Pipes.fold (<>) mempty (H.toBlocks . H.BulletedList) . Pipes.enumerate . fmap (one . displayOne)

displayOne :: Content -> H.ListItem
displayOne Content {contentResource, contentTitle} =
  H.ListItem $ H.toBlocks $ H.Paragraph {H.paragraphClasses, H.paragraphContent}
  where
    paragraphClasses = mempty
    paragraphContent = H.toInlines H.Anchor {H.anchorName, H.anchorHref, H.anchorContent}
    anchorName = Nothing
    anchorHref = Just $ resourceUrl contentResource
    anchorContent = fromMaybe (H.toInlines $ H.Comment "no title") contentTitle
