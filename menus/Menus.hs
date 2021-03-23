module Menus where

import Chronology
import Control.Lens
import FileLayout
import qualified HtmlBuilding as H
import qualified HtmlTypes as H
import qualified Pipes.Prelude as Pipes
import Relude hiding (head)
import Resource
import Title

listOfContent :: Scheme -> IO (H.Series H.Block)
listOfContent scheme = fmap displayContent $ getContent scheme

data Content = Content {contentResource :: Resource, contentTitle :: Maybe (H.Series H.Inline)}

getContent :: Scheme -> IO [Content]
getContent scheme = Pipes.toListM (findRecentResources scheme ([res|menus|] `Resource.isPrefixOf`)) >>= traverse (resourceContent scheme)

resourceContent :: Scheme -> Resource -> IO Content
resourceContent scheme r = Content r <$> resourceTitleHtml scheme r

displayContent :: [Content] -> H.Series H.Block
displayContent = H.toBlocks . H.BulletedList . foldMap (one . displayOne)

displayOne :: Content -> H.ListItem
displayOne Content {contentResource, contentTitle} =
  H.ListItem $ H.toBlocks $ H.Paragraph {H.paragraphClasses, H.paragraphContent}
  where
    paragraphClasses = mempty
    paragraphContent = H.toInlines H.Anchor {H.anchorName, H.anchorHref, H.anchorContent}
    anchorName = Nothing
    anchorHref = Just $ resourceUrl contentResource
    anchorContent = fromMaybe (H.toInlines $ H.Comment "no title") contentTitle
