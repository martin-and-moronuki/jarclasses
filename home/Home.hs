module Home where

import Chronology
import Control.Lens
import Data.Time
import FileLayout
import qualified HtmlBuilding as H
import qualified HtmlTypes as H
import qualified Pipes.Prelude as Pipes
import Relude hiding (head)
import Resource
import Title

listOfContent :: Scheme -> IO (Maybe Natural -> H.Series H.Block)
listOfContent scheme = fmap (\xs n -> displayContent $ maybe id genericTake n xs) $ getContent scheme

data Content = Content {contentResource :: Resource, contentTitle :: Maybe (H.Series H.Inline), contentDay :: Maybe Day}

getContent :: Scheme -> IO [Content]
getContent scheme = Pipes.toListM (findRecentResources scheme (const True)) >>= traverse (resourceContent scheme)

resourceContent :: Scheme -> Resource -> IO Content
resourceContent scheme r = Content r <$> resourceTitleHtml scheme r <*> resourceDay scheme r

displayContent :: [Content] -> H.Series H.Block
displayContent = H.toBlocks . H.BulletedList . foldMap (one . displayOne)

displayOne :: Content -> H.ListItem
displayOne Content {contentResource, contentTitle, contentDay} =
  H.ListItem $ H.toBlocks $ H.Paragraph {H.paragraphClasses, H.paragraphContent}
  where
    paragraphClasses = mempty
    paragraphContent = dayHtml <> anchorHtml
      where
        dayHtml = maybe (H.toInlines $ H.Comment "no day") daySpan contentDay
          where
            daySpan x = H.toInlines H.Span {H.spanClasses, H.spanStyles, H.spanContent}
              where
                spanClasses = one "day"
                spanStyles = mempty
                spanContent = H.toInlines (show x :: Text)

        anchorHtml = maybe (H.toInlines $ H.Comment "no title") titleAnchor contentTitle
          where
            titleAnchor x = H.toInlines H.Anchor {H.anchorName, H.anchorHref, H.anchorContent}
              where
                anchorName = Nothing
                anchorHref = Just (resourceUrl contentResource)
                anchorContent = x
