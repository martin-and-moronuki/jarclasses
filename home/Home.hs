module Home where

import Chronology
import Control.Lens (view)
import Data.Time
import FileLayout
import qualified HtmlBuilding as H
import qualified HtmlTypes as H
import Pipes (ListT, (>->))
import qualified Pipes as Pipes
import qualified Pipes.Prelude as Pipes
import Progress
import qualified Prosidy
import ProsidyHtml (BlockTagOpt (..), ProHtmlOpts, addBlockTag, addStyle)
import Relude hiding (head)
import Resource
import StringBuilding
import Title

proHtmlOpts :: Scheme -> Endo (ProHtmlOpts IO)
proHtmlOpts s =
  addBlockTag (listOfContentTag s)
    <> addStyle [res|style/home.css|]

listOfContentTag :: Scheme -> BlockTagOpt IO
listOfContentTag s = BlockTagOpt $ \_ x ->
  case (Prosidy.tagName x) of
    "list-of-content" -> Just $
      case view (Prosidy.atSetting "limit") x of
        Nothing -> listOfContent s Nothing
        Just t ->
          case readMaybe @Natural (toString t) of
            Nothing -> pure $ H.toBlocks $ H.Comment $ "limit must be a Natural, but is" <!> show t
            Just n -> listOfContent s (Just n)
    _ -> Nothing

listOfContent :: Scheme -> Maybe Natural -> IO (H.Series H.Block)
listOfContent scheme nMay = displayContent $ limit $ getContent scheme
  where
    limit =
      case nMay >>= toIntegralSized of
        Nothing -> id
        Just n -> Pipes.Select . (>-> Pipes.take n) . Pipes.enumerate

data Content = Content {contentResource :: Resource, contentTitle :: Maybe (H.Series H.Inline), contentDay :: Maybe Day, contentProgress :: Maybe Progress}

getContent :: Scheme -> ListT IO Content
getContent scheme =
  do
    r <- findRecentResources scheme (const True)
    c <- lift $ resourceContent scheme r
    guard $ contentProgress c == Just Published
    return c

resourceContent :: Scheme -> Resource -> IO Content
resourceContent scheme r = Content r <$> resourceTitleHtml scheme r <*> resourceDay scheme r <*> resourceProgress scheme r

displayContent :: Monad m => ListT m Content -> m (H.Series H.Block)
displayContent = Pipes.fold (<>) mempty (H.toBlocks . H.BulletedList) . Pipes.enumerate . fmap (one . displayOne)

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
