module Home where

import Chronology
import Control.Lens
import Data.Time
import FileLayout
import qualified Pipes.Prelude as Pipes
import Relude hiding (head)
import Resource
import Text.Blaze.Html (Html, toHtml, (!))
import qualified Text.Blaze.Html5 as HTML
import qualified Text.Blaze.Html5.Attributes as Attr
import Title

listOfContent :: Scheme -> IO Html
listOfContent scheme = fmap displayContent $ getContent scheme

data Content = Content Resource (Maybe Html) (Maybe Day)

getContent :: Scheme -> IO [Content]
getContent scheme = Pipes.toListM (findRecentResources scheme (const True)) >>= traverse (resourceContent scheme)

resourceContent :: Scheme -> Resource -> IO Content
resourceContent scheme r = Content r <$> resourceTitleHtml scheme r <*> resourceDay scheme r

displayContent :: [Content] -> Html
displayContent rs = HTML.ul $ traverse_ displayOne rs

displayOne :: Content -> Html
displayOne (Content r title day) =
  HTML.li $
    HTML.p $ do
      maybe (HTML.stringComment "no day") ((HTML.span ! Attr.class_ "day") . toHtml @Text . show) day
      HTML.a ! resourceHref r $ fromMaybe (HTML.stringComment "no title") title
