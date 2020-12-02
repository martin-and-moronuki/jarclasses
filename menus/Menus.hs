module Menus where

import Chronology
import Control.Lens
import FileLayout
import qualified Pipes.Prelude as Pipes
import Relude hiding (head)
import Resource
import Text.Blaze.Html (Html, (!))
import qualified Text.Blaze.Html5 as HTML
import Title

listOfContent :: Scheme -> IO Html
listOfContent scheme = fmap displayContent $ getContent scheme

data Content = Content Resource (Maybe Html)

getContent :: Scheme -> IO [Content]
getContent scheme = Pipes.toListM (findRecentResources scheme ([res|menus|] `Resource.isPrefixOf`)) >>= traverse (resourceContent scheme)

resourceContent :: Scheme -> Resource -> IO Content
resourceContent scheme r = Content r <$> resourceTitleHtml scheme r

displayContent :: [Content] -> Html
displayContent rs = HTML.ul $ traverse_ displayOne rs

displayOne :: Content -> Html
displayOne (Content r title) = HTML.li $ HTML.p $ HTML.a ! resourceHref r $ fromMaybe (HTML.stringComment "no title") title
