module Home where

import Control.Lens
import Prosidy
import Relude hiding (head)
import Text.Blaze.Html (Html, toHtml, toValue, (!))
import qualified Text.Blaze.Html5 as HTML
import qualified Text.Blaze.Html5.Attributes as Attr

listOfContent :: Html
listOfContent = HTML.stringComment "hey!"
