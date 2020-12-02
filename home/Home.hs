module Home where

import Chronology
import Control.Lens
import FileLayout
import qualified Pipes.Prelude as Pipes
import Prosidy
import Relude hiding (head)
import Resource
import Text.Blaze.Html (Html, toHtml, toValue, (!))
import qualified Text.Blaze.Html5 as HTML
import qualified Text.Blaze.Html5.Attributes as Attr

listOfContent :: Scheme -> IO Html
listOfContent scheme =
  do
    rs <- Pipes.toListM (findRecentResources scheme)
    pure $
      HTML.ul $
        for_ rs \r ->
          HTML.li $ HTML.a ! resourceHref r $ HTML.toHtml (show r :: Text)

resourceHref :: Resource -> HTML.Attribute
resourceHref = Attr.href . toValue . foldMap ("/" <>) . (\(ResourceSlashList x) -> x)
