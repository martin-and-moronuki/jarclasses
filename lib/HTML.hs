module HTML where

import Relude
import Text.Blaze.Html (Html, toValue, (!))
import qualified Text.Blaze.Html5 as HTML
import qualified Text.Blaze.Html5.Attributes as Attr

-- A magical meta tag that tells mobile devices that we have mobile-friendly CSS. It prevents phones from attempting to automatically scale the page.
viewportMeta :: Html
viewportMeta =
  HTML.meta
    ! Attr.name "viewport"
    ! Attr.content "width=device-width, initial-scale=1"

utf8htmlMeta :: Html
utf8htmlMeta =
  HTML.meta
    ! Attr.httpEquiv "Content-Type"
    ! Attr.content "text/html; charset=utf-8"

stylesheet :: Text -> Html
stylesheet x =
  HTML.link
    ! Attr.rel "stylesheet"
    ! Attr.type_ "text/css"
    ! Attr.href (toValue x)
