module ProsidyHtml where

import Control.Lens
import Home
import Prosidy
import Relude hiding (head)
import Text.Blaze.Html (Html, toHtml, toValue, (!))
import qualified Text.Blaze.Html5 as HTML
import qualified Text.Blaze.Html5.Attributes as Attr

proHtml :: Either Prosidy.Failure Document -> Html
proHtml doc = HTML.docTypeHtml ! Attr.lang "en" $ head <> body
  where
    head = HTML.head $ contentType <> title <> css
    contentType =
      HTML.meta
        ! Attr.httpEquiv "Content-Type"
        ! Attr.content "text/html; charset=utf-8"
    css =
      HTML.link
        ! Attr.rel "stylesheet"
        ! Attr.type_ "text/css"
        ! Attr.href "/style/jarclasses.css"
    title = foldMap (HTML.title . toHtml) $ proTitle doc
    body = HTML.body main
    main = HTML.main $ do
      foldMap (HTML.h1 . toHtml) $ proTitle doc
      either (HTML.stringComment . show) (foldMap proBlockHtml . view content) doc

proTitle :: Either Prosidy.Failure Document -> Maybe Text
proTitle = either (const Nothing) (view (atSetting "title"))

proBlockHtml :: Block -> Html
proBlockHtml =
  \case
    BlockLiteral x -> HTML.stringComment (show x)
    BlockParagraph x -> HTML.p (foldMap proInlineHtml (view content x))
    BlockTag x -> proBlockTagHtml x

proBlockTagHtml :: Tag (Series Block) -> Html
proBlockTagHtml x =
  case (tagName x) of
    "day" ->
      HTML.h2 $
        foldMap
          \case
            BlockParagraph y ->
              foldMap proInlineHtml (view content y)
            y -> HTML.stringComment (show y)
          (view content x)
    "list" -> proListHtml x
    "list-of-content-on-the-home-page" -> Home.listOfContent
    _ -> HTML.stringComment (show x)

proInlineHtml :: Inline -> Html
proInlineHtml =
  \case
    Break -> toHtml (" " :: String)
    InlineText x -> toHtml (fragmentText x)
    InlineTag x -> proInlineTagHtml x

proInlineTagHtml :: Tag (Series Inline) -> Html
proInlineTagHtml x =
  case (tagName x) of
    "dash" -> HTML.preEscapedToHtml ("&mdash;" :: Text)
    "emphatic" -> HTML.span ! Attr.class_ "emphatic" $ foldMap proInlineHtml (view content x)
    "title" -> HTML.span ! Attr.class_ "title" $ foldMap proInlineHtml (view content x)
    "link" -> HTML.a ! (maybe mempty (Attr.href . toValue) $ view (atSetting "to") x) $ foldMap proInlineHtml (view content x)
    _ -> HTML.stringComment (show x)

proListHtml :: Tag (Series Block) -> Html
proListHtml x = HTML.ul $ foldMap itemHtml (view content x)
  where
    itemHtml =
      \case
        BlockTag i
          | tagName i == "item" ->
            HTML.li $ foldMap proBlockHtml (view content i)
        y -> HTML.stringComment (show y)
