{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module ProsidyHtml where

import Control.Lens
import Prosidy
import Relude hiding (head)
import Text.Blaze.Html (Html, toHtml, toValue, (!))
import qualified Text.Blaze.Html5 as HTML
import qualified Text.Blaze.Html5.Attributes as Attr

proHtml :: Document -> Html
proHtml doc = HTML.docTypeHtml ! Attr.lang "en" $ head <> body
  where
    head = HTML.head $ contentType <> title <> css
    contentType = HTML.meta ! Attr.httpEquiv "Content-Type" ! Attr.content "text/html; charset=utf-8"
    css = HTML.link ! Attr.rel "stylesheet" ! Attr.type_ "text/css" ! Attr.href "/style/jarclasses.css"
    title = foldMap (HTML.title . toHtml) $ proTitle doc
    body = HTML.body main
    main = HTML.main $ do
      foldMap (HTML.h1 . HTML.p . toHtml) $ proTitle doc
      foldMap proBlockHtml $ view content doc

proTitle :: Document -> Maybe Text
proTitle = view (atSetting "title")

proBlockHtml :: Block -> Html
proBlockHtml = \case
  BlockLiteral x -> HTML.stringComment (show x)
  BlockParagraph x -> HTML.p (foldMap proInlineHtml (view content x))
  BlockTag x -> proBlockTagHtml x

proBlockTagHtml :: Tag (Series Block) -> Html
proBlockTagHtml x = case (tagName x) of
  "day" -> HTML.h2 (foldMap proBlockHtml (view content x))
  "list" -> proListHtml x
  _ -> HTML.stringComment (show x)

proInlineHtml :: Inline -> Html
proInlineHtml = \case
  Break -> toHtml (" " :: String)
  InlineText x -> toHtml (fragmentText x)
  InlineTag x -> proInlineTagHtml x

proInlineTagHtml :: Tag (Series Inline) -> Html
proInlineTagHtml x = case (tagName x) of
  "dash" -> HTML.preEscapedToHtml ("&mdash;" :: Text)
  "emphatic" -> HTML.span ! Attr.class_ "emphatic" $ foldMap proInlineHtml (view content x)
  "title" -> HTML.span ! Attr.class_ "title" $ foldMap proInlineHtml (view content x)
  "link" -> HTML.a ! (maybe mempty (Attr.href . toValue) $ view (atSetting "to") x) $ foldMap proInlineHtml (view content x)
  _ -> HTML.stringComment (show x)

proListHtml :: Tag (Series Block) -> Html
proListHtml x = HTML.ul $ foldMap itemHtml (view content x)
  where
    itemHtml = \case
      BlockTag i | tagName i == "item" -> HTML.li $ foldMap proBlockHtml (view content i)
