{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module ProsidyHtml where

import qualified Prosidy
import Relude hiding (head)
import Text.Blaze.Html (Html, toHtml, toValue, (!))
import qualified Text.Blaze.Html5 as HTML
import qualified Text.Blaze.Html5.Attributes as Attr
import qualified Text.Blaze.Internal as Blaze
import qualified Text.Blaze.Renderer.String as Blaze
import Control.Lens

proHtml :: Prosidy.Document -> Html
proHtml doc = HTML.docTypeHtml ! Attr.lang "en" $ head <> body
  where
    head = HTML.head $ contentType <> title <> css
    contentType = HTML.meta ! Attr.httpEquiv "Content-Type" ! Attr.content "text/html; charset=utf-8"
    css = HTML.link ! Attr.rel "stylesheet" ! Attr.type_ "text/css" ! Attr.href "/style/jarclasses.css"
    title = foldMap (HTML.title . toHtml) $ proTitle doc
    body = HTML.body content
    content = HTML.main $ do
      foldMap (HTML.h1 . HTML.p . toHtml) $ proTitle doc
      foldMap proBlockHtml $ view Prosidy.content doc

proTitle :: Prosidy.Document -> Maybe Text
proTitle = view (Prosidy.atSetting "title")

proBlockHtml :: Prosidy.Block -> Html
proBlockHtml = \case
  Prosidy.BlockLiteral x -> HTML.stringComment (show x)
  Prosidy.BlockParagraph x -> HTML.p (foldMap proInlineHtml (view Prosidy.content x))
  Prosidy.BlockTag x -> case (Prosidy.tagName x) of
    "day" -> HTML.h2 (foldMap proBlockHtml (view Prosidy.content x))
    "list" -> proListHtml x
    _ -> HTML.stringComment (show x)

proInlineHtml :: Prosidy.Inline -> Html
proInlineHtml = \case
  Prosidy.Break -> toHtml (" " :: String)
  Prosidy.InlineText x -> toHtml (Prosidy.fragmentText x)
  Prosidy.InlineTag x -> case (Prosidy.tagName x) of
    "dash" -> HTML.preEscapedToHtml ("&mdash;" :: Text)
    "emphatic" -> HTML.span ! Attr.class_ "emphatic" $ foldMap proInlineHtml (view Prosidy.content x)
    "title" -> HTML.span ! Attr.class_ "title" $ foldMap proInlineHtml (view Prosidy.content x)
    "link" -> HTML.a ! (maybe mempty (Attr.href . toValue) $ view (Prosidy.atSetting "to") x) $ foldMap proInlineHtml (view Prosidy.content x)
    _ -> HTML.stringComment (show x)

proListHtml :: Prosidy.Tag (Prosidy.Series Prosidy.Block) -> Html
proListHtml x = HTML.ul $ foldMap itemHtml (view Prosidy.content x)
  where
    itemHtml = \case
      Prosidy.BlockTag i | Prosidy.tagName i == "item" -> HTML.li $ foldMap proBlockHtml (view Prosidy.content i)

---  html rendering  ---

renderHtml :: Html -> String
renderHtml html = renderHtmlIndented html ""

-- forked from Text.Blaze.Renderer.Pretty
renderHtmlIndented :: Blaze.MarkupM b -> String -> String
renderHtmlIndented = go 0 id
  where
    contentInline :: Blaze.StaticString -> Bool
    contentInline open = Blaze.getString open "" `elem` ["<p", "<title"]

    go :: Int -> (String -> String) -> Blaze.MarkupM b -> String -> String

    go i attrs (Blaze.Parent _ open close content)
      | contentInline open =
        ind i . Blaze.getString open . attrs . (">" ++) . renderHtmlCompact content . Blaze.getString close . ('\n' :)
    go i attrs (Blaze.Parent _ open close content) =
      ind i . Blaze.getString open . attrs . (">\n" ++) . go (inc i) id content
        . ind i
        . Blaze.getString close
        . ('\n' :)
    go i attrs (Blaze.CustomParent tag content) =
      ind i . ('<' :) . Blaze.fromChoiceString tag . attrs . (">\n" ++)
        . go (inc i) id content
        . ind i
        . ("</" ++)
        . Blaze.fromChoiceString tag
        . (">\n" ++)
    go i attrs (Blaze.Leaf _ begin end _) =
      ind i . Blaze.getString begin . attrs . Blaze.getString end . ('\n' :)
    go i attrs (Blaze.CustomLeaf tag close _) =
      ind i . ('<' :) . Blaze.fromChoiceString tag . attrs
        . ((if close then " />\n" else ">\n") ++)
    go i attrs (Blaze.AddAttribute _ key value h) =
      flip (go i) h $
        Blaze.getString key . Blaze.fromChoiceString value . ('"' :) . attrs
    go i attrs (Blaze.AddCustomAttribute key value h) =
      flip (go i) h $
        (' ' :) . Blaze.fromChoiceString key . ("=\"" ++) . Blaze.fromChoiceString value
          . ('"' :)
          . attrs
    go i _ (Blaze.Content content _) = ind i . Blaze.fromChoiceString content . ('\n' :)
    go i _ (Blaze.Comment comment _) =
      ind i
        . ("<!-- " ++)
        . Blaze.fromChoiceString comment
        . (" -->\n" ++)
    go i attrs (Blaze.Append h1 h2) = go i attrs h1 . go i attrs h2
    go _ _ (Blaze.Empty _) = id

    -- Increase the indentation
    inc = (+) 2

    -- Produce appending indentation
    ind i = (replicate i ' ' ++)

-- forked from Text.Blaze.Renderer.String
renderHtmlCompact :: Blaze.MarkupM b -> String -> String
renderHtmlCompact = go id
  where
    go :: (String -> String) -> Blaze.MarkupM b -> String -> String
    go attrs (Blaze.Parent _ open close content) =
      Blaze.getString open . attrs . ('>' :) . go id content . Blaze.getString close
    go attrs (Blaze.CustomParent tag content) =
      ('<' :) . Blaze.fromChoiceString tag . attrs . ('>' :) . go id content
        . ("</" ++)
        . Blaze.fromChoiceString tag
        . ('>' :)
    go attrs (Blaze.Leaf _ begin end _) = Blaze.getString begin . attrs . Blaze.getString end
    go attrs (Blaze.CustomLeaf tag close _) =
      ('<' :) . Blaze.fromChoiceString tag . attrs
        . (if close then (" />" ++) else ('>' :))
    go attrs (Blaze.AddAttribute _ key value h) =
      flip go h $
        Blaze.getString key . Blaze.fromChoiceString value . ('"' :) . attrs
    go attrs (Blaze.AddCustomAttribute key value h) =
      flip go h $
        (' ' :) . Blaze.fromChoiceString key . ("=\"" ++) . Blaze.fromChoiceString value
          . ('"' :)
          . attrs
    go _ (Blaze.Content content _) = Blaze.fromChoiceString content
    go _ (Blaze.Comment comment _) =
      ("<!-- " ++) . Blaze.fromChoiceString comment . (" -->" ++)
    go attrs (Blaze.Append h1 h2) = go attrs h1 . go attrs h2
    go _ (Blaze.Empty _) = id