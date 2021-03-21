module ProsidyHtml where

import Control.Lens
import qualified HTML
import Prosidy
import Relude hiding (head)
import Resource
import Text.Blaze.Html (Html, toHtml, toValue, (!))
import qualified Text.Blaze.Html5 as HTML
import qualified Text.Blaze.Html5.Attributes as Attr

data ProHtmlOpts = ProHtmlOpts
  { extraBlockTags :: Tag (Series Block) -> Maybe Html,
    extraStyle :: [Resource]
  }

defaultOpts :: ProHtmlOpts
defaultOpts = ProHtmlOpts (const Nothing) []

proHtml :: ProHtmlOpts -> Either Prosidy.Failure Document -> Html
proHtml opts doc = HTML.docTypeHtml ! Attr.lang "en" $ head <> body
  where
    head = HTML.head $ HTML.utf8htmlMeta <> HTML.viewportMeta <> title <> css
    css = foldMap HTML.stylesheet (one [res|style/jarclasses.css|] ++ extraStyle opts)
    title = foldMap (HTML.title . toHtml) $ proTitle doc
    body = HTML.body main
    main = HTML.main $ do
      foldMap (HTML.h1 . toHtml) $ proTitle doc
      either (HTML.stringComment . ("\n" <>) . prettyFailure) (foldMap (proBlockHtml opts) . view content) doc

proTitle :: Either Prosidy.Failure Document -> Maybe Text
proTitle = either (const Nothing) (view (atSetting "title"))

proBlockHtml :: ProHtmlOpts -> Block -> Html
proBlockHtml opts =
  \case
    BlockLiteral x -> HTML.stringComment (show x)
    BlockParagraph x -> HTML.p (foldMap proInlineHtml (view content x))
    BlockTag x -> proBlockTagHtml opts x

proBlockTagHtml :: ProHtmlOpts -> Tag (Series Block) -> Html
proBlockTagHtml opts x =
  case (tagName x) of
    "day" -> h2 x
    "h2" -> h2 x
    "list" -> proListHtml opts x
    "quote" -> blockQuote opts x
    "blockquote" -> blockQuote opts x
    _ -> fromMaybe (HTML.stringComment (show x)) (extraBlockTags opts x)

h2 :: Tag (Series Block) -> Html
h2 = HTML.h2 . requireInlineOnly . view content

blockQuote :: ProHtmlOpts -> Tag (Series Block) -> Html
blockQuote opts = HTML.blockquote . foldMap (proBlockHtml opts) . view content

requireInlineOnly :: Series Block -> Html
requireInlineOnly =
  foldMap
    \case
      BlockParagraph y ->
        foldMap proInlineHtml (view content y)
      y -> HTML.stringComment (show y)

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
    "italic" -> HTML.span ! Attr.style "font-style: italic" $ foldMap proInlineHtml (view content x)
    "title" -> HTML.span ! Attr.class_ "title" $ foldMap proInlineHtml (view content x)
    "link" -> HTML.a ! (maybe mempty (Attr.href . toValue) $ view (atSetting "to") x) $ foldMap proInlineHtml (view content x)
    _ -> HTML.stringComment (show x)

proListHtml :: ProHtmlOpts -> Tag (Series Block) -> Html
proListHtml opts x
  | isHorizontal = HTML.p ! Attr.class_ "horizontalList" $ foldMap itemHtml (view content x)
  | otherwise = HTML.ul $ foldMap itemHtml (view content x)
  where
    isHorizontal = view (hasProperty "horizontal") x
    itemHtml =
      \case
        BlockTag i
          | tagName i == "item" ->
            if
                | isHorizontal -> HTML.span $ requireInlineOnly (view content i)
                | otherwise -> HTML.li $ foldMap (proBlockHtml opts) (view content i)
        BlockParagraph i -> HTML.li $ foldMap proInlineHtml (view content i)
        y -> HTML.stringComment (show y)
