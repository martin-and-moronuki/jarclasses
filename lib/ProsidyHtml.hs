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
  { extraBlockTags :: ProHtmlOpts -> BlockTag -> Maybe Html,
    extraInlineTags :: ProHtmlOpts -> InlineTag -> Maybe Html,
    extraStyle :: [Resource]
  }

defaultOpts :: ProHtmlOpts
defaultOpts = ProHtmlOpts (\_ _ -> Nothing) (\_ _ -> Nothing) []

addBlockTag :: (ProHtmlOpts -> BlockTag -> Maybe Html) -> ProHtmlOpts -> ProHtmlOpts
addBlockTag f opts = opts{ extraBlockTags = \o x -> extraBlockTags opts o x <|> f o x }

addInlineTag :: (ProHtmlOpts -> InlineTag -> Maybe Html) -> ProHtmlOpts -> ProHtmlOpts
addInlineTag f opts = opts{ extraInlineTags = \o x -> extraInlineTags opts o x <|> f o x }

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
    BlockParagraph x -> HTML.p (foldMap (proInlineHtml opts) (view content x))
    BlockTag x -> proBlockTagHtml opts x

proBlockTagHtml :: ProHtmlOpts -> BlockTag -> Html
proBlockTagHtml opts x =
  case (tagName x) of
    "day" -> h2 opts x
    "h2" -> h2 opts x
    "list" -> proListHtml opts x
    "quote" -> blockQuote opts x
    "blockquote" -> blockQuote opts x
    _ -> fromMaybe (HTML.stringComment (show x)) (extraBlockTags opts opts x)

h2 :: ProHtmlOpts -> BlockTag -> Html
h2 opts = HTML.h2 . requireInlineOnly opts . view content

blockQuote :: ProHtmlOpts -> BlockTag -> Html
blockQuote opts = HTML.blockquote . foldMap (proBlockHtml opts) . view content

requireInlineOnly :: ProHtmlOpts -> Series Block -> Html
requireInlineOnly opts =
  foldMap
    \case
      BlockParagraph y ->
        foldMap (proInlineHtml opts) (view content y)
      y -> HTML.stringComment (show y)

proInlineHtml :: ProHtmlOpts -> Inline -> Html
proInlineHtml opts =
  \case
    Break -> toHtml (" " :: String)
    InlineText x -> toHtml (fragmentText x)
    InlineTag x -> proInlineTagHtml opts x

proInlineTagHtml :: ProHtmlOpts -> InlineTag -> Html
proInlineTagHtml opts x =
  case (tagName x) of
    "dash" -> HTML.preEscapedToHtml ("&mdash;" :: Text)
    "emphatic" -> HTML.span ! Attr.class_ "emphatic" $ foldMap (proInlineHtml opts) (view content x)
    "italic" -> HTML.span ! Attr.style "font-style: italic" $ foldMap (proInlineHtml opts) (view content x)
    "title" -> HTML.span ! Attr.class_ "title" $ foldMap (proInlineHtml opts) (view content x)
    "link" -> HTML.a ! (maybe mempty (Attr.href . toValue) $ view (atSetting "to") x) $ foldMap (proInlineHtml opts) (view content x)
    _ -> fromMaybe (HTML.stringComment (show x)) (extraInlineTags opts opts x)

proListHtml :: ProHtmlOpts -> BlockTag -> Html
proListHtml opts x =
  case listAxis x of
    ListVertical -> HTML.ul i
    ListHorizontal -> HTML.p ! Attr.class_ "horizontalList" $ i
  where
    i = foldMap (itemHtml opts (listAxis x)) (view content x)

data ListAxis = ListVertical | ListHorizontal

listAxis :: BlockTag -> ListAxis
listAxis x = if view (hasProperty "horizontal") x then ListHorizontal else ListVertical

itemHtml :: ProHtmlOpts -> ListAxis -> Block -> Html
itemHtml opts a x =
  case x of
    BlockTag i | tagName i == "item" -> go (view content i)
    BlockParagraph i -> go (pure (BlockParagraph i))
    y -> HTML.stringComment (show y)
  where
    go i =
      case a of
        ListHorizontal -> HTML.span $ requireInlineOnly opts i
        ListVertical -> HTML.li $ foldMap (proBlockHtml opts') i
          where
            opts' = opts & addInlineTag inlineItemTag

inlineItemTag :: ProHtmlOpts -> InlineTag -> Maybe Html
inlineItemTag opts i
  | tagName i == "item" = Just $ HTML.span ! Attr.class_ "item" $ foldMap (proInlineHtml opts) (view content i)
  | otherwise = Nothing
