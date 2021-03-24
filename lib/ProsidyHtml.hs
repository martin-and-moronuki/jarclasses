module ProsidyHtml where

import Control.Lens
import qualified HtmlBuilding as H
import qualified HtmlTypes as H
import Prosidy
import Relude hiding (head)
import Resource

data ProHtmlOpts = ProHtmlOpts
  { extraBlockTags :: ProHtmlOpts -> BlockTag -> Maybe (H.Series H.Block),
    extraInlineTags :: ProHtmlOpts -> InlineTag -> Maybe (H.Series H.Inline),
    extraStyle :: [Resource]
  }

defaultOpts :: ProHtmlOpts
defaultOpts = ProHtmlOpts (\_ _ -> Nothing) (\_ _ -> Nothing) []

addBlockTag :: (ProHtmlOpts -> BlockTag -> Maybe (H.Series H.Block)) -> ProHtmlOpts -> ProHtmlOpts
addBlockTag f opts = opts {extraBlockTags = \o x -> extraBlockTags opts o x <|> f o x}

addInlineTag :: (ProHtmlOpts -> InlineTag -> Maybe (H.Series H.Inline)) -> ProHtmlOpts -> ProHtmlOpts
addInlineTag f opts = opts {extraInlineTags = \o x -> extraInlineTags opts o x <|> f o x}

proHtml :: ProHtmlOpts -> Either Prosidy.Failure Document -> H.Document
proHtml opts doc = H.Document {H.documentLang, H.documentHead, H.documentContent}
  where
    documentLang = Just "en"
    documentHead = H.utf8htmlMeta <> H.viewportMeta <> title <> css
    css = foldMap H.stylesheet (one [res|style/jarclasses.css|] ++ extraStyle opts)
    title = foldMap (one . H.HeaderTitle) $ proTitle doc
    documentContent =
      one $
        H.BlockMain $
          H.Main $
            (foldMap (H.toBlocks . H.H H.H1 . H.toInlines) $ proTitle doc)
              <> (either (H.toBlocks . H.Comment . ("\n" <>) . toText . prettyFailure) (foldMap (proBlockHtml opts) . view content) doc)

proTitle :: Either Prosidy.Failure Document -> Maybe Text
proTitle = either (const Nothing) (view (atSetting "title"))

proBlockHtml :: ProHtmlOpts -> Block -> H.Series H.Block
proBlockHtml opts =
  \case
    BlockLiteral x -> H.toBlocks $ H.Comment (show x)
    BlockParagraph x -> H.toBlocks $ H.Paragraph {H.paragraphClasses, H.paragraphContent}
      where
        paragraphClasses = mempty
        paragraphContent = inlineHtml opts (view content x)
    BlockTag x -> proBlockTagHtml opts x

proBlockTagHtml :: ProHtmlOpts -> BlockTag -> H.Series H.Block
proBlockTagHtml opts x =
  case (tagName x) of
    "day" -> h2 opts x
    "h2" -> h2 opts x
    "head" -> h2 opts x
    "subhead" -> h3 opts x
    "list" -> proListHtml opts x
    "quote" -> blockQuote opts x
    "blockquote" -> blockQuote opts x
    _ -> fromMaybe (H.toBlocks $ H.Comment (show x)) (extraBlockTags opts opts x)

h2 :: ProHtmlOpts -> BlockTag -> H.Series H.Block
h2 opts = H.toBlocks . H.H H.H2 . requireInlineOnly opts . view content

h3 :: ProHtmlOpts -> BlockTag -> H.Series H.Block
h3 opts = H.toBlocks . H.H H.H3 . requireInlineOnly opts . view content

blockQuote :: ProHtmlOpts -> BlockTag -> H.Series H.Block
blockQuote opts = H.toBlocks . H.Quote . foldMap (proBlockHtml opts) . view content

requireInlineOnly :: ProHtmlOpts -> Series Block -> H.Series H.Inline
requireInlineOnly opts =
  foldMap
    \case
      BlockParagraph y -> inlineHtml opts (view content y)
      y -> H.toInlines $ H.Comment (show y)

proInlineHtml :: ProHtmlOpts -> Inline -> H.Series H.Inline
proInlineHtml opts =
  \case
    Break -> H.toInlines (" " :: Text)
    InlineText x -> H.toInlines (fragmentText x)
    InlineTag x -> proInlineTagHtml opts x

proInlineTagHtml :: ProHtmlOpts -> InlineTag -> H.Series H.Inline
proInlineTagHtml opts x =
  case (tagName x) of
    "dash" -> H.toInlines ("—" :: Text)
    "emphatic" -> H.toInlines $ H.Span {H.spanClasses, H.spanStyles, H.spanContent}
      where
        spanClasses = one "emphatic"
        spanStyles = mempty
        spanContent = inlineHtml opts (view content x)
    "italic" -> H.toInlines $ H.Span {H.spanClasses, H.spanStyles, H.spanContent}
      where
        spanClasses = mempty
        spanStyles = one "font-style: italic"
        spanContent = inlineHtml opts (view content x)
    "title" -> H.toInlines $ H.Span {H.spanClasses, H.spanStyles, H.spanContent}
      where
        spanClasses = one "title"
        spanStyles = mempty
        spanContent = inlineHtml opts (view content x)
    "link" -> H.toInlines $ H.Anchor {H.anchorName, H.anchorHref, H.anchorContent}
      where
        anchorName = Nothing
        anchorHref = view (atSetting "to") x
        anchorContent = inlineHtml opts (view content x)
    _ -> fromMaybe (H.toInlines $ H.Comment (show x)) (extraInlineTags opts opts x)

proListHtml :: ProHtmlOpts -> BlockTag -> H.Series H.Block
proListHtml opts x =
  case listAxis x of
    ListVertical -> H.toBlocks $ H.BulletedList listItems
      where
        listItems = foldMap (itemVertical opts) (view content x)
    ListHorizontal -> H.toBlocks $ H.Paragraph {H.paragraphClasses, H.paragraphContent}
      where
        paragraphClasses = one "horizontalList"
        paragraphContent = foldMap (itemHorizontal opts) (view content x)

data ListAxis = ListVertical | ListHorizontal

listAxis :: BlockTag -> ListAxis
listAxis x = if view (hasProperty "horizontal") x then ListHorizontal else ListVertical

itemVertical :: ProHtmlOpts -> Block -> H.Series H.ListItem
itemVertical opts x =
  case x of
    BlockTag i | tagName i == "item" -> go $ view content i
    BlockParagraph i -> go $ pure $ BlockParagraph i
    y -> one $ H.ListComment $ H.Comment $ show y
  where
    go i = one $ H.ListItem $ foldMap (proBlockHtml opts') i
      where
        opts' = opts & addInlineTag inlineItemTag

itemHorizontal :: ProHtmlOpts -> Block -> H.Series H.Inline
itemHorizontal opts x =
  case x of
    BlockTag i | tagName i == "item" -> go $ view content i
    BlockParagraph i -> go $ pure $ BlockParagraph i
    y -> H.toInlines $ H.Comment $ show y
  where
    go i = H.toInlines $ H.Span {H.spanClasses, H.spanStyles, H.spanContent}
      where
        spanClasses = mempty
        spanStyles = mempty
        spanContent = requireInlineOnly opts i

inlineItemTag :: ProHtmlOpts -> InlineTag -> Maybe (H.Series H.Inline)
inlineItemTag opts =
  \case
    i
      | tagName i == "item" ->
        Just $ H.toInlines $ H.Span {H.spanClasses, H.spanStyles, H.spanContent}
      where
        spanClasses = one "item"
        spanStyles = mempty
        spanContent = inlineHtml opts (view content i)
    _ -> Nothing

inlineHtml :: Foldable series => ProHtmlOpts -> series Inline -> H.Series H.Inline
inlineHtml opts = foldMap (proInlineHtml opts)
