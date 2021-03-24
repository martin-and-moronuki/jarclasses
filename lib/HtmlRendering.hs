module HtmlRendering where

import qualified Data.Text as Text
import qualified Data.Text.Lazy.Builder as Text (Builder)
import qualified Data.Text.Lazy.Builder as Text.Builder
import HtmlTypes
import Relude

type IndentLevel = Natural

indentation :: IndentLevel -> Text.Builder
indentation il = fold $ replicate (fromIntegral il) $ Text.Builder.fromText "\t"

renderHtml :: Document -> Text.Builder
renderHtml Document {documentLang, documentHead, documentContent} =
  "<!DOCTYPE HTML>\n\n" <> html
  where
    html = elBlock il htmlTag htmlContent
    htmlTag = Tag {tagName = "html", tagAttrs = foldMap (one . Setting "lang") documentLang}
    htmlContent = renderHead (il + 1) documentHead <> renderBody (il + 1) documentContent
    il = 0

renderHead :: IndentLevel -> Series Header -> Text.Builder
renderHead il = elBlock il (tag "head") . foldMap (renderHeader (il + 1))

renderBody :: IndentLevel -> Series Block -> Text.Builder
renderBody il = elBlock il (tag "body") . foldMap (renderBlock (il + 1))

data Tag = Tag {tagName :: Text, tagAttrs :: Series Attr}

tag :: Text -> Tag
tag tagName = Tag {tagName, tagAttrs}
  where
    tagAttrs = mempty

data Attr = Setting Text Text | Property Text

renderHeader :: IndentLevel -> Header -> Text.Builder
renderHeader il =
  \case
    HeaderMeta x -> renderMeta il x
    HeaderHttpEquiv x -> renderHttpEquiv il x
    HeaderTitle x -> elBlockInlineTransition il (tag "title") (renderFragment x)
    HeaderLink x -> renderLink il x

renderMeta :: IndentLevel -> Meta -> Text.Builder
renderMeta il Meta {metaName, metaContent} =
  elBlockNoContent il Tag {tagName, tagAttrs}
  where
    tagName = "meta"
    tagAttrs =
      one (Setting "name" metaName)
        <> one (Setting "content" metaContent)

renderHttpEquiv :: IndentLevel -> HttpEquiv -> Text.Builder
renderHttpEquiv il HttpEquiv {httpEquivName, httpEquivContent} =
  elBlockNoContent il Tag {tagName, tagAttrs}
  where
    tagName = "meta"
    tagAttrs =
      one (Setting "http-equiv" httpEquivName)
        <> one (Setting "content" httpEquivContent)

renderLink :: IndentLevel -> Link -> Text.Builder
renderLink il Link {linkRel, linkType, linkHref} =
  elBlockNoContent il Tag {tagName, tagAttrs}
  where
    tagName = "link"
    tagAttrs =
      one (Setting "rel" linkRel)
        <> one (Setting "type" linkType)
        <> one (Setting "href" linkHref)

renderBlock :: IndentLevel -> Block -> Text.Builder
renderBlock il =
  \case
    BlockMain (Main x) -> blockNoAttrs "main" x
    BlockQuote (Quote x) -> blockNoAttrs "blockquote" x
    BlockParagraph x -> renderParagraph il x
    BlockComment (Comment x) -> indentation il <> "<!--" <> Text.Builder.fromText x <> "-->\n"
    BlockOrderedList (OrderedList x) -> list "ol" x
    BlockBulletedList (BulletedList x) -> list "ul" x
    BlockH x -> renderH il x
  where
    list :: Text -> Series ListItem -> Text.Builder
    list tagName = elBlock il Tag {tagName, tagAttrs} . foldMap (renderListItem (il + 1)) . toList
      where
        tagAttrs = mempty

    blockNoAttrs :: Text -> Series Block -> Text.Builder
    blockNoAttrs tagName = elBlock il Tag {tagName, tagAttrs} . foldMap (renderBlock (il + 1)) . toList
      where
        tagAttrs = mempty

renderInline :: Inline -> Text.Builder
renderInline =
  \case
    InlineFragment x -> renderFragment x
    InlineSpan x -> renderSpan x
    InlineComment (Comment x) -> "<!--" <> Text.Builder.fromText x <> "-->"
    InlineAnchor x -> renderAnchor x

renderAnchor :: Anchor -> Text.Builder
renderAnchor Anchor {anchorName, anchorHref, anchorContent} =
  elInline Tag {tagName, tagAttrs} (foldMap renderInline anchorContent)
  where
    tagName = "a"
    tagAttrs =
      foldMap (one . Setting "name") anchorName
        <> foldMap (one . Setting "href") anchorHref

renderSpan :: Span -> Text.Builder
renderSpan Span {spanClasses, spanStyles, spanContent} =
  elInline Tag {tagName, tagAttrs} (foldMap renderInline spanContent)
  where
    tagName = "span"
    tagAttrs = classAttrs spanClasses <> styleAttrs spanStyles

classAttrs :: Foldable series => series Text -> Series Attr
classAttrs = foldMap (one . Setting "class" . fold . intersperse " ") . mfilter (not . null) . Just . toList

styleAttrs :: Foldable series => series Text -> Series Attr
styleAttrs = foldMap (one . Setting "style" . fold . intersperse "; ") . mfilter (not . null) . Just . toList

renderH :: IndentLevel -> H -> Text.Builder
renderH il (H hl x) = elBlockInlineTransition il (tag (hLevelTagName hl)) $ foldMap renderInline x

hLevelTagName :: HLevel -> Text
hLevelTagName =
  \case
    H1 -> "h1"
    H2 -> "h2"
    H3 -> "h3"
    H4 -> "h4"
    H5 -> "h5"
    H6 -> "h6"

renderListItem :: IndentLevel -> ListItem -> Text.Builder
renderListItem il =
  \case
    ListItem x -> elBlock il (tag "li") $ foldMap (renderBlock (il + 1)) x
    ListComment (Comment x) -> indentation il <> "<!--" <> Text.Builder.fromText x <> "-->\n"

renderParagraph :: IndentLevel -> Paragraph -> Text.Builder
renderParagraph il Paragraph {paragraphClasses, paragraphContent} =
  elBlockInlineTransition il Tag {tagName, tagAttrs} (foldMap renderInline paragraphContent)
  where
    tagName = "p"
    tagAttrs = classAttrs paragraphClasses

renderAttrs :: Series Attr -> Text.Builder
renderAttrs =
  foldMap $
    \case
      Setting x value ->
        " " <> Text.Builder.fromText x <> "=\""
          <> (Text.Builder.fromText $ Text.concatMap escapeAttrChar value)
          <> "\""
      Property x ->
        " " <> Text.Builder.fromText x

open :: Tag -> Text.Builder
open Tag {tagName, tagAttrs} = "<" <> Text.Builder.fromText tagName <> renderAttrs tagAttrs <> ">"

close :: Tag -> Text.Builder
close Tag {tagName} = "</" <> Text.Builder.fromText tagName <> ">"

-- | Render a block element that contains block content.
elBlock :: IndentLevel -> Tag -> Text.Builder -> Text.Builder
elBlock il t content = indentation il <> open t <> "\n" <> content <> indentation il <> close t <> "\n"

-- | Render a block element that contains inline content.
elBlockInlineTransition :: IndentLevel -> Tag -> Text.Builder -> Text.Builder
elBlockInlineTransition il t content = indentation il <> open t <> content <> close t <> "\n"

-- | Render an inline element that contains inline content.
elInline :: Tag -> Text.Builder -> Text.Builder
elInline t content = open t <> content <> close t

-- | Render a block element that contains no content.
elBlockNoContent :: IndentLevel -> Tag -> Text.Builder
elBlockNoContent il t = indentation il <> open t <> "\n"

-- | Render an inline element that contains no content.
elInlineNoContent :: Tag -> Text.Builder
elInlineNoContent t = open t

renderFragment :: Text -> Text.Builder
renderFragment = Text.Builder.fromText . Text.concatMap escapeFragmentChar

escapeFragmentChar :: Char -> Text
escapeFragmentChar =
  \case
    '&' -> "&amp;"
    '<' -> "&lt;"
    '>' -> "&gt;"
    x -> Text.singleton x

escapeAttrChar :: Char -> Text
escapeAttrChar =
  \case
    '&' -> "&amp;"
    '<' -> "&lt;"
    '>' -> "&gt;"
    '"' -> "&quot;"
    x -> Text.singleton x
