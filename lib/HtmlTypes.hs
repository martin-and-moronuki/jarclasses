module HtmlTypes where

import Relude

type Series = Seq

data Document = Document {documentLang :: Maybe Text, documentHead :: Series Header, documentContent :: Series Block}

data Header = HeaderMeta Meta | HeaderHttpEquiv HttpEquiv | HeaderTitle Text | HeaderLink Link

data HttpEquiv = HttpEquiv {httpEquivName :: Text, httpEquivContent :: Text}

data Meta = Meta {metaName :: Text, metaContent :: Text}

data Link = Link {linkRel :: Text, linkType :: Text, linkHref :: Text}

data Block
  = BlockMain Main
  | BlockParagraph Paragraph
  | BlockComment Comment
  | BlockH H
  | BlockOrderedList OrderedList
  | BlockBulletedList BulletedList
  | BlockQuote Quote

data Main = Main (Series Block)

data H = H HLevel (Series Inline)

data HLevel = H1 | H2 | H3 | H4 | H5 | H6 deriving (Enum, Bounded)

data Paragraph = Paragraph {paragraphClasses :: Series Text, paragraphContent :: Series Inline}

data OrderedList = OrderedList (Series ListItem)

data BulletedList = BulletedList (Series ListItem)

data ListItem = ListItem (Series Block) | ListComment Comment

data Inline = InlineFragment Text | InlineSpan Span | InlineComment Comment | InlineAnchor Anchor

data Span = Span {spanClasses :: Series Text, spanStyles :: Series Text, spanContent :: Series Inline}

data Comment = Comment Text

data Quote = Quote (Series Block)

data Anchor = Anchor {anchorName :: Maybe Text, anchorHref :: Maybe Text, anchorContent :: Series Inline}
