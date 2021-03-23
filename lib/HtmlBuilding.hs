module HtmlBuilding where

import Relude

import HtmlTypes

import Resource (Resource, resourceUrl)

-- A magical meta tag that tells mobile devices that we have mobile-friendly CSS. It prevents phones from attempting to automatically scale the page.
viewportMeta :: Series Header
viewportMeta = one $ HeaderMeta Meta{ metaName = "viewport", metaContent = "width=device-width, initial-scale=1" }

utf8htmlMeta :: Series Header
utf8htmlMeta = one $ HeaderHttpEquiv HttpEquiv{ httpEquivName = "Content-Type", httpEquivContent = "text/html; charset=utf-8" }

stylesheet :: Resource -> Series Header
stylesheet x = one $ HeaderLink Link{ linkRel = "stylesheet", linkType = "text/css", linkHref = resourceUrl x }

class ToInlines a where toInlines :: a -> Series Inline
instance ToInlines Text where toInlines = one . InlineFragment
instance ToInlines Comment where toInlines = one . InlineComment
instance ToInlines Span where toInlines = one . InlineSpan
instance ToInlines Anchor where toInlines = one . InlineAnchor

class ToBlocks a where toBlocks :: a -> Series Block
instance ToBlocks H where toBlocks = one . BlockH
instance ToBlocks Comment where toBlocks = one . BlockComment
instance ToBlocks Paragraph where toBlocks = one . BlockParagraph
instance ToBlocks Quote where toBlocks = one . BlockQuote
instance ToBlocks BulletedList where toBlocks = one . BlockBulletedList
