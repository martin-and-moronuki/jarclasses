module Style where

import Clay hiding (bold, inlineBlock, italic, nowrap)
import qualified Clay
import Data.Maybe (fromJust)
import Path
import Pipes
import Relude hiding (intersperse, not, (&))
import Relude.Extra.Foldable1
import Resource

styles :: [(Resource, Css)]
styles =
  [ ([res|style/jarclasses.css|], jarclassesStyle),
    ([res|style/home.css|], homeStyle)
  ]

styleResources :: ListT IO Resource
styleResources = Select $ traverse_ (yield . fst) styles

makeStyles :: Path Abs Dir -> IO ()
makeStyles d = traverse_ (uncurry (make d)) styles

make :: Path Abs Dir -> Resource -> Css -> IO ()
make d r css = writeFileLBS path (encodeUtf8 txt)
  where
    path = Path.toFilePath (d Path.</> fromJust (resourceRelFile r))
    txt = renderWith pretty [] css

jarclassesStyle :: Css
jarclassesStyle =
  do
    p ? (noMargin <> noPadding)
    body ? background (rgb 0xf0 0xe8 0xe8)
    main_ ? do
      marginVertical (px 80)
      (p <> listTags <> blockquote <> headerTags) <? marginHorizontal auto
      (p <> listTags <> blockquote) <? maxWidth (px contentWidthPx)
      headerTags <? maxWidth (px (contentWidthPx + 40))
      (p <> listTags) <? do
        color (rgb 0x34 0x29 0x23)
        fontSize (px 17)
        lineHeight (unitless 1.6)
        textAlign justify
        contentFontFamily
        foldMap1 (\cls -> span # byClass cls) ("emphatic" :| "title" : []) ? italic
        foldMap1 (\cls -> span # byClass cls) ("item" :| []) ? (bold <> color (rgb 0x40 0x40 0x20))
        a ? (textDecoration none <> color (rgb 0x41 0x70 0x90) <> hoverUnderline)
      p <? marginVertical (em 0.7)
      headerTags <? (headerFontFamily <> bold)
      (h1 <> h2) <? do
        color (rgb 0x7c 0x33 0x4f)
        borderBottomColor (rgb 0xd3 0xcc 0xc1)
        lineHeight (unitless 1.2)
        paddingBottom (px 10)
      h1 <? do
        fontSize (em 1.85) <> italic <> textAlign center
        marginTop (em 1.1) <> marginBottom (em 1.5)
        borderBottomStyle double <> borderBottomWidth (em 0.2)
      h2 <? do
        fontSize (em 1.58)
        marginTop (em 2) <> marginBottom (em 0.5)
        borderBottomStyle solid <> borderBottomWidth (em 0.1)
      p # byClass "horizontalList" <? do
        star <? inlineBlock <> nowrap
        intersperse do
          content (stringContent "·")
          paddingHorizontal (em 0.5)
      blockquote <? do
        boxSizing borderBox
        borderLeft solid (px 1) purple
        paddingHorizontal (em 1)
        lineHeight (unitless 1.4)

homeStyle :: Css
homeStyle =
  do
    star # byClass "day" ? float floatRight <> color gray

contentWidthPx :: Double
contentWidthPx = 650

contentFontFamily, headerFontFamily :: Css
headerFontFamily = fontFamily ["Lato", "Open Sans", "Myriad", "Calibri"] [sansSerif]
contentFontFamily = fontFamily ["Georgia", "Palatino", "Palatino Linotype", "Times", "Times New Roman"] [serif]

noMargin, noPadding, hoverUnderline, bold, italic, nowrap, inlineBlock :: Css
noMargin = marginAll (px 0)
noPadding = paddingAll (px 0)
hoverUnderline = hover & textDecoration underline
bold = fontWeight Clay.bold
italic = fontStyle Clay.italic
nowrap = whiteSpace Clay.nowrap
inlineBlock = display Clay.inlineBlock

marginAll, marginVertical, marginHorizontal, paddingAll, paddingVertical, paddingHorizontal :: Size a -> Css
marginAll = marginVertical <> marginHorizontal
marginVertical = marginTop <> marginBottom
marginHorizontal = marginLeft <> marginRight
paddingAll = paddingVertical <> paddingHorizontal
paddingVertical = paddingTop <> paddingBottom
paddingHorizontal = paddingLeft <> paddingRight

headerTags, listTags :: Selector
headerTags = h1 <> h2 <> h3 <> h4 <> h5 <> h6
listTags = ul <> ol

intersperse :: Css -> Css
intersperse x = star # not (star # firstChild) # before <? x
