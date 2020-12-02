module Style where

import Clay hiding (bold, italic)
import qualified Clay
import Path
import Pipes
import Relude hiding ((&))
import Relude.Extra.Foldable1
import Resource

styleResources :: Producer Resource IO ()
styleResources = yield [res|style/jarclasses.css|]

makeStyles :: Path Abs Dir -> IO ()
makeStyles d = writeFileLBS path (encodeUtf8 txt)
  where
    path = Path.toFilePath (d Path.</> [relfile|style/jarclasses.css|])
    txt = renderWith pretty [] jarclassesStyle

jarclassesStyle :: Css
jarclassesStyle =
  do
    p ? (noMargin <> noPadding)
    body ? background (rgb 0xf0 0xe8 0xe8)
    main_ ? do
      marginVertical (px 80)
      (p <> listTags <> headerTags) <? marginHorizontal auto
      (p <> listTags) <? maxWidth (px contentWidthPx)
      headerTags <? maxWidth (px (contentWidthPx + 40))
      (p <> listTags) <? do
        color (rgb 0x34 0x29 0x23)
        fontSize (px 17)
        lineHeight (unitless 1.6)
        textAlign justify
        contentFontFamily
        foldMap1 (\cls -> span # byClass cls) ("emphatic" :| "title" : []) ? italic
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

contentWidthPx :: Double
contentWidthPx = 650

contentFontFamily, headerFontFamily :: Css
headerFontFamily = fontFamily ["Open Sans", "Myriad", "Calibri"] [sansSerif]
contentFontFamily = fontFamily ["Georgia", "Palatino", "Palatino Linotype", "Times", "Times New Roman"] [serif]

noMargin, noPadding, hoverUnderline, bold, italic :: Css
noMargin = marginAll (px 0)
noPadding = paddingAll (px 0)
hoverUnderline = hover & textDecoration underline
bold = fontWeight Clay.bold
italic = fontStyle Clay.italic

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
