{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Style where

import Clay
import Path
import Relude hiding ((&))
import Relude.Extra.Foldable1

makeStyles :: Path Abs Dir -> IO ()
makeStyles d = writeFileLBS path (encodeUtf8 txt)
  where
    path = Path.toFilePath (d Path.</> [relfile|style/jarclasses.css|])
    txt = renderWith pretty [] jarclassesStyle

jarclassesStyle :: Css
jarclassesStyle =
  do
    p ? do
      marginAll (px 0)
      paddingAll (px 0)
    body ? background (rgb 0xec 0xe4 0xd8)
    main_ ? do
      marginVertical (px 80)
      (p <> listTags <> headerTags) <? do
        maxWidth (px 506)
        marginHorizontal auto
      (p <> listTags) <? do
        color (rgb 0x54 0x49 0x43)
        fontSize (px 15)
        lineHeight (px 22.5)
        fontFamily ["Georgia", "Palatino", "Palatino Linotype", "Times", "Times New Roman"] [serif]
        foldMap1 (\cls -> span # byClass cls) ("emphatic" :| "title" : []) ? fontStyle italic
        a ? do
          textDecoration none
          color (rgb 0x41 0x70 0x90)
          hover & textDecoration underline
      p <? marginVertical (em 0.7)
      headerTags <? do
        fontFamily ["Open Sans", "Myriad", "Calibri"] [sansSerif]
        fontWeight bold
      (h1 <> h2) <? do
        color (rgb 0x7c 0x33 0x4f)
        borderBottomColor (rgb 0xd3 0xcc 0xc1)
        lineHeight (unitless 1.2)
        paddingBottom (px 10)
      h1 <? do
        fontSize (em 1.85)
        fontStyle italic
        textAlign center
        marginTop (em 1.1)
        marginBottom (em 0.38)
        borderBottomStyle double
        borderBottomWidth (em 0.2)
      h2 <? do
        fontSize (em 1.58)
        marginTop (em 0.95)
        marginBottom (em 0.5)
        borderBottomStyle solid
        borderBottomWidth (em 0.1)
  where
    marginAll = marginVertical <> marginHorizontal
    marginVertical = marginTop <> marginBottom
    marginHorizontal = marginLeft <> marginRight
    paddingAll = paddingVertical <> paddingHorizontal
    paddingVertical = paddingTop <> paddingBottom
    paddingHorizontal = paddingLeft <> paddingRight
    headerTags = h1 <> h2 <> h3 <> h4 <> h5 <> h6
    listTags = ul <> ol
