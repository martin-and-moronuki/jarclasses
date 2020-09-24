{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module ResourcePaths where

import Relude
import Path
import qualified Path
import qualified Data.Text as Text
import Control.Lens
import StringBuilding

---  Mappings between resource name, source path, and output file path  ---

type Resource = [Text]

resourceOutputPath :: Resource -> Maybe (Path Rel File)
resourceOutputPath [] = Nothing
resourceOutputPath r@("style" : _) = resourceRelFileBase r
resourceOutputPath r = (resourceRelFileBase >=> Path.addExtension ".html") r

test_resourceOutputPath :: Resource -> Text
test_resourceOutputPath x = "resourceOutputPath" <!> show x <!> "=" <!> show (resourceOutputPath x)

resourceInputPath :: Resource -> Maybe (Path Rel File)
resourceInputPath [] = Nothing
resourceInputPath ("style" : _) = Nothing
resourceInputPath r = (resourceRelFileBase >=> Path.addExtension ".pro") r

test_resourceInputPath :: Resource -> Text
test_resourceInputPath x = "resourceInputPath" <!> show x <!> "=" <!> show (resourceInputPath x)

resourceRelFileBase :: Resource -> Maybe (Path Rel File)
resourceRelFileBase r =
  unsnoc r >>= \(dirTexts, fileText) ->
    traverse (Path.parseRelDir . toString) dirTexts >>= \dirs ->
      (Path.parseRelFile . toString) fileText >>= \file ->
        Just $ foldr (Path.</>) file dirs

pathAsResourceInput :: Path Rel File -> Maybe Resource
pathAsResourceInput =
  Path.splitExtension >=> \case
    (p, ".pro") -> Just (relFileBaseResource p)
    _ -> Nothing

test_pathAsResourceInput :: Path Rel File -> Text
test_pathAsResourceInput x = "pathAsResourceInput" <!> quo (toText (toFilePath x)) <!> "=" <!> show (pathAsResourceInput x)

pathAsResourceOutput :: Path Rel File -> Maybe Resource
pathAsResourceOutput =
  Path.splitExtension >=> \case
    (p, ".html") -> Just (relFileBaseResource p)
    _ -> Nothing

test_pathAsResourceOutput :: Path Rel File -> Text
test_pathAsResourceOutput x = "pathAsResourceOutput" <!> quo (toText (toFilePath x)) <!> "=" <!> show (pathAsResourceOutput x)

relFileBaseResource :: Path Rel File -> Resource
relFileBaseResource file = f (Path.parent file) `snoc` txtFile (Path.filename file)
  where
    f :: Path Rel Dir -> [Text]
    f p = if Path.parent p == p then [] else f (Path.parent p) `snoc` txtDir (Path.dirname p)
    txtFile = toText . Path.toFilePath
    txtDir = fromMaybe (error "dir should have a trailing slash") . Text.stripSuffix "/" . toText . Path.toFilePath

resourceFileNameTests :: [Text]
resourceFileNameTests =
  [ test_pathAsResourceInput [relfile|menus/2019-11-11.pro|]
  , test_pathAsResourceOutput [relfile|menus/2019-11-11.pro|]
  , test_pathAsResourceInput [relfile|style/jarclasses.css|]
  , test_pathAsResourceOutput [relfile|style/jarclasses.css|]
  , test_resourceInputPath ["menus", "2019-11-11"]
  , test_resourceInputPath ["style", "jarclasses.css"]
  , test_resourceOutputPath ["menus", "2019-11-11"]
  , test_resourceOutputPath ["style", "jarclasses.css"]
  ]
