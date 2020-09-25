-- Mappings between resource name, source path, and output file path.
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module ResourcePaths where

import Control.Lens
import qualified Data.Text as Text
import Path
import Relude
import StringBuilding

type Resource = [Text]

resourceOutputPath :: Resource -> Maybe (Path Rel File)
resourceOutputPath = \case
  [] -> Nothing
  r@("style" : _) -> resourceRelFileBase r
  r -> (resourceRelFileBase >=> Path.addExtension ".html") r

test_resourceOutputPath :: Resource -> Seq Text
test_resourceOutputPath x =
  one $
    "resourceOutputPath" <!> show x <!> "=" <!> show (resourceOutputPath x)

resourceInputPath :: Resource -> Maybe (Path Rel File)
resourceInputPath = \case
  [] -> Nothing
  ("style" : _) -> Nothing
  r -> (resourceRelFileBase >=> Path.addExtension ".pro") r

test_resourceInputPath :: Resource -> Seq Text
test_resourceInputPath x =
  one $
    "resourceInputPath" <!> show x <!> "=" <!> show (resourceInputPath x)

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

test_pathAsResourceInput :: Path Rel File -> Seq Text
test_pathAsResourceInput x =
  one $
    "pathAsResourceInput" <!> quo (toText (toFilePath x)) <!> "=" <!> show (pathAsResourceInput x)

pathAsResourceOutput :: Path Rel File -> Maybe Resource
pathAsResourceOutput =
  Path.splitExtension >=> \case
    (p, ".html") -> Just (relFileBaseResource p)
    _ -> Nothing

test_pathAsResourceOutput :: Path Rel File -> Seq Text
test_pathAsResourceOutput x =
  one $
    "pathAsResourceOutput" <!> quo (toText (toFilePath x)) <!> "=" <!> show (pathAsResourceOutput x)

relFileBaseResource :: Path Rel File -> Resource
relFileBaseResource file = f (Path.parent file) `snoc` txtFile (Path.filename file)
  where
    f :: Path Rel Dir -> [Text]
    f p = if Path.parent p == p then [] else f (Path.parent p) `snoc` txtDir (Path.dirname p)
    txtFile = toText . Path.toFilePath
    txtDir = fromMaybe (error "dir should have a trailing slash") . Text.stripSuffix "/" . toText . Path.toFilePath

test_path :: Path Rel File -> Seq Text
test_path x =
  test_pathAsResourceInput x
    <> test_pathAsResourceOutput x

test_resource :: Resource -> Seq Text
test_resource x =
  test_resourceInputPath x
    <> test_resourceOutputPath x

test :: Seq Text
test =
  foldMap test_path paths
    <> foldMap test_resource resources
  where
    paths :: Seq (Path Rel File) =
      one [relfile|menus/2019-11-11.pro|]
        <> one [relfile|style/jarclasses.css|]
    resources :: Seq Resource =
      one ["menus", "2019-11-11"]
        <> one ["style", "jarclasses.css"]
