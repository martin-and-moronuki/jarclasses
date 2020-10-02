-- Mappings between resource name, source path, and output file path.

module ResourcePaths where

import Control.Lens
import qualified Data.Text as Text
import Path
import Relude
import StringBuilding
import TestFramework
import Pipes
import Path.IO

type Resource = [Text]

data Scheme = Scheme
  { scheme_proHtmlDirs :: Set (Path Rel Dir),
    scheme_styleDirs :: Set (Path Rel Dir)
  }
  deriving (Show)

inStyleDir :: Scheme -> Path Rel t -> Bool
inStyleDir s p = any (\d -> d `Path.isProperPrefixOf` p) (scheme_styleDirs s)

inProHtmlDir :: Scheme -> Path Rel t -> Bool
inProHtmlDir s p = any (\d -> d `Path.isProperPrefixOf` p) (scheme_proHtmlDirs s)

resourceOutputPath :: Scheme -> Resource -> Maybe (Path Rel File)
resourceOutputPath s r =
  resourceRelFileBase r >>= \p ->
    if inStyleDir s p
      then resourceRelFileBase r
      else resourceRelFileBase r >>= Path.addExtension ".html"

test_resourceOutputPath :: Scheme -> Resource -> Test
test_resourceOutputPath s x =
  one $
    "resourceOutputPath" <!> show s <!> show x <!> "=" <!> show (resourceOutputPath s x)

resourceInputPath :: Scheme -> Resource -> Maybe (Path Rel File)
resourceInputPath s r =
  resourceRelFileBase r >>= \p ->
    if inProHtmlDir s p
      then Path.addExtension ".pro" p
      else Nothing

test_resourceInputPath :: Scheme -> Resource -> Test
test_resourceInputPath s x =
  one $
    "resourceInputPath" <!> show s <!> show x <!> "=" <!> show (resourceInputPath s x)

resourceRelFileBase :: Resource -> Maybe (Path Rel File)
resourceRelFileBase r =
  unsnoc r >>= \(dirTexts, fileText) ->
    traverse (Path.parseRelDir . toString) dirTexts >>= \dirs ->
      (Path.parseRelFile . toString) fileText >>= \file ->
        Just $ foldr (Path.</>) file dirs

pathAsResourceInput :: Scheme -> Path Rel File -> Maybe Resource
pathAsResourceInput s p =
  guard (inProHtmlDir s p)
    *> Path.splitExtension p >>= \case
      (p', ".pro") -> Just (relFileBaseResource p')
      _ -> Nothing

test_pathAsResourceInput :: Scheme -> Path Rel File -> Test
test_pathAsResourceInput s x =
  one $
    "pathAsResourceInput" <!> show s <!> show x <!> "=" <!> show (pathAsResourceInput s x)

pathAsResourceOutput :: Scheme -> Path Rel File -> Maybe Resource
pathAsResourceOutput s p =
  guard (inProHtmlDir s p)
    *> Path.splitExtension p >>= \case
      (p', ".html") -> Just (relFileBaseResource p')
      _ -> Nothing

test_pathAsResourceOutput :: Scheme -> Path Rel File -> Test
test_pathAsResourceOutput s x =
  one $
    "pathAsResourceOutput" <!> show s <!> show x <!> "=" <!> show (pathAsResourceOutput s x)

relFileBaseResource :: Path Rel File -> Resource
relFileBaseResource file = f (Path.parent file) `snoc` txtFile (Path.filename file)
  where
    f :: Path Rel Dir -> [Text]
    f p = if Path.parent p == p then [] else f (Path.parent p) `snoc` txtDir (Path.dirname p)
    txtFile = toText . Path.toFilePath
    txtDir = fromMaybe (error "dir should have a trailing slash") . Text.stripSuffix "/" . toText . Path.toFilePath

findProHtmlResources :: Scheme -> Producer Resource IO ()
findProHtmlResources s =
  for_ (scheme_proHtmlDirs s) \d ->
    flip walkDirRel d \_ _ xs ->
      do
        for_ xs \x ->
          for_ (pathAsResourceInput s (d </> x)) yield
        pure $ WalkExclude []

test_path :: Scheme -> Path Rel File -> Test
test_path s x =
  test_pathAsResourceInput s x
    <> test_pathAsResourceOutput s x

test_resource :: Scheme -> Resource -> Test
test_resource s x =
  test_resourceInputPath s x
    <> test_resourceOutputPath s x

test :: Scheme -> Test
test s =
  foldMap (test_path s) paths
    <> foldMap (test_resource s) resources
  where
    paths :: Seq (Path Rel File) =
      one [relfile|menus/2019-11-11.pro|]
        <> one [relfile|style/jarclasses.css|]
    resources :: Seq Resource =
      one ["menus", "2019-11-11"]
        <> one ["style", "jarclasses.css"]
