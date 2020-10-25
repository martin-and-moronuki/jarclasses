-- Mappings between resource name, source path, and output file path.

module ResourcePaths where

import Control.Lens
import qualified Data.Text as Text
import Path
import Path.IO
import Pipes
import Relude
import StringBuilding
import TestFramework

type Resource = [Text]

data Scheme = Scheme
  { scheme_proHtmlDirs :: Set (Path Rel Dir),
    scheme_styleDirs :: Set (Path Rel Dir)
  }
  deriving (Show)

testLine :: Text -> Test
testLine line = one ([relfile|ResourcePaths.txt|], line)

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
  testLine $
    "resourceOutputPath" <!> show s <!> show x <!> "=" <!> show (resourceOutputPath s x)

resourceInputPath :: Scheme -> Resource -> Maybe (Path Rel File)
resourceInputPath s r =
  resourceRelFileBase r >>= \p ->
    if inProHtmlDir s p
      then Path.addExtension ".pro" p
      else Nothing

test_resourceInputPath :: Scheme -> Resource -> Test
test_resourceInputPath s x =
  testLine $
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
  testLine $
    "pathAsResourceInput" <!> show s <!> show x <!> "=" <!> show (pathAsResourceInput s x)

pathAsResourceOutput :: Scheme -> Path Rel File -> Maybe Resource
pathAsResourceOutput s p =
  guard (inProHtmlDir s p)
    *> Path.splitExtension p >>= \case
      (p', ".html") -> Just (relFileBaseResource p')
      _ -> Nothing

test_pathAsResourceOutput :: Scheme -> Path Rel File -> Test
test_pathAsResourceOutput s x =
  testLine $
    "pathAsResourceOutput" <!> show s <!> show x <!> "=" <!> show (pathAsResourceOutput s x)

pathAsProHtmlInput :: Scheme -> Path Rel File -> Maybe ProHtmlResource
pathAsProHtmlInput s p =
  do
    r <- pathAsResourceOutput s p
    p' <- resourceOutputPath s r
    Just (ProHtmlResource r p p')

resourceAsProHtml :: Scheme -> Resource -> Maybe ProHtmlResource
resourceAsProHtml s r =
  do
    p <- resourceInputPath s r
    p' <- resourceOutputPath s r
    Just (ProHtmlResource r p p')

relFileBaseResource :: Path Rel File -> Resource
relFileBaseResource file = f (Path.parent file) `snoc` txtFile (Path.filename file)
  where
    f :: Path Rel Dir -> [Text]
    f p = if Path.parent p == p then [] else f (Path.parent p) `snoc` txtDir (Path.dirname p)
    txtFile = toText . Path.toFilePath
    txtDir = fromMaybe (error "dir should have a trailing slash") . Text.stripSuffix "/" . toText . Path.toFilePath

data ProHtmlResource
  = ProHtmlResource
      Resource
      (Path Rel File) -- input
      (Path Rel File) -- output

findProHtmlResources :: Scheme -> Producer ProHtmlResource IO ()
findProHtmlResources s =
  for_ (scheme_proHtmlDirs s) \d ->
    flip walkDirRel d \_ _ xs ->
      do
        for_ xs \x ->
          for_ (pathAsProHtmlInput s (d </> x)) yield
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
