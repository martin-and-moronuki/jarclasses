module FileLayout where

import Generic.Data
import Path
import Path.IO
import Pipes
import Relude
import Resource
import StringBuilding
import TestFramework

data ProHtmlResource
  = ProHtmlResource Resource InputPath OutputPath DeployPath
  deriving stock (Eq, Ord, Show)

proHtmlResourceId :: ProHtmlResource -> Resource
proHtmlResourceId (ProHtmlResource x _ _ _) = x

proHtmlInputPath :: ProHtmlResource -> InputPath
proHtmlInputPath (ProHtmlResource _ x _ _) = x

newtype InputPath = InputPath (Path Rel File)
  deriving stock (Eq, Ord, Show)

newtype OutputPath = OutputPath (Path Rel File)
  deriving stock (Eq, Ord, Show)

newtype DeployPath = DeployPath (Path Rel File)
  deriving stock (Eq, Ord, Show)

data Scheme = Scheme
  { scheme_proHtmlDirs :: Set (Path Rel Dir),
    scheme_styleDirs :: Set (Path Rel Dir),
    scheme_otherProHtmlResources :: Set ProHtmlResource,
    scheme_rssLocation :: Set (Path Rel File)
  }
  deriving stock (Show, Generic)

instance Semigroup Scheme where
  (<>) = gmappend

instance Monoid Scheme where
  mempty = gmempty

schemeRssLocationMaybe :: Scheme -> Maybe (Path Rel File)
schemeRssLocationMaybe scheme =
  case toList (scheme_rssLocation scheme) of
    [x] -> Just x
    _ -> Nothing

schemeRssResourceMaybe :: Scheme -> Maybe Resource
schemeRssResourceMaybe = fmap relFileResource . schemeRssLocationMaybe

dirsToWatch :: Scheme -> [Path Rel Dir]
dirsToWatch s = toList $ scheme_proHtmlDirs s <> scheme_styleDirs s

filesToWatch :: Scheme -> [Path Rel File]
filesToWatch s =
  toList $
    foldMap
      ( \(ProHtmlResource _ (InputPath a) (OutputPath b) _) ->
          one @(Set (Path Rel File)) a
            <> one @(Set (Path Rel File)) b
      )
      (scheme_otherProHtmlResources s)

testLine :: Text -> Test
testLine line = one ([relfile|ResourcePaths.txt|], line)

inStyleDir :: Scheme -> Path Rel t -> Bool
inStyleDir s p = any (\d -> d `Path.isProperPrefixOf` p) (scheme_styleDirs s)

inProHtmlDir :: Scheme -> Path Rel t -> Bool
inProHtmlDir s p = any (\d -> d `Path.isProperPrefixOf` p) (scheme_proHtmlDirs s)

resourceOutputPath :: Scheme -> Resource -> Maybe OutputPath
resourceOutputPath s r = other <|> css <|> html <|> rss
  where
    css =
      fmap OutputPath $
        resourceRelFile r >>= \p ->
          if inStyleDir s p
            then resourceRelFile r
            else Nothing
    html =
      fmap OutputPath $
        resourceRelFile r >>= \p ->
          if inProHtmlDir s p
            then resourceRelFile r >>= Path.addExtension ".html"
            else Nothing
    rss =
      guard (schemeRssResourceMaybe s == Just r)
        *> (OutputPath <$> schemeRssLocationMaybe s)
    other =
      getFirst $
        foldMap
          (\(ProHtmlResource r' _ p _) -> First $ if r' == r then Just p else Nothing)
          (scheme_otherProHtmlResources s)

test_resourceOutputPath :: Scheme -> Resource -> Test
test_resourceOutputPath s x =
  testLine $
    "resourceOutputPath" <!> show x <!> "=" <!> show (resourceOutputPath s x)

resourceInputPath :: Scheme -> Resource -> Maybe InputPath
resourceInputPath s r = other <|> pro
  where
    pro =
      fmap InputPath $
        resourceRelFile r >>= \p ->
          if inProHtmlDir s p then Path.addExtension ".pro" p else Nothing
    other =
      getFirst $
        foldMap
          (\(ProHtmlResource r' p _ _) -> First $ if r' == r then Just p else Nothing)
          (scheme_otherProHtmlResources s)

test_resourceInputPath :: Scheme -> Resource -> Test
test_resourceInputPath s x =
  testLine $
    "resourceInputPath" <!> show x <!> "=" <!> show (resourceInputPath s x)

pathAsResourceInput :: Scheme -> InputPath -> Maybe Resource
pathAsResourceInput s ip@(InputPath p) = other <|> inDir
  where
    inDir =
      guard (inProHtmlDir s p)
        *> Path.splitExtension p >>= \case
          (p', ".pro") -> Just (relFileResource p')
          _ -> Nothing
    other =
      getFirst $
        foldMap
          (\(ProHtmlResource r p' _ _) -> First $ if p' == ip then Just r else Nothing)
          (scheme_otherProHtmlResources s)

test_pathAsResourceInput :: Scheme -> InputPath -> Test
test_pathAsResourceInput s x =
  testLine $
    "pathAsResourceInput" <!> show x <!> "=" <!> show (pathAsResourceInput s x)

pathAsResourceOutput :: Scheme -> OutputPath -> Maybe Resource
pathAsResourceOutput s ip@(OutputPath p) = other <|> inDir <|> rss
  where
    inDir =
      guard (inProHtmlDir s p)
        *> Path.splitExtension p >>= \case
          (p', ".html") -> Just (relFileResource p')
          _ -> Nothing
    rss =
      guard (schemeRssLocationMaybe s == Just p)
        *> schemeRssResourceMaybe s
    other =
      getFirst $
        foldMap
          (\(ProHtmlResource r _ p' _) -> First $ if p' == ip then Just r else Nothing)
          (scheme_otherProHtmlResources s)

test_pathAsResourceOutput :: Scheme -> OutputPath -> Test
test_pathAsResourceOutput s x =
  testLine $
    "pathAsResourceOutput" <!> show x <!> "=" <!> show (pathAsResourceOutput s x)

pathAsProHtmlInput :: Scheme -> InputPath -> Maybe ProHtmlResource
pathAsProHtmlInput s p =
  do
    r <- pathAsResourceInput s p
    p' <- resourceOutputPath s r
    p'' <- resourceDeployPath s r
    Just (ProHtmlResource r p p' p'')

resourceAsProHtml :: Scheme -> Resource -> Maybe ProHtmlResource
resourceAsProHtml s r =
  do
    p <- resourceInputPath s r
    p' <- resourceOutputPath s r
    p'' <- resourceDeployPath s r
    Just (ProHtmlResource r p p' p'')

resourceDeployPath :: Scheme -> Resource -> Maybe DeployPath
resourceDeployPath s r =
  case getFirst (foldMap (\(ProHtmlResource r' _ _ p) -> First $ if r' == r then Just p else Nothing) (scheme_otherProHtmlResources s)) of
    Just p -> Just p
    Nothing -> case resourceOutputPath s r of
      Nothing -> Nothing
      Just (OutputPath x) -> Just (DeployPath x)

findProHtmlResources :: Scheme -> Producer ProHtmlResource IO ()
findProHtmlResources s =
  do
    for_ (scheme_otherProHtmlResources s) yield
    for_ (scheme_proHtmlDirs s) \d ->
      flip walkDirRel d \_ _ xs ->
        do
          for_ xs \x ->
            for_ (pathAsProHtmlInput s (InputPath (d </> x))) yield
          pure $ WalkExclude []

test_path :: Scheme -> Path Rel File -> Test
test_path s x =
  test_pathAsResourceInput s (InputPath x)
    <> test_pathAsResourceOutput s (OutputPath x)

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
        <> one [relfile|home/home.pro|]
        <> one [relfile|feed/rss.xml|]
    resources :: Seq Resource =
      one [res|menus/2019-11-11|]
        <> one [res|style/jarclasses.css|]
        <> one [res||]
        <> one [res|menus|]
        <> one [res|feed/rss.xml|]
