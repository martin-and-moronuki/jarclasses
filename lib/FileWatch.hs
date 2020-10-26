module FileWatch (fileWatch) where

import Control.Exception.Safe
import qualified Data.Map as Map
import qualified Data.Set as Set
import Path
import Relude
import qualified System.FSNotify as FSN

data WatchDepth = Deep | Shallow

data WatchSet = WatchSet (Map (Path Rel Dir) WatchDepth)

makeWatchSet :: [Path Rel Dir] -> [Path Rel File] -> WatchSet
makeWatchSet dirs files =
  WatchSet $
    foldMap
      (\x -> Map.singleton x Deep)
      dirSet
      <> foldMap
        (\x -> Map.singleton x Shallow)
        ( Set.filter
            -- remove any dirs subsumed by a deep watch
            (\x -> not $ any (\p -> p `isProperPrefixOf` x) dirSet)
            fileParentSet
        )
  where
    dirSet :: Set (Path Rel Dir) = Set.fromList dirs
    fileParentSet :: Set (Path Rel Dir) = foldMap (one . parent) files

watchSetList :: WatchSet -> [(Path Rel Dir, WatchDepth)]
watchSetList (WatchSet m) = Map.toAscList m

fsnConfig :: FSN.WatchConfig
fsnConfig = FSN.defaultConfig {FSN.confDebounce = FSN.NoDebounce}

fileWatch :: (SomeException -> IO ()) -> (Path Rel File -> IO ()) -> Path Abs Dir -> [Path Rel Dir] -> [Path Rel File] -> IO a -> IO a
fileWatch l act cwd fps fps' go = FSN.withManagerConf fsnConfig \man -> fileWatchN l man act cwd (watchSetList (makeWatchSet fps fps')) go

fileWatchN :: (SomeException -> IO ()) -> FSN.WatchManager -> (Path Rel File -> IO ()) -> Path Abs Dir -> [(Path Rel Dir, WatchDepth)] -> IO a -> IO a
fileWatchN l man act cwd = fix \r ->
  \case
    [] -> id
    (fp, d) : fps -> fileWatch1 l man act cwd fp d . r fps

fileWatch1 :: (SomeException -> IO ()) -> FSN.WatchManager -> (Path Rel File -> IO ()) -> Path Abs Dir -> Path Rel Dir -> WatchDepth -> IO a -> IO a
fileWatch1 l man act cwd fp d go =
  (case d of Deep -> FSN.watchTree; Shallow -> FSN.watchDir) man (Path.toFilePath (cwd Path.</> fp)) (const True) action >>= \stop ->
    go `finally` stop
  where
    action :: FSN.Action
    action e = case g e of
      Just f ->
        -- https://github.com/haskell-fswatch/hfsnotify/issues/91
        act f `catchAny` l
      Nothing -> pure ()

    g = (Path.parseAbsFile >=> Path.stripProperPrefix cwd) . FSN.eventPath
