module FileWatch where

import Control.Exception.Safe
import Relude
import qualified System.FSNotify as FSN
import Path (Abs, Dir, File, Path, Rel)
import qualified Path

fsnConfig :: FSN.WatchConfig
fsnConfig = FSN.defaultConfig {FSN.confDebounce = FSN.NoDebounce}

withWatches :: (SomeException -> IO ()) -> FSN.WatchManager -> (Path Rel File -> IO ()) -> Path Abs Dir -> [Path Rel Dir] -> IO a -> IO a
withWatches l man act cwd = fix \r ->
  \case
    [] -> id
    fp : fps -> withWatch l man act cwd fp . r fps

withWatch :: (SomeException -> IO ()) -> FSN.WatchManager -> (Path Rel File -> IO ()) -> Path Abs Dir -> Path Rel Dir -> IO a -> IO a
withWatch l man act cwd fp go =
  FSN.watchTree man (Path.toFilePath (cwd Path.</> fp)) (const True) action >>= \stop ->
    go `finally` stop
  where
    action :: FSN.Action
    action e = case g e of
      Just f ->
        -- https://github.com/haskell-fswatch/hfsnotify/issues/91
        act f `catchAny` l
      Nothing -> pure ()

    g = (Path.parseAbsFile >=> Path.stripProperPrefix cwd) . FSN.eventPath
