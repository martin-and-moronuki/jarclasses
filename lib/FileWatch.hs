module FileWatch where

import Control.Exception.Safe
import Path (Abs, Dir, File, Path, Rel)
import qualified Path
import Relude
import qualified System.FSNotify as FSN

fsnConfig :: FSN.WatchConfig
fsnConfig = FSN.defaultConfig {FSN.confDebounce = FSN.NoDebounce}

fileWatch :: (SomeException -> IO ()) -> (Path Rel File -> IO ()) -> Path Abs Dir -> [Path Rel Dir] -> IO a -> IO a
fileWatch l act cwd fps go = FSN.withManagerConf fsnConfig \man -> fileWatchN l man act cwd fps go

fileWatchN :: (SomeException -> IO ()) -> FSN.WatchManager -> (Path Rel File -> IO ()) -> Path Abs Dir -> [Path Rel Dir] -> IO a -> IO a
fileWatchN l man act cwd = fix \r ->
  \case
    [] -> id
    fp : fps -> fileWatch1 l man act cwd fp . r fps

fileWatch1 :: (SomeException -> IO ()) -> FSN.WatchManager -> (Path Rel File -> IO ()) -> Path Abs Dir -> Path Rel Dir -> IO a -> IO a
fileWatch1 l man act cwd fp go =
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
