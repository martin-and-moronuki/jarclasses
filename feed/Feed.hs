module Feed where

import Path
import Relude
import FileLayout
import RSS

makeFeeds :: Scheme -> Path Abs Dir -> IO ()
makeFeeds = makeRSS
