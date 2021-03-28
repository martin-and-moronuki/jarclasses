module ResourceEnumeration where

import FileLayout
import Path
import Path.IO
import Pipes
import Relude

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
