module BuildAll where

import Path
import Path.IO
import Pipes
import qualified Pipes.Prelude as Pipes
import Relude
import ResourceBuilding
import ResourcePaths
import Scheme

main :: IO ()
main =
  do
    runEffect $
        findProHtmlResources scheme
        >-> Pipes.mapM_ (buildResource putStrLn)
    runEffect $
        findProHtmlResources scheme
        >-> Pipes.mapM_ linkResource

linkResource :: Resource -> IO ()
linkResource r =
  for_ (resourceOutputPath scheme r) \p ->
    do
      let p' = [reldir|links|] </> p
      createDirIfMissing True (parent p')
      createFileLink p p'
