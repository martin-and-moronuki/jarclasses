module BuildAll where

import Path
import Path.IO
import Pipes
import qualified Pipes.Prelude as Pipes
import Relude
import ResourceBuilding
import ResourcePaths
import Scheme
import Style

outDir :: Path Rel Dir
outDir = [reldir|all|]

main :: IO ()
main =
  do
    ensureDirGone outDir

    runEffect $
      findProHtmlResources scheme
        >-> Pipes.mapM_ (buildResource putStrLn)

    runEffect $
      (findProHtmlResources scheme *> styleResources)
        >-> Pipes.mapM_ copyResource

copyResource :: Resource -> IO ()
copyResource r =
  for_ (resourceOutputPath scheme r) \p ->
    do
      let p' = outDir </> p
      createDirIfMissing True (parent p')
      copyFile p p'

ensureDirGone :: Path Rel Dir -> IO ()
ensureDirGone d = doesDirExist d >>= \x -> when x $ removeDirRecur d
