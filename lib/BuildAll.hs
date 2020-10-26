module BuildAll where

import FileLayout
import FileLayoutPro
import Path
import Path.IO
import Pipes
import qualified Pipes.Prelude as Pipes
import Relude
import ResourceBuilding
import Style
import System.Directory (getCurrentDirectory)
import qualified Test
import TestFramework (writeTestFiles)

outDir :: Path Rel Dir
outDir = [reldir|all|]

main :: IO ()
main =
  do
    cwd <- getCurrentDirectory >>= Path.parseAbsDir

    ensureDirGone outDir

    makeStyles cwd

    scheme <- getScheme

    writeTestFiles (Test.test scheme) cwd

    runEffect $
      findProHtmlResources scheme
        >-> Pipes.mapM_ (buildProHtmlResource putStrLn)

    runEffect $
      ( ( findProHtmlResources scheme
            >-> Pipes.map (\(ProHtmlResource r _ _ _) -> r)
        )
          *> styleResources
      )
        >-> Pipes.mapM_ (copyResource scheme)

copyResource :: Scheme -> Resource -> IO ()
copyResource scheme r =
  for_ @Maybe (resourceOutputPath scheme r) \(OutputPath outputPath) ->
    for_ @Maybe (resourceDeployPath scheme r) \(DeployPath deployPath) ->
      do
        let deployPath' = outDir </> deployPath
        createDirIfMissing True (parent deployPath')
        copyFile outputPath deployPath'

ensureDirGone :: Path Rel Dir -> IO ()
ensureDirGone d = doesDirExist d >>= \x -> when x $ removeDirRecur d
