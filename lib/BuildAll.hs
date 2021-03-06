module BuildAll where

import FileLayout
import FileLayoutPro
import Path
import Path.IO
import Pipes
import Relude
import Resource
import ResourceBuilding
import ResourceEnumeration
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

    runListT $
      do
        r <- findProHtmlResources scheme
        lift $ buildProHtmlResource scheme putTextLn r

    runListT $
      do
        ProHtmlResource r _ _ _ <- findProHtmlResources scheme
        lift $ copyResource scheme r

    runListT $
      do
        r <- styleResources
        lift $ copyResource scheme r

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
