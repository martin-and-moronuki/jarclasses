module Format where

import Data.Foldable
import qualified Data.Text.IO as T
import Ormolu
import Path
import Path.IO
import Pipes
import qualified Pipes.Prelude as Pipes
import Relude

main :: IO ()
main = runEffect $ files >-> Pipes.mapM_ format

files, rootFiles, libFiles :: Producer (Path Rel File) IO ()
files = rootFiles *> libFiles
rootFiles =
  flip walkDirRel [reldir|.|] \_ _ xs ->
    do
      traverse_ (\x -> when (isHs x) (yield x)) xs
      pure WalkFinish
libFiles =
  flip walkDirRel [reldir|lib|] \_ _ xs ->
    do
      traverse_ (\x -> when (isHs x) $ yield $ [reldir|lib|] </> x) xs
      pure $ WalkExclude []

isHs :: Path Rel File -> Bool
isHs x = fileExtension x == Just ".hs"

format :: Path Rel File -> IO ()
format fp =
  do
    putStrLn (toFilePath fp)
    T.readFile (toFilePath fp) >>= \input ->
      ormolu defaultConfig (toFilePath fp) (toString input) >>= \output ->
        T.writeFile (toFilePath fp) output
