module TestFramework where

import Path
import Path.IO
import Relude
import qualified Data.Map as Map
import Data.Map.Append

newtype Test = Test (Map (Path Rel File) (Seq Text))
  deriving (Semigroup, Monoid)
    via AppendMap (Path Rel File) (Seq Text)

instance One Test
  where
    type OneItem Test = (Path Rel File, Text)
    one (file, line) = Test (Map.singleton file (one line))

writeTestFiles :: Test -> Path Abs Dir -> IO ()
writeTestFiles (Test (Map.toAscList -> entries)) dir = traverse_ f entries
  where
    f (file, toList -> tests) =
      do
        createDirIfMissing True (parent path)
        writeFileLBS (toFilePath path) (encodeUtf8 (unlines tests))
      where
        path = dir </> [reldir|test|] </> file
