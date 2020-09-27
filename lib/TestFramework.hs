module TestFramework where

import Relude
import Path

type Test = Seq Text

writeTestFile :: Test -> Path Abs Dir -> IO ()
writeTestFile t dir = writeFileLBS path (encodeUtf8 txt)
  where
    path = toFilePath (dir Path.</> [relfile|test.txt|])
    txt = unlines (toList t)
