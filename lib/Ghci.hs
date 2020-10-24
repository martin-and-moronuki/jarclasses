module Ghci where

import Haskell
import Path
import Path.IO
import Relude
import System.Process

main :: IO ()
main = getTargets >>= \targets -> callProcess "ghci" (["-ignore-dot-ghci", "-Wall", "-fdefer-typed-holes", "-ilib"] <> extensionFlags <> targets)

getTargets :: IO [String]
getTargets = fmap (\(_, xs) -> mapMaybe pathModule xs) $ listDirRecurRel [reldir|lib|]

pathModule :: Path Rel File -> Maybe String
pathModule x =
  case splitExtension x of
    Just (y, ".hs") -> Just $ map slashToDot $ toFilePath y
    _ -> Nothing

slashToDot :: Char -> Char
slashToDot = \case '/' -> '.'; c -> c
