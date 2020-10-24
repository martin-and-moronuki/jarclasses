module Deploy where

import Relude
import System.Process

main :: IO ()
main = callProcess "rsync" ["--recursive", "--progress", "--human-readable", "--verbose", "--compress", "--rsh=ssh -p 36411", "all/", "chris-martin.org:/var/www/jarclasses.com/"]
