module Deploy where

import qualified BuildAll
import Relude
import System.Process

main :: IO ()
main = BuildAll.main *> deploy

deploy :: IO ()
deploy = callProcess "rsync" ["--recursive", "--progress", "--human-readable", "--verbose", "--compress", "--rsh=ssh -p 36411", "all/", "chris-martin.org:/var/www/jarclasses.com/"]
