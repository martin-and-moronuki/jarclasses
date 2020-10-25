module Deploy where

import Relude
import System.Process
import qualified BuildAll

main :: IO ()
main = BuildAll.main *> deploy

deploy :: IO ()
deploy = callProcess "rsync" ["--recursive", "--progress", "--human-readable", "--verbose", "--compress", "--rsh=ssh -p 36411", "all/", "chris-martin.org:/var/www/jarclasses.com/"]
