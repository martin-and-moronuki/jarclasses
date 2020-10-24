module BuildAll where

import Pipes
import qualified Pipes.Prelude as Pipes
import Relude
import ResourceBuilding
import ResourcePaths
import Scheme

main :: IO ()
main = runEffect $ findProHtmlResources scheme >-> Pipes.mapM_ (buildResource putStrLn)