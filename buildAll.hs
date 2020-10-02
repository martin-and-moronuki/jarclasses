#! /usr/bin/env nix-shell
#! nix-shell -i ./runhaskell.hs shell.nix

module BuildAll where

import Relude
import ResourcePaths
import Pipes
import qualified Pipes.Prelude as Pipes
import Scheme
import ResourceBuilding

main :: IO ()
main = runEffect $ findProHtmlResources scheme >-> Pipes.mapM_ (buildResource putStrLn)
