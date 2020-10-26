module Scheme where

import Path
import Relude
import ResourcePaths

scheme :: Scheme
scheme =
  Scheme
    { scheme_proHtmlDirs =
        fromList
          [ [reldir|menus|],
            [reldir|posts|]
          ],
      scheme_styleDirs =
        fromList
          [ [reldir|style|]
          ],
      scheme_otherProHtmlResources =
        fromList
          [ ProHtmlResource
              []
              (InputPath [relfile|home/home.pro|])
              (OutputPath [relfile|home/home.html|])
              (DeployPath [relfile|index.html|]),
            ProHtmlResource
              ["menus"]
              (InputPath [relfile|menus/menus.pro|])
              (OutputPath [relfile|menus/menus.html|])
              (DeployPath [relfile|menus/index.html|])
          ]
    }
