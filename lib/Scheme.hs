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
          ]
    }
