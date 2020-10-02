module Scheme where

import ResourcePaths
import Relude
import Path

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
