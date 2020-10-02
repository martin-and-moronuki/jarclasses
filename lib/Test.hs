module Test where

import qualified ResourcePaths
import TestFramework

test :: ResourcePaths.Scheme -> Test
test s = ResourcePaths.test s
