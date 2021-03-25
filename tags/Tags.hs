module Tags where

import Chronology
import Control.Lens
import Data.Time
import FileLayout
import qualified HtmlBuilding as H
import qualified HtmlTypes as H
import qualified Pipes.Prelude as Pipes
import Progress
import Relude hiding (head)
import Resource
import Title

listOfTags :: Scheme -> IO (H.Series H.Block)
listOfTags scheme = return mempty
