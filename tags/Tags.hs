module Tags where

import Chronology
import Control.Lens
import Data.Time
import FileLayout
import qualified HtmlBuilding as H
import qualified HtmlTypes as H
import qualified Pipes.Prelude as Pipes
import Progress
import qualified Prosidy
import ProsidyHtml (BlockTagOpt (..), ProHtmlOpts, addBlockTag)
import Relude hiding (head)
import Resource
import Title

proHtmlOpts :: Scheme -> Endo (ProHtmlOpts IO)
proHtmlOpts s = addBlockTag (listOfTagsTag s)

listOfTagsTag :: Scheme -> BlockTagOpt IO
listOfTagsTag s = BlockTagOpt $ \_ x ->
  case (Prosidy.tagName x) of
    "list-of-tags" -> Just $ Tags.listOfTags s
    _ -> Nothing

listOfTags :: Scheme -> IO (H.Series H.Block)
listOfTags scheme = return mempty
