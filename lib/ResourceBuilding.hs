module ResourceBuilding where

import Relude
import StateOfResources
import ResourcePaths
import qualified Path
import qualified Prosidy
import Scheme
import BlazeHtmlRendering
import ProsidyHtml

ensureResourceBuilt :: (String -> IO ()) -> StateOfResources Resource -> Resource -> IO ()
ensureResourceBuilt l rs r =
  if isJust (resourceInputPath scheme r)
    then StateOfResources.ensureResourceBuilt (buildResource l r) rs r
    else pure ()

buildResource :: (String -> IO ()) -> Resource -> IO ()
buildResource l r =
  do
    l $ "Building " <> show r
    fpIn <- maybe undefined pure $ resourceInputPath scheme r
    fpOut <- maybe undefined pure $ resourceOutputPath scheme r
    src <- decodeUtf8 <$> readFileBS (Path.toFilePath fpIn)
    doc <- either (fail . show) pure $ Prosidy.parseDocument (Path.toFilePath fpIn) src
    writeFileLBS (Path.toFilePath fpOut) $ encodeUtf8 $ toText $ renderHtml $ proHtml doc
