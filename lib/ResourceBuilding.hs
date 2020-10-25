module ResourceBuilding where

import BlazeHtmlRendering
import Path
import qualified Prosidy
import ProsidyHtml
import Relude
import ResourcePaths
import Scheme
import StateOfResources

ensureResourceBuilt :: (String -> IO ()) -> StateOfResources Resource -> Resource -> IO ()
ensureResourceBuilt l rs r =
  maybe (pure ()) id $
    do
      r' <- resourceAsProHtml scheme r
      Just (StateOfResources.ensureResourceBuilt (buildProHtmlResource l r') rs r)

buildProHtmlResource :: (String -> IO ()) -> ProHtmlResource -> IO ()
buildProHtmlResource l (ProHtmlResource r fpIn fpOut) =
  do
    l $ "Building " <> show r
    src <- decodeUtf8 <$> readFileBS (Path.toFilePath fpIn)
    doc <- either (fail . show) pure $ Prosidy.parseDocument (Path.toFilePath fpIn) src
    writeFileLBS (Path.toFilePath fpOut) $ encodeUtf8 $ toText $ renderHtml $ proHtml doc
