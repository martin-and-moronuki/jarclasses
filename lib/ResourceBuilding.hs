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
buildProHtmlResource l (ProHtmlResource r (InputPath fpIn) (OutputPath fpOut) _) =
  do
    l ("Building " <> show r)
    src <- readFileBS (Path.toFilePath fpIn)
    writeFileLBS (Path.toFilePath fpOut) (f src)
  where
    f = encodeUtf8 . toText . renderHtml . proHtml . Prosidy.parseDocument (Path.toFilePath fpIn) . decodeUtf8
