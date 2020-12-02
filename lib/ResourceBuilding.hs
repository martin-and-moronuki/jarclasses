module ResourceBuilding where

import BlazeHtmlRendering
import FileLayout
import Home
import Path
import qualified Prosidy
import ProsidyHtml
import Relude
import Resource
import StateOfResources

ensureResourceBuilt :: Scheme -> (String -> IO ()) -> StateOfResources Resource -> Resource -> IO ()
ensureResourceBuilt scheme l rs r =
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
    f = encodeUtf8 . toText . renderHtml . proHtml opts . Prosidy.parseDocument (toFilePath fpIn) . decodeUtf8
    opts
      | r == [res||] = defaultOpts {extraBlockTags = \x -> case (Prosidy.tagName x) of "list-of-content-on-the-home-page" -> Just Home.listOfContent; _ -> Nothing}
      | otherwise = defaultOpts
