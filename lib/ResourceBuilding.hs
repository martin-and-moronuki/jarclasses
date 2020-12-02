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
      Just (StateOfResources.ensureResourceBuilt (buildProHtmlResource scheme l r') rs r)

buildProHtmlResource :: Scheme -> (String -> IO ()) -> ProHtmlResource -> IO ()
buildProHtmlResource scheme l (ProHtmlResource r (InputPath fpIn) (OutputPath fpOut) _) =
  do
    l ("Building " <> show r)

    src <- readFileBS (Path.toFilePath fpIn)

    opts <-
      if
          | r == [res||] ->
            do
              list <- Home.listOfContent scheme
              pure
                defaultOpts
                  { extraBlockTags = \x ->
                      case (Prosidy.tagName x) of
                        "list-of-content-on-the-home-page" -> Just list
                        _ -> Nothing
                  }
          | otherwise -> pure defaultOpts

    let f = encodeUtf8 . toText . renderHtml . proHtml opts . Prosidy.parseDocument (toFilePath fpIn) . decodeUtf8

    writeFileLBS (Path.toFilePath fpOut) (f src)
