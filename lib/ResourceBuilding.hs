module ResourceBuilding where

import Control.Lens
import qualified Data.Text.Lazy.Builder as Text.Builder
import FileLayout
import qualified Home
import qualified HtmlRendering as H
import qualified Menus
import Path
import qualified Prosidy
import ProsidyHtml
import Relude
import Resource
import StateOfResources
import qualified Tags

ensureResourceBuilt :: Scheme -> (Text -> IO ()) -> StateOfResources Resource -> Resource -> IO ()
ensureResourceBuilt scheme l rs r =
  maybe (pure ()) id $
    do
      r' <- resourceAsProHtml scheme r
      Just (StateOfResources.ensureResourceBuilt (buildProHtmlResource scheme l r') rs r)

buildProHtmlResource :: Scheme -> (Text -> IO ()) -> ProHtmlResource -> IO ()
buildProHtmlResource scheme l (ProHtmlResource r (InputPath fpIn) (OutputPath fpOut) _) =
  do
    l ("Building " <> resourceUrl r)

    src <- readFileBS (Path.toFilePath fpIn)

    let opts :: ProHtmlOpts IO =
          if
              | r == [res||] -> defaultOpts & appEndo (Home.proHtmlOpts scheme)
              | r == [res|menus|] -> defaultOpts & appEndo (Menus.proHtmlOpts scheme)
              | r == [res|tags|] -> defaultOpts & appEndo (Tags.proHtmlOpts scheme)
              | otherwise -> defaultOpts

    let f = fmap (encodeUtf8 . Text.Builder.toLazyText . H.renderHtml) . proHtml opts . Prosidy.parseDocument (toFilePath fpIn) . decodeUtf8

    writeFileLBS (Path.toFilePath fpOut) =<< f src
