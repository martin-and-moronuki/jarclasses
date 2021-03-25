module ResourceBuilding where

import Control.Lens
import qualified Data.Text.Lazy.Builder as Text.Builder
import FileLayout
import qualified Home
import qualified HtmlBuilding as H
import qualified HtmlRendering as H
import qualified HtmlTypes as H
import qualified Menus
import Path
import qualified Prosidy
import ProsidyHtml
import Relude
import Resource
import StateOfResources
import StringBuilding
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
            | r == [res||] ->
                  defaultOpts
                    { extraBlockTags = \_ x ->
                        case (Prosidy.tagName x) of
                          "list-of-content" -> Just $
                            case view (Prosidy.atSetting "limit") x of
                              Nothing -> Home.listOfContent scheme Nothing
                              Just t ->
                                case readMaybe @Natural (toString t) of
                                  Nothing -> pure $ H.toBlocks $ H.Comment $ "limit must be a Natural, but is" <!> show t
                                  Just n -> Home.listOfContent scheme (Just n)
                          _ -> Nothing,
                      extraStyle = one [res|style/home.css|]
                    }
            | r == [res|menus|] ->
              do
                  defaultOpts
                    { extraBlockTags = \_ x ->
                        case (Prosidy.tagName x) of
                          "list-of-content" -> Just $ Menus.listOfContent scheme
                          _ -> Nothing
                    }
            | r == [res|tags|] ->
              do
                  defaultOpts
                    { extraBlockTags = \_ x ->
                        case (Prosidy.tagName x) of
                          "list-of-tags" -> Just $ Tags.listOfTags scheme
                          _ -> Nothing
                    }
            | otherwise -> defaultOpts

    let f = fmap (encodeUtf8 . Text.Builder.toLazyText . H.renderHtml) . proHtml opts . Prosidy.parseDocument (toFilePath fpIn) . decodeUtf8

    writeFileLBS (Path.toFilePath fpOut) =<< f src
