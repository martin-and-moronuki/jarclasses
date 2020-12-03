module ResourceBuilding where

import BlazeHtmlRendering
import Control.Lens
import FileLayout
import qualified Home
import qualified Menus
import Path
import qualified Prosidy
import ProsidyHtml
import Relude
import Resource
import StateOfResources
import StringBuilding
import qualified Text.Blaze.Html5 as HTML

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

    opts <-
      if
          | r == [res||] ->
            do
              list <- Home.listOfContent scheme
              pure
                defaultOpts
                  { extraBlockTags = \x ->
                      case (Prosidy.tagName x) of
                        "list-of-content" -> Just $
                          case view (Prosidy.atSetting "limit") x of
                            Nothing -> list Nothing
                            Just t ->
                              case readMaybe @Natural (toString t) of
                                Nothing -> HTML.stringComment $ "limit must be a Natural, but is" <!> show t
                                Just n -> list (Just n)
                        _ -> Nothing,
                    extraStyle = one [res|style/home.css|]
                  }
          | r == [res|menus|] ->
            do
              list <- Menus.listOfContent scheme
              pure
                defaultOpts
                  { extraBlockTags = \x ->
                      case (Prosidy.tagName x) of
                        "list-of-content" -> Just list
                        _ -> Nothing
                  }
          | otherwise -> pure defaultOpts

    let f = encodeUtf8 . toText . renderHtml . proHtml opts . Prosidy.parseDocument (toFilePath fpIn) . decodeUtf8

    writeFileLBS (Path.toFilePath fpOut) (f src)
