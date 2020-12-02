module Title where

import Control.Lens
import FileLayout
import Path
import qualified Prosidy
import Relude
import Resource
import Text.Blaze.Html (Html, toHtml)

resourceTitleHtml :: Scheme -> Resource -> IO (Maybe Html)
resourceTitleHtml scheme = maybe (pure Nothing) prhTitleHtml . resourceAsProHtml scheme

inputPathDoc :: InputPath -> IO (Maybe Prosidy.Document)
inputPathDoc (InputPath (toFilePath -> x)) = fmap (bsDoc x) (readFileBS x)

bsDoc :: FilePath -> ByteString -> Maybe Prosidy.Document
bsDoc fp = either (const Nothing) Just . Prosidy.parseDocument fp . decodeUtf8

proTitle :: Prosidy.Document -> Maybe Text
proTitle = view (Prosidy.atSetting "title")

prhTitleHtml :: ProHtmlResource -> IO (Maybe Html)
prhTitleHtml = fmap (>>= (fmap toHtml . proTitle)) . inputPathDoc . proHtmlInputPath
