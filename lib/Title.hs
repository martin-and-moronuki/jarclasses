module Title where

import Control.Lens
import FileLayout
import qualified HtmlTypes as H
import Path
import qualified Prosidy
import Relude
import Resource

resourceTitleHtml :: Scheme -> Resource -> IO (Maybe (H.Series H.Inline))
resourceTitleHtml scheme = maybe (pure Nothing) prhTitleHtml . resourceAsProHtml scheme

inputPathDoc :: InputPath -> IO (Maybe Prosidy.Document)
inputPathDoc (InputPath (toFilePath -> x)) = fmap (bsDoc x) (readFileBS x)

bsDoc :: FilePath -> ByteString -> Maybe Prosidy.Document
bsDoc fp = either (const Nothing) Just . Prosidy.parseDocument fp . decodeUtf8

proTitle :: Prosidy.Document -> Maybe Text
proTitle = view (Prosidy.atSetting "title")

prhTitleHtml :: ProHtmlResource -> IO (Maybe (H.Series H.Inline))
prhTitleHtml = fmap (>>= (fmap (one . H.InlineFragment) . proTitle)) . inputPathDoc . proHtmlInputPath
