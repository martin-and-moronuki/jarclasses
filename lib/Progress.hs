module Progress where

import Control.Lens
import FileLayout
import Path
import qualified Prosidy
import Relude
import Resource

data Progress = Published | Draft deriving (Eq)

resourceProgress :: Scheme -> Resource -> IO (Maybe Progress)
resourceProgress scheme = maybe (pure Nothing) prhProgress . resourceAsProHtml scheme

inputPathDoc :: InputPath -> IO (Maybe Prosidy.Document)
inputPathDoc (InputPath (toFilePath -> x)) = fmap (bsDoc x) (readFileBS x)

bsDoc :: FilePath -> ByteString -> Maybe Prosidy.Document
bsDoc fp = either (const Nothing) Just . Prosidy.parseDocument fp . decodeUtf8

prhProgress :: ProHtmlResource -> IO (Maybe Progress)
prhProgress = fmap (fmap docProgress) . inputPathDoc . proHtmlInputPath

docProgress :: Prosidy.Document -> Progress
docProgress doc =
  if view (Prosidy.hasProperty "draft") doc
    then Draft
    else Published
