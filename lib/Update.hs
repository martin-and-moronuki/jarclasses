{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Update where

import Data.Aeson
import Data.Aeson.Encode.Pretty
import qualified Data.Aeson.Encode.Pretty as AP
import Data.Aeson.Optics
import qualified Data.ByteString.Lazy as LBS
import qualified Data.HashMap.Lazy as HashMap
import qualified Data.Text as T
import Data.Time.Calendar
import Data.Time.Clock
import Data.Time.Format
import Data.Time.LocalTime
import Network.HTTP.Simple
import Network.HTTP.Types.Header
import Optics
import Relude
import System.Process

main :: IO ()
main = read file >>= either fail pure >>= traverse update >>= write file
  where
    file = "versions.json"

read :: FilePath -> IO (Either String (Map Text Value))
read = eitherDecodeFileStrict

write :: FilePath -> (Map Text Value) -> IO ()
write file = LBS.writeFile file . encodePretty' conf
  where
    conf = AP.defConfig {AP.confIndent = AP.Spaces 2, AP.confTrailingNewline = True}

update :: Value -> IO Value
update v = (updateRev >=> updateHash) v >>= \v' -> if v == v' then pure v' else updateTime v'

updateRev :: Value -> IO Value
updateRev x = setRev <$> getRev <*> pure x
  where
    request = gitHubRequest <$> s "owner" <*> s "repo" <*> s "branch"
    getRev = request >>= httpJSON >>= gitHubResponseRev
    s k = forceKey k _String x

setRev :: Text -> Value -> Value
setRev x = over _Object (HashMap.insert "rev" (review _String x))

updateHash :: Value -> IO Value
updateHash x = setHash <$> getHash <*> pure x
  where
    url = gitHubArchive <$> s "owner" <*> s "repo" <*> s "rev"
    getHash = url >>= nixPrefetchUrl
    s k = forceKey k _String x

setHash :: Text -> Value -> Value
setHash x = over _Object (HashMap.insert "sha256" (review _String x))

nixPrefetchUrl :: Text -> IO Text
nixPrefetchUrl url = T.strip . toText <$> readProcess "nix-prefetch-url" ["--unpack", toString url] ""

gitHubRequest :: Text -> Text -> Text -> Request
gitHubRequest owner repo branch =
  defaultRequest
    & setRequestCheckStatus
    & setRequestSecure True
    & setRequestPort 443
    & setRequestHost (encodeUtf8 ("api.github.com" :: Text))
    & setRequestPath (encodeUtf8 $ foldMap ("/" <>) ["repos", owner, repo, "branches", branch])
    & addRequestHeader hUserAgent (encodeUtf8 gitHubUserAgent)

gitHubUserAgent :: Text
gitHubUserAgent = "github.com/martin-and-moronuki/martin-moronuki-cloud"

gitHubResponseRev :: Response Value -> IO Text
gitHubResponseRev = forceKey' ["commit", "sha"] _String . getResponseBody

gitHubArchive :: Text -> Text -> Text -> Text
gitHubArchive owner repo rev =
  "https://github.com" <> foldMap ("/" <>) [owner, repo, "archive", rev <> ".tar.gz"]

updateTime :: Value -> IO Value
updateTime x = setTime <$> getTime <*> pure x

getTime :: IO Day
getTime = localDay <$> (utcToLocalTime <$> getCurrentTimeZone <*> getCurrentTime)

setTime :: Day -> Value -> Value
setTime x = over _Object (HashMap.insert "lastUpdated" (review _String (formatDay x)))

formatDay :: Day -> Text
formatDay = toText . formatTime defaultTimeLocale "%Y-%m-%d"

forceKey :: Text -> Prism' Value a -> Value -> IO a
forceKey k p v = orErr $ preview (key k % p) v
  where
    orErr = maybe (fail err) pure
    err = "JSON problem at" ! show k ! "in value" ! show v

forceKey' :: [Text] -> Prism' Value a -> Value -> IO a
forceKey' ks p v = orErr $ preview (pathOptic % p) v
  where
    pathOptic :: AffineTraversal' Value Value
    pathOptic = foldr (\k o -> key k % o) (castOptic simple) ks
    orErr = maybe (fail err) pure
    err = "JSON problem at" ! intercalate "." (map show ks) ! "in value" ! show v

(!) :: (IsString a, Semigroup a) => a -> a -> a
a ! b = a <> " " <> b
