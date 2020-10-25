module WebServer where

import qualified ASCII.QuasiQuoters as ASCII
import qualified Network.HTTP.Types as HTTP
import Network.Wai (Request, Response)
import qualified Network.Wai as WAI
import qualified Network.Wai.Handler.Warp as Warp
import Path
import Path.IO
import Relude
import ResourcePaths

serve :: Scheme -> (Resource -> IO ()) -> IO ()
serve s build = Warp.runEnv 8000 (webapp s build)

webapp :: Scheme -> (Resource -> IO ()) -> WAI.Application
webapp s build request respond = build r *> go
  where
    r = requestResource request
    go = responseResource s r >>= \case
      Just response -> respond response
      Nothing -> respond notFound

notFound :: Response
notFound = WAI.responseLBS HTTP.notFound404 headers body
  where
    headers = [(HTTP.hContentType, [ASCII.string|text/plain; charset=us-ascii|])]
    body = [ASCII.string|Not found|]

requestResource :: Request -> Resource
requestResource = WAI.pathInfo

responseResource :: Scheme -> Resource -> IO (Maybe Response)
responseResource s r =
  case resourceOutputPath s r of
    Nothing -> pure Nothing
    Just fp -> doesFileExist fp >>= \case
      True -> pure $ Just $ WAI.responseFile HTTP.ok200 (resourceHeaders s fp) (Path.toFilePath fp) Nothing
      False -> pure Nothing

resourceHeaders :: Scheme -> Path Rel t -> [(HTTP.HeaderName, ByteString)]
resourceHeaders s fp =
  maybe [] (\ct -> [(HTTP.hContentType, ct)]) (resourceContentType s fp)

resourceContentType :: Scheme -> Path Rel t -> Maybe ByteString
resourceContentType s fp
  | inProHtmlDir s fp = Just [ASCII.string|text/html; charset=utf-8|]
  | inStyleDir s fp = Just [ASCII.string|text/css|]
  | otherwise = Nothing
