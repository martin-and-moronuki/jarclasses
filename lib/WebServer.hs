module WebServer where

import qualified Network.HTTP.Types as HTTP
import Network.Wai (Request, Response)
import qualified Network.Wai as WAI
import qualified Network.Wai.Handler.Warp as Warp
import Path
import Relude
import ResourcePaths

serve :: Scheme -> (Resource -> IO ()) -> IO ()
serve s build = Warp.runEnv 8000 (webapp s build)

webapp :: Scheme -> (Resource -> IO ()) -> WAI.Application
webapp s build request respond = build r *> go
  where
    r = requestResource request
    go = case (responseResource s r) of
      Just response -> respond response
      Nothing -> undefined

requestResource :: Request -> Resource
requestResource = WAI.pathInfo

responseResource :: Scheme -> Resource -> Maybe Response
responseResource s r =
  resourceOutputPath s r >>= \fp ->
    pure $ WAI.responseFile HTTP.ok200 (headers s fp) (Path.toFilePath fp) Nothing

headers :: Scheme -> Path Rel t -> [(HTTP.HeaderName, ByteString)]
headers s fp =
  maybe [] (\ct -> [(HTTP.hContentType, ct)]) (resourceContentType s fp)

resourceContentType :: Scheme -> Path Rel t -> Maybe ByteString
resourceContentType s fp
  | inProHtmlDir s fp = Just "text/html; charset=utf-8"
  | inStyleDir s fp = Just "text/css"
  | otherwise = Nothing
