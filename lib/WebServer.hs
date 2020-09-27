module WebServer where

import qualified Network.HTTP.Types as HTTP
import Network.Wai (Request, Response)
import qualified Network.Wai as WAI
import qualified Network.Wai.Handler.Warp as Warp
import qualified Path
import Relude
import ResourcePaths

serve :: (Resource -> IO ()) -> IO ()
serve build = Warp.runEnv 8000 (webapp build)

webapp :: (Resource -> IO ()) -> WAI.Application
webapp build request respond = build r *> go
  where
    r = requestResource request
    go = case (responseResource r) of
      Just response -> respond response
      Nothing -> undefined

requestResource :: Request -> Resource
requestResource = WAI.pathInfo

responseResource :: Resource -> Maybe Response
responseResource r =
  resourceOutputPath r >>= \fp ->
    pure $ WAI.responseFile HTTP.ok200 headers (Path.toFilePath fp) Nothing
  where
    headers = [(HTTP.hContentType, resourceContentType r)]

resourceContentType :: Resource -> ByteString
resourceContentType = \case
  ("menus" : _) -> "text/html; charset=utf-8"
  ("posts" : _) -> "text/html; charset=utf-8"
  ("style" : _) -> "text/css"
