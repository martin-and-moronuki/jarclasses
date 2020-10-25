module WebServer where

import qualified Network.HTTP.Types as HTTP
import Network.Wai (Request, Response)
import qualified Network.Wai as WAI
import qualified Network.Wai.Handler.Warp as Warp
import qualified Path
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
    pure $ WAI.responseFile HTTP.ok200 headers (Path.toFilePath fp) Nothing
  where
    headers = maybe [] (\ct -> [(HTTP.hContentType, ct)]) $ resourceContentType r

resourceContentType :: Resource -> Maybe ByteString
resourceContentType = \case
  ("menus" : _) -> Just "text/html; charset=utf-8"
  ("posts" : _) -> Just "text/html; charset=utf-8"
  ("style" : _) -> Just "text/css"
  _ -> Nothing
