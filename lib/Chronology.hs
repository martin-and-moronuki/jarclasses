module Chronology where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Time
import FileLayout
import Pipes
import qualified Pipes.Prelude as Pipes
import Relude
import Resource
import ResourceEnumeration
import qualified Text.Regex.Applicative as RE
import qualified Text.Regex.Applicative.Common as RE

findRecentResources :: Scheme -> (Resource -> Bool) -> ListT IO Resource
findRecentResources scheme resourcePredicate = Pipes.lift (getResourceDayMap scheme resourcePredicate) >>= yieldFromSetMapDesc

foldMappings :: (Monad m, Ord a, Ord b) => ListT m (a, b) -> m (Map a (Set b))
foldMappings = Pipes.fold insert Map.empty id . Pipes.enumerate
  where
    insert m (a, b) = Map.insertWith (<>) a (one b) m

resourceDay :: Scheme -> Resource -> IO (Maybe Day)
resourceDay scheme = maybe (pure Nothing) proHtmlResourceDay . resourceAsProHtml scheme

proHtmlResourceDay :: ProHtmlResource -> IO (Maybe Day)
proHtmlResourceDay phr = pure $ dateFromResourceId $ proHtmlResourceId phr

getResourceDayMap :: Scheme -> (Resource -> Bool) -> IO (Map Day (Set Resource))
getResourceDayMap scheme resourcePredicate =
  foldMappings $
    do
      r <- findProHtmlResources scheme
      case resourcePredicate (proHtmlResourceId r) of
        False -> mempty
        True ->
          do
            dMay <- lift $ proHtmlResourceDay r
            case dMay of
              Nothing -> mempty
              Just d -> return (d, proHtmlResourceId r)

yieldFromSetMapDesc :: Monad m => Map a (Set b) -> ListT m b
yieldFromSetMapDesc = Pipes.Select . traverse_ (traverse_ yield . Set.toDescList . snd) . Map.toDescList

dateFromResourceId :: Resource -> Maybe Day
dateFromResourceId (ResourceSlashList xs) = viaNonEmpty head (mapMaybe parseDay xs)

parseDay :: Text -> Maybe Day
parseDay txt = join $ RE.match e (toString txt)
  where
    e =
      pure fromGregorianValid
        <*> RE.decimal
        <*> (RE.sym '-' *> RE.decimal)
        <*> (RE.sym '-' *> RE.decimal)
        <* many (RE.sym '-' <* many RE.anySym)
