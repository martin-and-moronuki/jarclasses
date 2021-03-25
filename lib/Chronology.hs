module Chronology where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Time
import FileLayout
import Pipes
import qualified Pipes.Prelude as Pipes
import Relude
import Resource
import qualified Text.Regex.Applicative as RE
import qualified Text.Regex.Applicative.Common as RE

findRecentResources :: Scheme -> (Resource -> Bool) -> ListT IO Resource
findRecentResources scheme resourcePredicate = Pipes.lift (getResourceDayMap scheme resourcePredicate) >>= yieldFromSetMapDesc

pipeMapMaybeM :: Monad m => (a -> m (Maybe b)) -> Pipe a b m r
pipeMapMaybeM f = forever $ await >>= (Pipes.lift . f) >>= traverse_ yield

foldMappings :: (Monad m, Ord a, Ord b) => Producer (a, b) m () -> m (Map a (Set b))
foldMappings = Pipes.fold insert Map.empty id
  where
    insert m (a, b) = Map.insertWith (<>) a (one b) m

resourceDay :: Scheme -> Resource -> IO (Maybe Day)
resourceDay scheme = maybe (pure Nothing) proHtmlResourceDay . resourceAsProHtml scheme

proHtmlResourceDay :: ProHtmlResource -> IO (Maybe Day)
proHtmlResourceDay phr = pure $ dateFromResourceId $ proHtmlResourceId phr

getResourceDayMap :: Scheme -> (Resource -> Bool) -> IO (Map Day (Set Resource))
getResourceDayMap scheme resourcePredicate = foldMappings $ findProHtmlResources scheme >-> pipeMapMaybeM f
  where
    f :: ProHtmlResource -> IO (Maybe (Day, Resource))
    f r
      | resourcePredicate (proHtmlResourceId r) =
        (fmap . fmap) (\d -> (d, proHtmlResourceId r)) (proHtmlResourceDay r)
      | otherwise = pure Nothing

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
