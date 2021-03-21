module Resource where

import Control.Lens
import Data.Data (Data)
import qualified Data.List as List
import qualified Data.Text as T
import Language.Haskell.TH.Quote
import Language.Haskell.TH.Syntax
import Path
import Relude
import qualified Text.Blaze.Html5 as HTML
import qualified Text.Blaze.Html5.Attributes as Attr
import Text.Show

-- A "resource" is anything that a client might request from our web server, such as an HTML page, a CSS stylesheet, or an image.
newtype Resource = ResourceSlashList [Text]
  deriving stock (Eq, Ord, Generic, Data)
  deriving anyclass (Hashable)

instance Show Resource where
  showsPrec _ (ResourceSlashList xs) =
    showString "[res|"
      . showString (toString (T.intercalate "/" xs))
      . showString "|]"

resourceExp :: Resource -> Q Exp
resourceExp (ResourceSlashList xs) =
  [e|
    ResourceSlashList $
      map
        (toText :: String -> Text)
        $(liftData $ map toString xs)
    |]

res :: QuasiQuoter
res =
  QuasiQuoter
    { quoteExp = resourceExp . ResourceSlashList . filter (not . T.null) . T.splitOn "/" . toText,
      quotePat = const $ fail "Cannot be used in a pattern context.",
      quoteType = const $ fail "Cannot be used in a type context.",
      quoteDec = const $ fail "Cannot be used in a declaration context."
    }

resourceUrl :: Resource -> Text
resourceUrl = ("/" <>) . T.intercalate "/" . (\(ResourceSlashList x) -> x)

resourceHref :: Resource -> HTML.Attribute
resourceHref = Attr.href . HTML.toValue . resourceUrl

isPrefixOf :: Resource -> Resource -> Bool
ResourceSlashList a `isPrefixOf` ResourceSlashList b = a `List.isPrefixOf` b

resourceRelFile :: Resource -> Maybe (Path Rel File)
resourceRelFile (ResourceSlashList r) =
  unsnoc r >>= \(dirTexts, fileText) ->
    traverse (Path.parseRelDir . toString) dirTexts >>= \dirs ->
      (Path.parseRelFile . toString) fileText >>= \file ->
        Just $ foldr (Path.</>) file dirs

relFileResource :: Path Rel File -> Resource
relFileResource file = ResourceSlashList $ f (Path.parent file) `snoc` txtFile (Path.filename file)
  where
    f :: Path Rel Dir -> [Text]
    f p = if Path.parent p == p then [] else f (Path.parent p) `snoc` txtDir (Path.dirname p)
    txtFile = toText . Path.toFilePath
    txtDir = fromMaybe (error "dir should have a trailing slash") . T.stripSuffix "/" . toText . Path.toFilePath
