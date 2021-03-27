module ProsidyHtml where

import Control.Lens
import qualified HtmlBuilding as H
import qualified HtmlTypes as H
import Prosidy
import Relude hiding (head)
import Resource

data ProHtmlOpts m = ProHtmlOpts
  { extraBlockTags :: BlockTagOpt m,
    extraInlineTags :: InlineTagOpt m,
    extraStyle :: [Resource]
  }

newtype BlockTagOpt m
  = BlockTagOpt
      (ProHtmlOpts m -> BlockTag -> Maybe (m (H.Series H.Block)))

newtype InlineTagOpt m
  = InlineTagOpt
      (ProHtmlOpts m -> InlineTag -> Maybe (m (H.Series H.Inline)))

defaultOpts :: ProHtmlOpts m
defaultOpts = ProHtmlOpts (BlockTagOpt $ \_ _ -> Nothing) (InlineTagOpt $ \_ _ -> Nothing) []

addStyle :: Resource -> Endo (ProHtmlOpts m)
addStyle r = Endo $ \o -> o {extraStyle = extraStyle o <> one r}

addBlockTag :: BlockTagOpt m -> Endo (ProHtmlOpts m)
addBlockTag (BlockTagOpt f) = Endo $ \opts -> opts {extraBlockTags = BlockTagOpt $ \o x -> let BlockTagOpt g = extraBlockTags opts in g o x <|> f o x}

addInlineTag :: InlineTagOpt m -> Endo (ProHtmlOpts m)
addInlineTag (InlineTagOpt f) = Endo $ \opts -> opts {extraInlineTags = InlineTagOpt $ \o x -> let InlineTagOpt g = extraInlineTags opts in g o x <|> f o x}

proHtml :: Monad m => ProHtmlOpts m -> Either Prosidy.Failure Document -> m H.Document
proHtml opts doc =
  do
    let documentLang = Just "en"
    let title = foldMap (one . H.HeaderTitle) $ proTitle doc
    let css = foldMap H.stylesheet (one [res|style/jarclasses.css|] ++ extraStyle opts)
    let documentHead = H.utf8htmlMeta <> H.viewportMeta <> title <> css
    documentContent <-
      fmap (one . H.BlockMain . H.Main) $
        liftA2
          (<>)
          (pure $ foldMap (H.toBlocks . H.H H.H1 . H.toInlines) $ proTitle doc)
          (either (pure . H.toBlocks . H.Comment . ("\n" <>) . toText . prettyFailure) (foldMapSequence (proBlockHtml opts) . view content) doc)
    pure H.Document {H.documentLang, H.documentHead, H.documentContent}

proTitle :: Either Prosidy.Failure Document -> Maybe Text
proTitle = either (const Nothing) (view (atSetting "title"))

proBlockHtml :: Monad m => ProHtmlOpts m -> Block -> m (H.Series H.Block)
proBlockHtml opts =
  \case
    BlockLiteral x -> pure $ H.toBlocks $ H.Comment (show x)
    BlockParagraph x ->
      do
        let paragraphClasses = mempty
        paragraphContent <- inlineHtml opts (view content x)
        pure $ H.toBlocks $ H.Paragraph {H.paragraphClasses, H.paragraphContent}
    BlockTag x -> proBlockTagHtml opts x

proBlockTagHtml :: Monad m => ProHtmlOpts m -> BlockTag -> m (H.Series H.Block)
proBlockTagHtml opts x =
  case (tagName x) of
    "day" -> h2 opts x
    "h2" -> h2 opts x
    "head" -> h2 opts x
    "subhead" -> h3 opts x
    "list" -> proListHtml opts x
    "quote" -> blockQuote opts x
    "blockquote" -> blockQuote opts x
    _ -> fromMaybe (pure $ H.toBlocks $ H.Comment (show x)) (let BlockTagOpt f = extraBlockTags opts in f opts x)

h2 :: Monad m => ProHtmlOpts m -> BlockTag -> m (H.Series H.Block)
h2 opts = fmap (H.toBlocks . H.H H.H2) . requireInlineOnly opts . view content

h3 :: Monad m => ProHtmlOpts m -> BlockTag -> m (H.Series H.Block)
h3 opts = fmap (H.toBlocks . H.H H.H3) . requireInlineOnly opts . view content

blockQuote :: Monad m => ProHtmlOpts m -> BlockTag -> m (H.Series H.Block)
blockQuote opts = fmap (H.toBlocks . H.Quote) . foldMapSequence (proBlockHtml opts) . view content

requireInlineOnly :: Monad m => ProHtmlOpts m -> Series Block -> m (H.Series H.Inline)
requireInlineOnly opts =
  foldMapSequence
    \case
      BlockParagraph y -> inlineHtml opts (view content y)
      y -> pure $ H.toInlines $ H.Comment (show y)

proInlineHtml :: Monad m => ProHtmlOpts m -> Inline -> m (H.Series H.Inline)
proInlineHtml opts =
  \case
    Break -> pure $ H.toInlines (" " :: Text)
    InlineText x -> pure $ H.toInlines (fragmentText x)
    InlineTag x -> proInlineTagHtml opts x

proInlineTagHtml :: Monad m => ProHtmlOpts m -> InlineTag -> m (H.Series H.Inline)
proInlineTagHtml opts x =
  case (tagName x) of
    "dash" -> pure $ H.toInlines ("—" :: Text)
    "emphatic" ->
      do
        let spanClasses = one "emphatic"
        let spanStyles = mempty
        spanContent <- inlineHtml opts (view content x)
        pure $ H.toInlines $ H.Span {H.spanClasses, H.spanStyles, H.spanContent}
      where
    "italic" ->
      do
        let spanClasses = mempty
        let spanStyles = one "font-style: italic"
        spanContent <- inlineHtml opts (view content x)
        pure $ H.toInlines $ H.Span {H.spanClasses, H.spanStyles, H.spanContent}
      where
    "title" ->
      do
        let spanClasses = one "title"
        let spanStyles = mempty
        spanContent <- inlineHtml opts (view content x)
        pure $ H.toInlines $ H.Span {H.spanClasses, H.spanStyles, H.spanContent}
      where
    "link" ->
      do
        let anchorName = Nothing
        let anchorHref = view (atSetting "to") x
        anchorContent <- inlineHtml opts (view content x)
        pure $ H.toInlines $ H.Anchor {H.anchorName, H.anchorHref, H.anchorContent}
      where
    _ -> fromMaybe (pure $ H.toInlines $ H.Comment (show x)) (let InlineTagOpt f = extraInlineTags opts in f opts x)

proListHtml :: Monad m => ProHtmlOpts m -> BlockTag -> m (H.Series H.Block)
proListHtml opts x =
  case listAxis x of
    ListVertical ->
      do
        listItems <- foldMapSequence (itemVertical opts) (view content x)
        pure $ H.toBlocks $ H.BulletedList listItems
    ListHorizontal ->
      do
        let paragraphClasses = one "horizontalList"
        paragraphContent <- foldMapSequence (itemHorizontal opts) (view content x)
        pure $ H.toBlocks $ H.Paragraph {H.paragraphClasses, H.paragraphContent}

data ListAxis = ListVertical | ListHorizontal

listAxis :: BlockTag -> ListAxis
listAxis x = if view (hasProperty "horizontal") x then ListHorizontal else ListVertical

itemVertical :: Monad m => ProHtmlOpts m -> Block -> m (H.Series H.ListItem)
itemVertical opts x =
  case x of
    BlockTag i | tagName i == "item" -> go $ view content i
    BlockParagraph i -> go $ pure $ BlockParagraph i
    y -> pure $ one $ H.ListComment $ H.Comment $ show y
  where
    go i = one . H.ListItem <$> foldMapSequence (proBlockHtml opts') i
      where
        opts' = opts & appEndo (addInlineTag inlineItemTag)

itemHorizontal :: Monad m => ProHtmlOpts m -> Block -> m (H.Series H.Inline)
itemHorizontal opts x =
  case x of
    BlockTag i | tagName i == "item" -> go $ view content i
    BlockParagraph i -> go $ pure $ BlockParagraph i
    y -> pure $ H.toInlines $ H.Comment $ show y
  where
    go i =
      do
        let spanClasses = mempty
        let spanStyles = mempty
        spanContent <- requireInlineOnly opts i
        pure $ H.toInlines $ H.Span {H.spanClasses, H.spanStyles, H.spanContent}

inlineItemTag :: Monad m => InlineTagOpt m
inlineItemTag = InlineTagOpt $ \opts ->
  \case
    i
      | tagName i == "item" -> Just $
        do
          spanContent <- inlineHtml opts (view content i)
          let spanClasses = one "item"
          let spanStyles = mempty
          pure $ H.toInlines $ H.Span {H.spanClasses, H.spanStyles, H.spanContent}
    _ -> Nothing

inlineHtml :: (Traversable series, Monad m) => ProHtmlOpts m -> series Inline -> m (H.Series H.Inline)
inlineHtml opts = foldMapSequence (proInlineHtml opts)

foldMapSequence :: (Traversable series, Monad m, Monoid b) => (a -> m b) -> series a -> m b
foldMapSequence f = fmap fold . sequence . fmap f
