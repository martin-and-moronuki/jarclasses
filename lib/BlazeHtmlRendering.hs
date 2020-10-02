module BlazeHtmlRendering where

import Relude hiding (head)
import Text.Blaze.Html (Html)
import qualified Text.Blaze.Internal as Blaze
import qualified Text.Blaze.Renderer.String as Blaze

renderHtml :: Html -> String
renderHtml html = renderHtmlIndented html ""

-- forked from Text.Blaze.Renderer.Pretty
renderHtmlIndented :: Blaze.MarkupM b -> String -> String
renderHtmlIndented = go 0 id
  where
    contentInline :: Blaze.StaticString -> Bool
    contentInline open = Blaze.getString open "" `elem` (["<p", "<title"] ++ map (\n -> "<h" <> show n) (enumFromTo 1 6 :: [Int]))

    go :: Int -> (String -> String) -> Blaze.MarkupM b -> String -> String

    go i attrs (Blaze.Parent _ open close content)
      | contentInline open =
        ind i . Blaze.getString open . attrs . (">" ++) . renderHtmlCompact content . Blaze.getString close . ('\n' :)
    go i attrs (Blaze.Parent _ open close content) =
      ind i . Blaze.getString open . attrs . (">\n" ++) . go (inc i) id content
        . ind i
        . Blaze.getString close
        . ('\n' :)
    go i attrs (Blaze.CustomParent tag content) =
      ind i . ('<' :) . Blaze.fromChoiceString tag . attrs . (">\n" ++)
        . go (inc i) id content
        . ind i
        . ("</" ++)
        . Blaze.fromChoiceString tag
        . (">\n" ++)
    go i attrs (Blaze.Leaf _ begin end _) =
      ind i . Blaze.getString begin . attrs . Blaze.getString end . ('\n' :)
    go i attrs (Blaze.CustomLeaf tag close _) =
      ind i . ('<' :) . Blaze.fromChoiceString tag . attrs
        . ((if close then " />\n" else ">\n") ++)
    go i attrs (Blaze.AddAttribute _ key value h) =
      flip (go i) h $
        Blaze.getString key . Blaze.fromChoiceString value . ('"' :) . attrs
    go i attrs (Blaze.AddCustomAttribute key value h) =
      flip (go i) h $
        (' ' :) . Blaze.fromChoiceString key . ("=\"" ++) . Blaze.fromChoiceString value
          . ('"' :)
          . attrs
    go i _ (Blaze.Content content _) = ind i . Blaze.fromChoiceString content . ('\n' :)
    go i _ (Blaze.Comment comment _) =
      ind i
        . ("<!-- " ++)
        . Blaze.fromChoiceString comment
        . (" -->\n" ++)
    go i attrs (Blaze.Append h1 h2) = go i attrs h1 . go i attrs h2
    go _ _ (Blaze.Empty _) = id

    -- Increase the indentation
    inc = (+) 2

    -- Produce appending indentation
    ind i = (replicate i ' ' ++)

-- forked from Text.Blaze.Renderer.String
renderHtmlCompact :: Blaze.MarkupM b -> String -> String
renderHtmlCompact = go id
  where
    go :: (String -> String) -> Blaze.MarkupM b -> String -> String
    go attrs (Blaze.Parent _ open close content) =
      Blaze.getString open . attrs . ('>' :) . go id content . Blaze.getString close
    go attrs (Blaze.CustomParent tag content) =
      ('<' :) . Blaze.fromChoiceString tag . attrs . ('>' :) . go id content
        . ("</" ++)
        . Blaze.fromChoiceString tag
        . ('>' :)
    go attrs (Blaze.Leaf _ begin end _) = Blaze.getString begin . attrs . Blaze.getString end
    go attrs (Blaze.CustomLeaf tag close _) =
      ('<' :) . Blaze.fromChoiceString tag . attrs
        . (if close then (" />" ++) else ('>' :))
    go attrs (Blaze.AddAttribute _ key value h) =
      flip go h $
        Blaze.getString key . Blaze.fromChoiceString value . ('"' :) . attrs
    go attrs (Blaze.AddCustomAttribute key value h) =
      flip go h $
        (' ' :) . Blaze.fromChoiceString key . ("=\"" ++) . Blaze.fromChoiceString value
          . ('"' :)
          . attrs
    go _ (Blaze.Content content _) = Blaze.fromChoiceString content
    go _ (Blaze.Comment comment _) =
      ("<!-- " ++) . Blaze.fromChoiceString comment . (" -->" ++)
    go attrs (Blaze.Append h1 h2) = go attrs h1 . go attrs h2
    go _ (Blaze.Empty _) = id
