-- | A page's sections, as places in its source text: what the API for
-- programs reads a page's table of contents and one section of a page by
-- (WP5.138 of docs/PLAN.md, "Wiki.Server.ContentsApi").
--
-- The headings are those of the page's table of contents
-- ("Wiki.Core.Markdown.Toc"'s 'tableOfContents'), with the same ids, so
-- that an anchor here is the one a link to the heading has in the rendered
-- page. They are found by the wiki's own parser: a @#@ in a code block is
-- no heading, a setext heading (its title, then @===@ or @---@) is one, and
-- a heading inside a quotation, a list, a box or a footnote is part of
-- that block, not a section of the page, as the page's "Contents" list has
-- it. A heading inside a spoiler is listed, as there.
--
-- = Bounds
--
-- A section starts at the start of its heading's first line: the line of
-- a heading written with @#@, the title's first line of a setext heading.
-- Its own part ('ownEnd') ends where the next heading of the table starts,
-- of any level; the whole section ('end') where the next heading of the
-- same or a higher level starts, so with its subsections. The text before
-- the first heading is the page's top ('topAnchor'). So the top and every
-- section's own part, in order, are the whole text, cut at the headings'
-- lines: every character is in exactly one of them, the blank lines at a
-- section's end included.
--
-- The headings' lines come from the parser ('parseWithLinesMarkersAndDeck'),
-- which counts lines as 'parserLineStarts' does (a carriage return ends a
-- line too). A heading inside a block spoiler has no line in the parse of
-- the page, which keeps only its top-level blocks' lines: the inside of the
-- spoiler, between its marker lines, is parsed again on its own, as the
-- editor's folds do ("Wiki.Core.Markdown.Folds"). Its lines are then known
-- within that inside, and its ids are not the page's: the headings found so
-- are matched with the table's in order, and their levels must agree.
module Wiki.Core.Markdown.Sections
  ( Outline (..)
  , Section (..)
  , outline
  , topAnchor
  , sectionRange
  , sectionText
  , outlineAnchors
  )
where

import Data.List (span, zip3)
import Data.Sequence qualified as Seq
import Data.Text qualified as T
import Protolude
import Wiki.Core.Markdown.Ast (Block (..), MarkerLine (..), MarkerRole (..), SpoilerHead (..), SpoilerKind (..), parseMarkdown, parseWithLinesMarkersAndDeck)
import Wiki.Core.Markdown.Toc (Entry (..), tableOfContents)
import Wiki.Core.Slides (parserLineStarts)

-- | 'Outline' is a page's headings as places in its text: the table of
-- contents, each entry with its section's bounds.
data Outline = Outline
  { top :: Int
  -- ^ Where the page's top ends: the start of the first heading's line,
  -- or the end of the text in a page without headings. The top starts at
  -- the text's start.
  , sections :: [Section]
  }
  deriving stock (Eq, Show)

-- | 'Section' is a heading of a page's table of contents and the part of
-- the page's text it heads, with the sections of the deeper headings
-- under it. The bounds are offsets in characters from the text's start.
data Section = Section
  { id :: Text
  -- ^ The heading's id: the anchor of a link to it.
  , level :: Int
  , text :: Text
  -- ^ The heading's text, without its markup.
  , start :: Int
  -- ^ Where the heading's first line starts.
  , ownEnd :: Int
  -- ^ Where the next heading of any level starts, or the text ends.
  , end :: Int
  -- ^ Where the next heading of the same or a higher level starts, or
  -- the text ends.
  , below :: [Section]
  }
  deriving stock (Eq, Show)

-- | The anchor of the page's top, the text before its first heading:
-- @~top@. No heading has it: an id the wiki makes from a heading's text
-- has letters, digits, @-@ and @_@ only, and an id written as @{#name}@
-- cannot hold a @~@.
topAnchor :: Text
topAnchor = "~top"

-- | The outline of a page's Markdown. 'Nothing' when the text does not
-- parse, which CommonMark makes rare, or when the headings found in it are
-- not those of its table of contents, which the tests say does not happen.
outline :: Text -> Maybe Outline
outline source = do
  doc <- rightToMaybe (parseMarkdown source)
  let entries = concatMap flatten (tableOfContents doc)
      found = headingStarts source
  guard (length entries == length found && and (zipWith (\(_, level, _) (level', _) -> level == level') entries found))
  let starts = map snd found
      placed = zip3 entries starts (drop 1 starts <> [total])
  pure Outline {top = fromMaybe total (head starts), sections = nest total placed}
  where
    total = T.length source
    flatten :: Entry -> [(Text, Int, Text)]
    flatten e = (e.id, e.level, e.text) : concatMap flatten e.below
    -- The sections of some headings, each with the deeper ones after it;
    -- the last one ends where the section they are in ends.
    nest limit = \case
      [] -> []
      ((id, level, text), start, ownEnd) : rest ->
        let (inside, after) = span (\((_, l, _), _, _) -> l > level) rest
            end = maybe limit (\(_, s, _) -> s) (head after)
         in Section {id, level, text, start, ownEnd, end, below = nest end inside} : nest limit after

-- | The level and the start of each heading of a text that its table of
-- contents lists, in the order of the text: the headings of the text's
-- top-level blocks and of its heading spoilers' sections, and those inside
-- its block spoilers, at any depth.
headingStarts :: Text -> [(Int, Int)]
headingStarts = region 0
  where
    -- The headings of a text read as a page of its own, which starts at
    -- this offset in the page.
    region :: Int -> Text -> [(Int, Int)]
    region offset text = case parseWithLinesMarkersAndDeck text of
      Left _ -> []
      Right (_, markers, deck, _) ->
        let lineStarts = Seq.fromList (parserLineStarts text)
            -- Where a line starts, by its number from 1; the text's end for
            -- a line after the last.
            lineStart n = fromMaybe (T.length text) (Seq.lookup (n - 1) lineStarts)
            -- The end marker's line of a block of the wiki's own, by the
            -- line it starts on.
            closing l = head [e | MarkerLine e _ (Ends s) <- markers, s == l]
         in concat
              [ case b of
                  Heading level _ _ -> [(level, offset + lineStart l)]
                  -- A heading spoiler: its heading alone, its section's
                  -- blocks after it in the deck's reading.
                  Spoiler h (Heading level _ _ : _) | h.kind == OnHeading -> [(level, offset + lineStart l)]
                  -- A block spoiler: the lines between its markers, or
                  -- after its start marker to the end, when it has none.
                  Spoiler _ _ ->
                    let first' = lineStart (l + 1)
                        past = maybe (T.length text) lineStart (closing l)
                     in region (offset + first') (T.take (past - first') (T.drop first' text))
                  _ -> []
              | (Just l, b) <- deck
              ]

-- | The bounds of a page's part by its anchor: the top ('topAnchor'), or
-- a section, whole or its own part only. An id written twice in braces
-- names the first heading that has it.
sectionRange :: Outline -> Text -> Bool -> Maybe (Int, Int)
sectionRange o anchor own
  | anchor == topAnchor = Just (0, o.top)
  | otherwise = (\s -> (s.start, if own then s.ownEnd else s.end)) <$> find (\s -> s.id == anchor) (everySection o.sections)

-- | A part of a text between two bounds: a slice of it, character for
-- character.
sectionText :: Text -> (Int, Int) -> Text
sectionText source (a, b) = T.take (b - a) (T.drop a source)

-- | Every anchor of an outline, in the order of the text: the top's, then
-- the headings'.
outlineAnchors :: Outline -> [Text]
outlineAnchors o = topAnchor : map (\s -> s.id) (everySection o.sections)

-- | The sections and their subsections, in the order of the text.
everySection :: [Section] -> [Section]
everySection = concatMap (\s -> s : everySection s.below)
