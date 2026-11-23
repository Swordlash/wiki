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
-- it.
--
-- = Bounds
--
-- A section starts at the start of its heading's first line: the line of
-- a heading written with @#@, the title's first line of a setext heading.
-- Its own part ('ownEnd') ends where the next section of its outline
-- starts, whatever its level; the whole section ('end') where the next
-- section of the same or a higher level starts, so with its subsections.
-- The text before the first section is the outline's top. So the top and
-- every section's own part, in order, are the outline's whole text, cut at
-- the headings' lines, the blank lines at a section's end included.
--
-- = Spoilers
--
-- A section never crosses a spoiler's edge (the owner, 2026-11-23: either
-- the whole section is in the spoiler, or the spoiler has no subsection in
-- it).
--
-- * A block spoiler, @[[!spoiler]]@ … @[[\/spoiler]]@, is one block of the
--   section it is in: the headings inside it neither end nor split the
--   sections around it, and the own part of that section holds the whole
--   spoiler, its marker lines too. Inside, its headings make an outline of
--   their own ('SpoilerBody'), whose text is the spoiler's body, the lines
--   between its marker lines: a section there ends at the next heading of
--   the same or a higher level in the spoiler, or at the end marker's line.
--   A spoiler in a spoiler is the same, one level down.
-- * A heading spoiler, @## [[!spoiler]] Question@, hides its heading's
--   section and nothing else, so it is a section like any other.
-- * An inline spoiler, @||hidden||@, lies within a line and holds no
--   heading.
--
-- A spoiler whose body cannot be had (one never closed) has no outline:
-- its headings are no sections, and its text stays in the section around
-- it, as a heading in a quotation does.
--
-- The headings' lines all come from one parse of the page
-- ('parseDeckSpansAndInsides'), which counts lines as 'parserLineStarts'
-- does (a carriage return ends a line too): the lines of the page's own
-- blocks, and those of the blocks inside each block spoiler, as the parser
-- read them in the page ('SpoilerInside'). A piece of the page is never
-- parsed again on its own, since out of the page it can read differently
-- (a marker line before a @---@, a fence left open). Where the blocks of a
-- block spoiler in the tree are not, one for one, those the parser read
-- inside it, the spoiler has no outline either; a heading whose line would
-- not come after the line of the section before it is no section. The
-- tests say neither happens on the pages they make.
module Wiki.Core.Markdown.Sections
  ( Outline (..)
  , Section (..)
  , SpoilerBody (..)
  , outline
  , topRange
  , sectionRange
  , sectionText
  , outlineSections
  , outlineAnchors
  )
where

import Data.List (span)
import Data.Map.Strict qualified as Map
import Data.Sequence qualified as Seq
import Data.Text qualified as T
import Protolude
import Wiki.Core.Markdown.Ast (Block (..), SpoilerHead (..), SpoilerInside (..), SpoilerKind (..), attribute, parseDeckSpansAndInsides, plainText)
import Wiki.Core.Slides (parserLineStarts)

-- | 'Outline' is the sections of a text, the page's or a block spoiler's
-- body: where the text starts and ends, where its top (the text before its
-- first section) ends, the block spoilers in the top, and the sections.
-- Offsets are in characters from the page's start.
data Outline = Outline
  { from :: Int
  , top :: Int
  -- ^ Where the top ends: the start of the first section, or 'to'.
  , to :: Int
  , spoilers :: [SpoilerBody]
  -- ^ The block spoilers in the top.
  , sections :: [Section]
  }
  deriving stock (Eq, Show)

-- | 'Section' is a heading of a page's table of contents and the part of
-- the text it heads, with the block spoilers in its own part and the
-- sections of the deeper headings under it.
data Section = Section
  { id :: Text
  -- ^ The heading's id: the anchor of a link to it.
  , level :: Int
  , text :: Text
  -- ^ The heading's text, without its markup.
  , start :: Int
  -- ^ Where the heading's first line starts.
  , ownEnd :: Int
  -- ^ Where the next section of the outline starts, or the outline ends.
  , end :: Int
  -- ^ Where the next section of the same or a higher level starts, or the
  -- outline ends.
  , spoilers :: [SpoilerBody]
  -- ^ The block spoilers in the section's own part.
  , below :: [Section]
  }
  deriving stock (Eq, Show)

-- | 'SpoilerBody' is a block spoiler with the outline of its body: where
-- its start marker's line starts, and the outline of the lines between its
-- marker lines.
data SpoilerBody = SpoilerBody
  { start :: Int
  , outline :: Outline
  }
  deriving stock (Eq, Show)

-- | The outline of a page's Markdown. A text the parser does not read,
-- which CommonMark makes rare, has no sections: all of it is the top.
outline :: Text -> Outline
outline source = case parseDeckSpansAndInsides source of
  Left _ -> Outline {from = 0, top = total, to = total, spoilers = [], sections = []}
  Right (deck, insides) ->
    let table = Map.fromList [(l, (e, bs)) | SpoilerInside l e bs <- insides]
     in region table 0 total (concatMap (\(at, b) -> zip (at : repeat Nothing) (unsectioned [b])) deck)
  where
    total = T.length source
    lineStarts = Seq.fromList (parserLineStarts source)
    lineStart n = fromMaybe total (Seq.lookup (n - 1) lineStarts)
    -- The outline of the text between two offsets, whose blocks are these,
    -- each with its lines when they are known, heading spoilers their
    -- heading alone ('unsectioned').
    region table lo hi located =
      let items = concatMap (item table) located
          heads = increasing [(h, s) | Left (h, s) <- items, s >= lo, s < hi]
          boxes = [b | Right b <- items, b.start >= lo, b.outline.to <= hi]
          starts = map snd heads
          placed = zipWith (\(h, s) e -> (h, s, e)) heads (drop 1 starts <> [hi])
          top = fromMaybe hi (head starts)
       in Outline {from = lo, top, to = hi, spoilers = within boxes lo top, sections = nest boxes hi placed}
    -- A block as the outline sees it: a heading of the table of contents,
    -- with where its line starts, or a block spoiler with its own outline.
    item table (at, b) = case b of
      Heading level attrs xs
        | Just i <- attribute "id" attrs
        , Just (l, _) <- at ->
            [Left ((i, level, plainText xs), lineStart l)]
      Spoiler h bs | h.kind == OnHeading -> concatMap (item table . (at,)) bs
      Spoiler h bs
        | Just (Just closing, read') <- Map.lookup h.line table
        , flat <- unsectioned bs
        , length read' == length flat
        , and (zipWith (\(_, r) f -> alike r f) read' flat) ->
            [Right SpoilerBody {start = lineStart h.line, outline = region table (lineStart (h.line + 1)) (lineStart closing) (zip (map fst read') flat)}]
      _ -> []
    -- The headings each starting after the one before.
    increasing = \case
      a@(_, s) : b@(_, s') : rest
        | s' <= s -> increasing (a : rest)
        | otherwise -> a : increasing (b : rest)
      xs -> xs
    within boxes a b = [x | x <- boxes, x.start >= a, x.start < b]
    -- The sections of some headings, each with the deeper ones after it;
    -- the last one ends where the section they are in ends.
    nest boxes limit = \case
      [] -> []
      ((id, level, text), start, ownEnd) : rest ->
        let (inside, after) = span (\((_, l, _), _, _) -> l > level) rest
            end = maybe limit (\(_, s, _) -> s) (head after)
         in Section {id, level, text, start, ownEnd, end, spoilers = within boxes start ownEnd, below = nest boxes end inside} : nest boxes limit after

-- | Whether a block the parser read and a block of the tree are the same
-- one, as far as the sections go: a heading of the same level, a spoiler
-- of the same kind on the same line, or neither.
alike :: Block -> Block -> Bool
alike r f = case (r, f) of
  (Heading l _ _, Heading l' _ _) -> l == l'
  (Spoiler h _, Spoiler h' _) -> h.kind == h'.kind && h.line == h'.line
  (Heading {}, _) -> False
  (_, Heading {}) -> False
  (Spoiler {}, _) -> False
  (_, Spoiler {}) -> False
  _ -> True

-- | Blocks with each heading spoiler its heading alone and its section's
-- blocks after it, as the parser reads them before a heading spoiler takes
-- in its section ("Wiki.Core.Markdown.Ast"'s @withHeadingSections@).
unsectioned :: [Block] -> [Block]
unsectioned = concatMap $ \case
  Spoiler h (heading@Heading {} : rest) | h.kind == OnHeading -> Spoiler h [heading] : unsectioned rest
  b -> [b]

-- | The bounds of an outline's top, the text before its first section.
topRange :: Outline -> (Int, Int)
topRange o = (o.from, o.top)

-- | The bounds of a section by its anchor, in the page or in a spoiler,
-- whole or its own part only. An id written twice in braces names the
-- first heading that has it.
sectionRange :: Outline -> Text -> Bool -> Maybe (Int, Int)
sectionRange o anchor own = (\s -> (s.start, if own then s.ownEnd else s.end)) <$> find (\s -> s.id == anchor) (outlineSections o)

-- | A part of a text between two bounds: a slice of it, character for
-- character.
sectionText :: Text -> (Int, Int) -> Text
sectionText source (a, b) = T.take (b - a) (T.drop a source)

-- | Every section of an outline, those in its spoilers too, in the order
-- of the text.
outlineSections :: Outline -> [Section]
outlineSections o = concatMap inSpoiler o.spoilers <> concatMap inSection o.sections
  where
    inSpoiler sb = outlineSections sb.outline
    inSection s = s : concatMap inSpoiler s.spoilers <> concatMap inSection s.below

-- | The anchors of every section of an outline, in the order of the text.
outlineAnchors :: Outline -> [Text]
outlineAnchors = map (\s -> s.id) . outlineSections
