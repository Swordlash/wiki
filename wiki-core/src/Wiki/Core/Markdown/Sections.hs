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
--
-- = Writing a section
--
-- 'replaceSection' puts a new text in the place of one section, of its own
-- part, or of the top, and refuses a text that would change any section
-- outside that place: see its own documentation.
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

    -- * Writing a section
  , Target (..)
  , Replaced (..)
  , Refusal (..)
  , replaceSection
  )
where

import Data.List (lookup, span)
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

-- | 'Target' is the part of a page that a write replaces: a section whole,
-- by its anchor, its subsections included; a section's own part, by its
-- anchor, its subsections left where they are; or the page's top, the text
-- before its first section.
data Target
  = WholeSection Text
  | OwnPart Text
  | Top
  deriving stock (Eq, Show)

-- | 'Replaced' is a page after a write of one of its parts: its new text,
-- and the anchor of the section written ('Nothing' for the top). The
-- anchor can differ from the one asked for, since the body may change its
-- heading's text.
data Replaced = Replaced
  { text :: Text
  , anchor :: Maybe Text
  }
  deriving stock (Eq, Show)

-- | 'Refusal' is why 'replaceSection' refused a body. A refused body
-- changes nothing.
data Refusal
  = -- | No section of the page has the anchor.
    NoSuchAnchor
  | -- | The body for a section is empty: a section cannot be deleted by a
    -- write of its text.
    EmptyBody
  | -- | The body does not start with a heading of the section's level,
    -- this one.
    NotAHeading Int
  | -- | Written whole, the body has a heading of the section's level or a
    -- higher one after its own, with this text and level: a section is
    -- written alone.
    AnotherSection Text Int
  | -- | Written as a section's own part, the body has a subsection's
    -- heading, with this text.
    Subsection Text
  | -- | Written as the top, the body has a heading, with this text.
    SectionInTop Text
  | -- | The body's heading has an id written out that an earlier section
    -- has, this one: the anchor would name that section.
    AnchorTaken Text
  | -- | A section outside the part written would change its anchor, from
    -- the first to the second, as a heading with the same text as another
    -- one does.
    AnchorChanges Text Text
  | -- | The anchor of a section outside the part written, this one, would
    -- name a section of the body instead, whose heading has the same id
    -- written out.
    AnchorMoves Text
  | -- | A section outside the part written, by its anchor, would change,
    -- move or no longer be one; after the part ('True') or before it.
    SectionChanges Text Bool
  | -- | A heading outside the part written, by its text, would become a
    -- section; after the part ('True') or before it.
    SectionAppears Text Bool
  | -- | A block spoiler outside the part written would change its bounds,
    -- or no longer have an outline; after the part ('True') or around it.
    SpoilerChanges Bool
  | -- | Something else outside the part written would change.
    OutsideChanges
  deriving stock (Eq, Show)

-- | 'replaceSection' writes a body in the place of a part of a page: the
-- new text of the page, or why the body is refused.
--
-- The body is ended with a line break when it has none. When text follows
-- the part in the page and the body does not end with an empty line, and
-- the body is refused as it is, one empty line is added and the body tried
-- again: a paragraph line directly before a title underlined with @===@
-- would become that title's first line, and the empty line keeps the two
-- apart. The empty line is added only when the body is then taken.
-- Nothing else of the body changes. So a part written back as it was read
-- gives the same page, but at the end of a page without a final line
-- break, which gains one. An empty body is refused, but for the top, where
-- it removes the top.
--
-- The new page's outline is then compared with the old one. Outside the
-- part, every section stays as it was: its anchor, its level, its text,
-- the section it is under, the spoilers it is in, and its bounds once
-- shifted by the change of length; and a section that its anchor names
-- stays named by it. So do the block spoilers there. Inside the part:
--
-- * A section written whole is one section: the body starts with a
--   heading of the section's level, in the same spoiler, and every other
--   heading of the body is under it.
-- * A section's own part is its heading and its own text: the body starts
--   with a heading of the section's level, and has no other heading but
--   in a block spoiler.
-- * The top has no heading, but in a block spoiler.
--
-- That one comparison refuses a code fence or a spoiler left open (the
-- sections after it change), a spoiler's end marker that closes the
-- spoiler the section is in, a heading of another level, a second
-- heading of the same level, and a new heading text that changes another
-- heading's anchor (@notes@ and @notes-2@). The body may change its own
-- heading's text; the new anchor is in the answer. An id written out
-- (@{#x}@) that another section has is refused: an earlier section's,
-- since the anchor would not name the section written, and a later one's,
-- since that section would lose its address.
--
-- Both texts are read by 'outline', the whole page every time, never the
-- body alone.
replaceSection :: Target -> Text -> Text -> Either Refusal Replaced
replaceSection target page body = do
  (a, b, wanted) <- case target of
    Top -> Right (0, old.top, Nothing)
    WholeSection anchor -> (\p -> (p.section.start, p.section.end, Just p)) <$> named anchor
    OwnPart anchor -> (\p -> (p.section.start, p.section.ownEnd, Just p)) <$> named anchor
  ended <- ending
  case writeIn a b wanted ended of
    Left why
      | b < T.length page
      , not (T.null ended)
      , not (endsEmpty ended) ->
          writeIn a b wanted (ended <> breakAfter ended)
      | otherwise -> Left why
    accepted -> accepted
  where
    old = outline page
    named anchor = maybeToRight NoSuchAnchor (find (\p -> p.section.id == anchor) (placedIn old))
    -- The body ended with a line break, or refused when it is empty.
    ending
      | T.null body = if target == Top then Right "" else Left EmptyBody
      | lineBreak (T.last body) = Right body
      | otherwise = Right (body <> "\n")
    -- Whether a text that ends with a line break ends with an empty line:
    -- its last line, before that break, has nothing but spaces and tabs.
    endsEmpty t =
      let before = fromMaybe (T.dropEnd 1 t) (T.stripSuffix "\r\n" t)
       in T.all (\c -> c == ' ' || c == '\t') (T.takeWhileEnd (not . lineBreak) before)
    -- The line break that makes an empty line after a text that ends with
    -- one: a line feed, but after a carriage return alone another carriage
    -- return, since a line feed there would make one line break of the two.
    breakAfter t = if T.last t == '\r' then "\r" else "\n"
    lineBreak c = c == '\n' || c == '\r'
    -- The page with this text in the place of the part from @a@ to @b@,
    -- the section @wanted@ (or the top), or why it is refused.
    writeIn a b wanted written = do
      let new = T.take a page <> written <> T.drop b page
          a' = a + T.length written
          o' = outline new
          newPlaced = placedIn o'
          -- Offsets of each page as if the part had no width: before the part
          -- as they are, after it moved back by its length, inside it none.
          atOld p = if p <= a then At p else if p >= b then At (p - (b - a)) else Within
          atNew p = if p <= a then At p else if p >= a' then At (p - (a' - a)) else Within
          (oldSeen, oldBoxes, oldPage) = view atOld (\p -> p >= a && p < b) old
          (newSeen, newBoxes, newPage) = view atNew (\p -> p >= a && p < a') o'
          headingAt p = find (\x -> x.section.start == p) newPlaced
          own = case target of
            OwnPart _ -> True
            _ -> False
          -- The section the body starts with ('Nothing' for the top), or why
          -- there is none; and whether the body is that section and nothing
          -- more, or for the top, no section.
          (opening, inside) = case wanted of
            Nothing
              | o'.top == a' -> (Right Nothing, True)
              | otherwise -> (maybe (Right Nothing) (\x -> Left (SectionInTop x.section.text)) (headingAt o'.top), False)
            Just w -> case headingAt a of
              Just t
                | t.section.level == w.section.level ->
                    let stop = if own then t.section.ownEnd else t.section.end
                        beyond x = if own then Subsection x.section.text else AnotherSection x.section.text x.section.level
                     in if stop == a' && map atNew t.spoilers == map atOld w.spoilers
                          then (Right (Just t), True)
                          else (maybe (Right (Just t)) (Left . beyond) (guard (stop < a') *> headingAt stop), False)
              _ -> (Left (NotAHeading w.section.level), False)
          -- Why the outside differs: the first section of the old page that
          -- changed, then the first new one, then the spoilers.
          outsideRefusal =
            head
              ( mapMaybe (\(raw, s) -> differs raw s (lookup s.shape.start [(n.shape.start, n) | (_, n) <- newSeen])) oldSeen
                  <> [SectionAppears n.shape.text (raw >= a') | (raw, n) <- newSeen, n.shape.start `notElem` map (\(_, s) -> s.shape.start) oldSeen]
                  <> [SpoilerChanges (raw >= b) | (raw, x) <- oldBoxes, x `notElem` map snd newBoxes]
                  <> [SpoilerChanges (raw >= a') | (raw, x) <- newBoxes, x `notElem` map snd oldBoxes]
              )
          differs raw s = \case
            Nothing -> Just (SectionChanges s.anchor (raw >= b))
            Just n
              | sameAs n s -> Nothing
              | n.shape == s.shape -> Just (AnchorChanges s.anchor n.anchor)
              | otherwise -> Just (SectionChanges s.anchor (raw >= b))
          sameAs n s = n.anchor == s.anchor && n.shape == s.shape
          -- The sections outside whose anchor named them and would name an
          -- earlier section, one in the part, with the same id written out. A
          -- section whose anchor named an earlier one may come to be named by
          -- it: that mends a link, and breaks none.
          unnamed = [s.anchor | ((_, s), (_, n)) <- zip oldSeen newSeen, s.named, not n.named]
      section <- opening
      unless (inside && and (zipWith (\(_, s) (_, n) -> sameAs n s) oldSeen newSeen) && length oldSeen == length newSeen && map snd oldBoxes == map snd newBoxes && oldPage == newPage) $
        Left (fromMaybe OutsideChanges outsideRefusal)
      for_ (head unnamed) (Left . AnchorMoves)
      case section of
        Just t
          | Just leading <- find (\x -> x.section.id == t.section.id) newPlaced
          , leading.section.start /= a ->
              Left (AnchorTaken t.section.id)
        _ -> Right Replaced {text = new, anchor = (\t -> t.section.id) <$> section}

-- | 'Placed' is a section of an outline with where it is: the start of the
-- section it is under ('Nothing' at the top level of its outline, the
-- page's or a spoiler's), and the starts of the block spoilers it is in,
-- the innermost first.
data Placed = Placed
  { section :: Section
  , parent :: Maybe Int
  , spoilers :: [Int]
  }

-- | Every section of an outline, in the order of 'outlineSections', with
-- where it is.
placedIn :: Outline -> [Placed]
placedIn = inOutline []
  where
    inOutline around o = concatMap (inSpoiler around) o.spoilers <> concatMap (inSection Nothing around) o.sections
    inSpoiler around sb = inOutline (sb.start : around) sb.outline
    inSection parent around s = Placed s parent around : concatMap (inSpoiler around) s.spoilers <> concatMap (inSection (Just s.start) around) s.below

-- | 'Pos' is an offset of a page with a part written, as 'replaceSection'
-- compares them: the offset as if the part had no width, or 'Within' the
-- part.
data Pos = At Int | Within
  deriving stock (Eq, Show)

-- | 'Seen' is a section as 'replaceSection' compares it: its anchor, its
-- 'Shape', and whether its anchor names it, being the first section with
-- that id, or an earlier one with the same id written out ('sectionRange').
data Seen = Seen
  { anchor :: Text
  , shape :: Shape
  , named :: Bool
  }
  deriving stock (Eq, Show)

-- | 'Shape' is a section's place and heading, but its anchor: its level,
-- its text, its bounds, the section it is under and the spoilers it is in.
data Shape = Shape
  { level :: Int
  , text :: Text
  , start :: Pos
  , ownEnd :: Pos
  , end :: Pos
  , parent :: Maybe Pos
  , spoilers :: [Pos]
  }
  deriving stock (Eq, Show)

-- | 'Box' is a block spoiler with an outline, as 'replaceSection' compares
-- it: where its start marker's line, its body, its body's top and its end
-- marker's line start, and the spoilers it is in.
data Box = Box
  { start :: Pos
  , from :: Pos
  , top :: Pos
  , to :: Pos
  , spoilers :: [Pos]
  }
  deriving stock (Eq, Show)

-- | An outline as 'replaceSection' compares it, its offsets through @at@:
-- the sections and the block spoilers that do not start in the part
-- written (@inPart@), each with its start in the page, and where the
-- page's top and the page end.
view :: (Int -> Pos) -> (Int -> Bool) -> Outline -> ([(Int, Seen)], [(Int, Box)], (Pos, Pos))
view at inPart o =
  ( [ (p.section.start, Seen {anchor = p.section.id, shape = shapeOf p, named = fmap (\x -> x.section.start) (find (\x -> x.section.id == p.section.id) ps) == Just p.section.start})
    | p <- ps
    , not (inPart p.section.start)
    ]
  , [ (sb.start, Box {start = at sb.start, from = at sb.outline.from, top = at sb.outline.top, to = at sb.outline.to, spoilers = map at around})
    | (sb, around) <- boxes [] o
    , not (inPart sb.start)
    ]
  , (at o.top, at o.to)
  )
  where
    ps = placedIn o
    shapeOf p = Shape {level = p.section.level, text = p.section.text, start = at p.section.start, ownEnd = at p.section.ownEnd, end = at p.section.end, parent = at <$> p.parent, spoilers = map at p.spoilers}
    boxes around x = concatMap (\sb -> (sb, around) : boxes (sb.start : around) sb.outline) (x.spoilers <> concatMap (\s -> s.spoilers) (flat x.sections))
    flat = concatMap (\s -> s : flat s.below)
