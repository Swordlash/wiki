-- | 'Starts' are where a page's text may be cut so that a parse of the
-- part after the cut reads it as the parse of the whole page does: the
-- lines at which a top-level block starts with nothing open above it.
-- The editor's Enter ("Wiki.Core.Markdown.Enter") parses only the part
-- of the page around the cursor, and must read it as the page reads it.
--
-- CodeMirror's syntax tree cannot say where those lines are: it knows no
-- footnote's definition, none of the wiki's own blocks (@[[!notes]]@ …
-- @[[/notes]]@), and reads HTML in a page otherwise than the wiki's
-- parser, so a block it finds may start inside one of them. So the starts
-- come from the wiki's own parse, the one the editor makes of the whole
-- text after a pause in the typing ("Wiki.Core.Markdown.Problems"'
-- 'Wiki.Core.Markdown.Problems.checkText'), and are kept with the text
-- they were found in. As the text changes, the editor maps them through
-- the changes and keeps where the first change since that parse is: the
-- text before it is the text parsed, so a start whose line ends at or
-- before it is still one ('textRange'). Its own line counts: an indent
-- typed on it can put it in the item above.
--
-- A label of a footnote defined twice is the one exception: the parser
-- drops a second definition of a label with its blocks, whatever lies
-- between, so the part after a cut reads such a definition as the first.
module Wiki.Core.Markdown.Starts
  ( blockStarts
  , Kept (..)
  , textRange
  , blockDefinitions
  , Earlier (..)
  , noEarlier
  , linesFromStart
  , aboveLimit
  , linesBelow
  )
where

import Data.IntSet qualified as IntSet
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as T
import Protolude
import Wiki.Core.Markdown.Ast (Shape (..), ShapeKind (..), shapeLines)
import Wiki.Core.Markdown.Enter (textLimit)

-- | The lines, from 1, at which a top-level block of a text starts with
-- nothing open above it, in order, the first line always among them: the
-- first line of each block of the page's own (not one in a footnote's
-- definition, which the parser places at the page's end) that comes after
-- a blank line, and the first line of each item after the first of a list
-- of the page's own (not nested, not in a quotation, a footnote or a
-- block of the wiki's own). Cut at such an item, the rest reads as a list
-- of the same kind beginning with it, which is all Enter needs: the mark
-- and its number are on the item's line, and whether the list is loose
-- does not change what Enter writes. So in a list of any length Enter
-- reads only the items around the cursor's. A block of HTML that reads on, a fence left open, one of
-- the wiki's own blocks not closed is one block to its end, with no start
-- inside it; so is a footnote's definition. A block right after another
-- is read as it is because of the one before: an item may start a list
-- after a paragraph's line only if it has text, and is no task then.
blockStarts :: Text -> [Shape] -> [Int]
blockStarts source shapes =
  ordNub (1 : sort ([a | s <- shapes, s.kind /= NoteShape, let { a = fst (shapeLines s) }, a > 1, afterBlank a, not (inNote a)] <> items))
  where
    -- The items after the first of a list at the page's own level.
    items = [fst (shapeLines i) | s <- shapes, s.kind == ListShape, i <- drop 1 s.inner, not (inNote (fst (shapeLines i)))]
    -- The footnotes' definitions, each by its first line, with its last:
    -- they do not overlap, so the one that may hold a line is the last
    -- that starts at or before it.
    notes = Map.fromList [shapeLines s | s <- shapes, s.kind == NoteShape]
    inNote l = maybe False (\(_, b) -> l <= b) (Map.lookupLE l notes)
    -- The blank lines, found in one pass over the text, so that each
    -- block's start is a lookup in an 'IntSet', as each footnote's is in
    -- a 'Map'. The check finds the starts after every pause in the
    -- typing: a walk down the lines for each block was quadratic (1.1 s
    -- natively for 16,000 short paragraphs, 28 to 42 ms with the lookups).
    blanks = IntSet.fromList [l | (l, line) <- zip [1 ..] (T.splitOn "\n" source), T.null (T.strip line)]
    afterBlank l = IntSet.member (l - 1) blanks

-- | 'Kept' is a start the last parse found, as the editor holds it after
-- the changes since: mapped through them, where it is now, and where the
-- line it is on now starts and ends (offsets in the text now). A change
-- can map a start into the middle of a line, or several starts onto one
-- place (a deletion across them).
data Kept = Kept
  { at :: Int
  , lineFrom :: Int
  , lineTo :: Int
  }
  deriving stock (Eq, Ord, Show)

-- | Where the text handed to Enter starts and ends, for a cursor, as
-- offsets in the text now: given the starts kept ('Kept'), the offset of
-- the first change since they were found, the start and the end of the
-- cursor's line, and the text's length. Nothing when the text would be
-- longer than 'textLimit': Enter is then a line break alone.
--
-- Only a start still at a line's start counts, and once however many
-- starts the changes put there (several collapsed onto one would count a
-- line twice and cut the block below short).
--
-- The text starts at the last start before the start of the cursor's
-- line whose line ends at or before the first change, or at 0. Before the
-- cursor's line, not at it: on a line that starts a block, the block
-- above comes too, since Enter may empty that line, and a block above
-- that goes on over blank lines (a fence left open in an item) would take
-- the emptied line in. Typing in the cursor's block below its first line
-- leaves its start; an edit above it, or on its first line, until the
-- next parse, gives an earlier start and a longer text.
--
-- It ends just before the second start after the end of the cursor's
-- line, so that the block below comes whole, or at the end of the text. A
-- block cut short reads otherwise (a table's header without its delimiter
-- row is a paragraph), and Enter checks that a new line leaves the lines
-- below it read as they were; the blocks after one read as it was are
-- read as they were. A start the changes have put in a block that reads
-- on (an end marker typed above it) Enter sees only on the lines it is
-- given.
textRange :: [Kept] -> Int -> Int -> Int -> Int -> Maybe (Int, Int)
textRange kept firstChange lineStart lineEnd len = do
  let starts = ordNub (sort [k | k <- kept, k.at == k.lineFrom])
      positions = ordNub (map (.at) starts)
      begins = fromMaybe 0 (lastMay [k.at | k <- starts, k.at < lineStart, k.lineTo <= firstChange])
      ends = case dropWhile (<= lineEnd) positions of
        _ : end : _ -> end - 1
        _ -> len
  guard (ends - begins <= textLimit)
  pure (begins, ends)

----------------------------------------------------------------------
-- The completion's text around the cursor

-- | The definitions of a text, by line: for each definition of a link's
-- reference (@[label]: \/url@) and of a footnote (@[^label]: …@), the line
-- it starts on and a definition of the same label in one line
-- (@[label]: x@, @[^label]: x@), or 'Nothing' where its label could not be
-- read on that line (a label over two lines), or where its line is not
-- known (a footnote's definition with nothing in it, put at line 1). The shapes are the parse's
-- ('Wiki.Core.Markdown.Ast.parseShapes'): a link's definition is a shape
-- with no block of its own ('OtherShape', nothing inside), whose text
-- starts with its label in brackets and a colon; a footnote's definition
-- is a 'NoteShape', whose first line holds its label.
--
-- A definition counts wherever it is in the page: a reference elsewhere
-- reads as a link, or a footnote's number, only because of it. So the
-- completion, which reads the text from a start rather than from the top
-- ('linesFromStart'), is handed those above the start, in one line each.
blockDefinitions :: Text -> [Shape] -> [(Int, Maybe Text)]
blockDefinitions source shapes = sortOn fst (concatMap definitionsIn shapes)
  where
    sourceLines = T.splitOn "\n" source
    lineText l = fromMaybe "" (atMay sourceLines (l - 1))
    definitionsIn s = case s.kind of
      OtherShape
        | null s.inner
        , ((l, c), _) : _ <- s.parts
        , Just rest <- T.stripPrefix "[" (T.drop (c - 1) (lineText l))
        , not ("[" `T.isPrefixOf` rest) ->
            [(l, (\label -> "[" <> label <> "]: x") <$> labelIn rest)]
      NoteShape ->
        let l = fst (shapeLines s)
            (_, at) = T.breakOn "[^" (lineText l)
         in -- A footnote's definition with nothing in it has no line in the
            -- shapes: its label is not read, and it may be anywhere.
            if l > 0
              then (l, (\label -> "[^" <> label <> "]: x") <$> labelIn (T.drop 2 at)) : concatMap definitionsIn s.inner
              else [(1, Nothing)]
      _ -> concatMap definitionsIn s.inner
    -- The label before "]:", if the line holds it.
    labelIn rest = case T.breakOn "]:" rest of
      (label, end) | not (T.null end), not (T.null (T.strip label)) -> Just label
      _ -> Nothing

-- | 'Earlier' is what the editor keeps of its last check of the text
-- ("Wiki.Core.Markdown.Problems"' 'Wiki.Core.Markdown.Problems.Checked'),
-- for the completion: the lines of the text checked, the lines at which a
-- top-level block starts with nothing open above it ('blockStarts'), and
-- the text's definitions ('blockDefinitions').
data Earlier = Earlier
  { lines :: [Text]
  , starts :: [Int]
  , definitions :: [(Int, Maybe Text)]
  }

-- | Nothing checked yet: no start but the text's first line.
noEarlier :: Earlier
noEarlier = Earlier {lines = [], starts = [], definitions = []}

-- | The lines the completion's parser reads above the cursor's line: the
-- lines above it from the last start ('blockStarts') above the cursor's
-- line that the text has kept as the check read it, that line and all
-- above it; after the definitions above that start, one line each, and a
-- blank line ('blockDefinitions'). The text above the start is not read:
-- every block of it ended before the start, so the parser reads the lines
-- from the start as the whole text reads them, but for the definitions,
-- which count wherever they are. 'Nothing' where those lines are longer
-- than 'aboveLimit' (no start serves, or the block above the cursor is
-- one long block): the caller reads the whole text above, or opens no
-- list.
--
-- A start is not used below a definition whose label could not be read,
-- and not where definitions go before it and its line starts with white
-- space: a footnote's definition would take an indented line in.
linesFromStart :: Earlier -> [Text] -> Maybe [Text]
linesFromStart earlier above = do
  guard (sum (map ((+ 1) . T.length) handed) <= aboveLimit)
  pure handed
  where
    -- The lines above the cursor's that the text has kept as the check
    -- read them, from the first: a start among them is one still, its own
    -- line included.
    same = length (takeWhile identity (zipWith (==) earlier.lines above))
    unread = minimumMay [l | (l, Nothing) <- earlier.definitions]
    usable s =
      s > 1
        && s <= same
        && maybe True (s <=) unread
        && (null (definitionsAbove s) || not (indented (lineAt s)))
    indented = T.any isSpace . T.take 1
    lineAt s = fromMaybe "" (atMay above (s - 1))
    definitionsAbove s = firstOfEach Set.empty [d | (l, Just d) <- earlier.definitions, l < s]
    firstOfEach _ [] = []
    firstOfEach seen (d : ds)
      | key d `Set.member` seen = firstOfEach seen ds
      | otherwise = d : firstOfEach (Set.insert (key d) seen) ds
    key d = T.toCaseFold (T.unwords (T.words (fst (T.breakOn "]:" d))))
    handed = case lastMay (filter usable earlier.starts) of
      Nothing -> above
      Just s -> case definitionsAbove s of
        [] -> drop (s - 1) above
        ds -> ds <> [""] <> drop (s - 1) above

-- | 'aboveLimit' is the most text, in characters, the completion's parser
-- reads above the cursor's line ('linesFromStart').
aboveLimit :: Int
aboveLimit = 4000

-- | How many of the lines below the cursor's the completion's parser
-- reads, given how far after the cursor it reads at most (the
-- completion's @afterBound@, in characters), the lines above the
-- cursor's, the cursor's line before the cursor and after it, and the
-- lines below,
-- where it need not read them that far: up to the second start of a
-- block after a blank line below the cursor's line, that start's line
-- not read. The block after the cursor's comes whole, so the parser
-- reads whether the cursor's block ends before it, as it reads it from
-- the longer text: the cursor's block, and every block after it up to
-- that start, read the same whatever follows a blank line and a block
-- with nothing open above it.
--
-- Only where the cursor's line is the one line the text has changed
-- since the check (the lines above it and below it are the lines the
-- check read, as typing on the line leaves them; the line may be new, a
-- blank line beside it too), since a block typed elsewhere could take in
-- what follows. A start after a blank line, not an item's after another
-- (which a cut would make a tight list of a loose one), and not a
-- definition's (which makes no block). And not where a definition comes
-- after that start within the bound: it counts above it too. 'Nothing'
-- otherwise: the parser reads the lines below to the bound.
linesBelow :: Int -> Earlier -> [Text] -> Text -> Text -> [Text] -> Maybe Int
linesBelow bound earlier above before after below = do
  -- A carriage return on the cursor's line is a line break to the parser
  -- and not to the editor, whose lines the check's are compared with.
  guard (not (T.any (== '\r') (before <> after)))
  let k = length above
      total = length earlier.lines
      -- The text checked was this text with the cursor's line as it was
      -- (-1), or without it (0), or without it and a blank line beside it
      -- (1: a paragraph begun on a blank line takes a blank line after
      -- it): the lines above and below it overlap on that many lines of
      -- the text checked.
      overlap = k + length below - total
      -- How far a line below has moved since the check.
      moved = overlap + 1
  guard (overlap >= -1 && overlap <= 1)
  guard (and (zipWith (==) earlier.lines above) && drop (total - length below) earlier.lines == below)
  guard (all (T.null . T.strip) (take overlap (drop (k - overlap) earlier.lines)))
  let definitionLines = Set.fromList (map fst earlier.definitions)
      -- Whether the line above a line of the text now is a blank line
      -- below the cursor's.
      blankAbove l = l - 1 > k + 1 && maybe False (T.null . T.strip) (atMay below (l - k - 3))
      -- Starts below the cursor's line, as lines of the text now.
      after' = [l | s <- earlier.starts, s > total - length below, let l = s + moved, blankAbove l, not (s `Set.member` definitionLines)]
  end <- atMay after' 1
  let reach = length (takeWhile (<= bound) (scanl (\n l -> n + 1 + T.length l) (T.length after) below))
      lastRead = k + 1 + reach
  guard (not (any (\l -> l + moved >= end && l + moved <= lastRead) (Set.toList definitionLines)))
  pure (end - k - 2)
