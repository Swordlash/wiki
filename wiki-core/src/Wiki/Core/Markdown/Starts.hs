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
  , earlierFrom
  , noEarlier
  , linesFromStart
  , aboveLimit
  , linesBelow
  )
where

import Data.IntMap.Strict qualified as IntMap
import Data.IntSet qualified as IntSet
import Data.List (zip3)
import Data.Map.Strict qualified as Map
import Data.Sequence qualified as Seq
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
-- the changes since: mapped through them, where it is now, where the line
-- it is on now starts and ends (offsets in the text now), and whether that
-- line is flush: it holds text from its first column, not a blank or a
-- tab. A change can map a start into the middle of a line, onto a blank
-- line, or several starts onto one place (a deletion across them).
data Kept = Kept
  { at :: Int
  , lineFrom :: Int
  , lineTo :: Int
  , flush :: Bool
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
-- line whose line ends at or before the first change and starts before
-- it, or at 0. Before the
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
-- read as they were. A start after the first change counts there only
-- if its line is flush ('Kept'). A change above a start can make an
-- indented line part of a block above: a deletion of the block between
-- an item and an indented line, since the check, makes the line the
-- item's next paragraph, and the deleted block's start lands on the blank
-- line above it, which is no start either; an item typed above an
-- indented line takes it in the same way. No block above takes in a
-- flush line by its indentation. One may take it in as a lazy line, but
-- then the first start below the cursor's line is a flush line taken in
-- too, and it comes whole: Enter reads that the cursor's block goes on
-- below it. A start the changes have put in a block that reads on (an end
-- marker typed above it) Enter sees only on the lines it is given.
textRange :: [Kept] -> Int -> Int -> Int -> Int -> Maybe (Int, Int)
textRange kept firstChange lineStart lineEnd len = do
  let starts = ordNub (sort [k | k <- kept, k.at == k.lineFrom])
      -- A start the check read, on a line the changes have left: one
      -- that ends at or before the first change, and starts before it. A
      -- start the changes mapped onto an empty line where the first
      -- change begins (one in a block deleted there) is none.
      checked k = k.lineFrom < firstChange && k.lineTo <= firstChange
      begins = fromMaybe 0 (lastMay [k.at | k <- starts, k.at < lineStart, checked k])
      below = ordNub [k.at | k <- starts, checked k || k.flush]
      ends = case dropWhile (<= lineEnd) below of
        _ : end : _ -> end - 1
        _ -> len
  guard (ends - begins <= textLimit)
  pure (begins, ends)

----------------------------------------------------------------------
-- The completion's text around the cursor

-- | The definitions of a text, by line: for each line that may hold a
-- definition of a link's reference (@[label]: \/url@) or of a footnote
-- (@[^label]: …@), a definition of the same label in one line
-- (@[label]: x@, @[^label]: x@), or 'Nothing' where it is not read here.
--
-- A line may hold one where it holds @]:@ and its run of lines (since the
-- last blank line, or the text's start) has a line that starts with @[@
-- after its container marks. CommonMark's link reference definition is a
-- label in brackets and a colon right after the closing bracket, and
-- commonmark-extensions' footnote definition is @[^label]@ on one line
-- and a colon right after it. The label opens with @[@ as the first
-- character of a paragraph or block, after the container marks of its
-- line (spaces, tabs, @>@, a list's @-@, @+@, @*@, a number and its @.@ or
-- @)@; a task's box and a footnote's or a box's line start with @[@
-- themselves); it may run over lines but not over a blank line; @]:@ is
-- on its last line. So a @]:@ in a run of lines none of which starts with
-- @[@ after its marks closes no label. The marks are stripped by
-- characters, wide, so that the check errs towards suspecting a line.
-- Every suspect line is a candidate: the parser
-- may read a definition there that no shape reports (a reference
-- definition in the body of a footnote defined twice, whose blocks it
-- drops and whose definitions it keeps; a footnote's definition in the
-- body of another on the same line). So a line holding @]:@ is read
-- only where nothing can differ: it holds @]:@ once, one definition
-- starts on it in the shapes ('Wiki.Core.Markdown.Ast.parseShapes': a link's definition is a
-- shape with no block of its own, a footnote's a 'NoteShape'), at the top
-- level (in no quotation, item, footnote or other block), its label in
-- brackets from the line's first character after at most three spaces, a
-- label of plain text (no backslash, no bracket). Any other line holding
-- @]:@ is not read ('Nothing'), and no start below it is used: the
-- completion reads more of the text there, as the whole text reads it. A
-- line that starts with @[@ and holds @]:@ in a code listing (a
-- TypeScript index signature, @[key: string]: number@) or in prose is
-- such a line too, and costs the text below it that shortcut: known, and
-- left.
--
-- A definition counts wherever it is in the page: a reference elsewhere
-- reads as a link, or a footnote's number, only because of it. So the
-- completion, which reads the text from a start rather than from the top
-- ('linesFromStart'), is handed those above the start, in one line each.
blockDefinitions :: Text -> [Shape] -> [(Int, Maybe Text)]
blockDefinitions source shapes = [(l, readOn l (Map.findWithDefault [] l byLine)) | (l, line, opened) <- zip3 [1 ..] lines' (drop 1 (scanl runOpened False lines')), opened, "]:" `T.isInfixOf` line]
  where
    lines' = T.splitOn "\n" source
    sourceLines = Seq.fromList lines'
    -- Whether a line's run of lines (since the last blank line) has a line
    -- that starts with a bracket after its container marks, stripped by
    -- characters, wide: every way a container (a quotation, an item, a
    -- numbered item, a task's box, a footnote's or a box's line) puts a
    -- paragraph's first character later on its line is among them.
    runOpened opened line
      | T.null (T.strip line) = False
      | otherwise = opened || "[" `T.isPrefixOf` T.dropWhile (\c -> isDigit c || c `elem` (" \t>-+*.)" :: [Char])) line
    lineText l = fromMaybe "" (Seq.lookup (l - 1) sourceLines)
    byLine = Map.fromListWith (flip (<>)) [(l, [(top, footnote)]) | (l, top, footnote) <- concatMap (definitionsIn True) shapes]
    -- Each definition's line, whether it is at the top level, and whether
    -- it is a footnote's.
    definitionsIn top s = case s.kind of
      OtherShape
        | null s.inner
        , ((l, c), _) : _ <- s.parts
        , Just rest <- T.stripPrefix "[" (T.drop (c - 1) (lineText l))
        , not ("[" `T.isPrefixOf` rest) ->
            [(l, top, False)]
      NoteShape ->
        let l = fst (shapeLines s)
         in [(l, top, True) | l > 0] <> concatMap (definitionsIn False) s.inner
      _ -> concatMap (definitionsIn False) s.inner
    readOn l = \case
      [(True, footnote)] -> do
        let line = lineText l
            indent = T.takeWhile (== ' ') line
            opening = if footnote then "[^" else "["
        guard (T.length indent <= 3 && T.count "]:" line == 1)
        rest <- T.stripPrefix opening (T.drop (T.length indent) line)
        label <- labelIn rest
        pure (opening <> label <> "]: x")
      _ -> Nothing
    -- The label before "]:", if the line holds it and nothing in it is
    -- escaped or bracketed.
    labelIn rest = case T.breakOn "]:" rest of
      (label, end)
        | not (T.null end)
        , not (T.null (T.strip label))
        , not (T.any (`elem` ['\\', '[', ']', '^']) label) ->
            Just label
      _ -> Nothing

-- | 'Earlier' is what the editor keeps of its last check of the text
-- ("Wiki.Core.Markdown.Problems"' 'Wiki.Core.Markdown.Problems.Checked'),
-- for the completion, made once when the check's result is kept
-- ('earlierFrom'): the lines of the text checked, indexed; the lines at
-- which a top-level block starts with nothing open above it
-- ('blockStarts'); the text's definitions by line ('blockDefinitions'),
-- and the first of them whose label is not read.
data Earlier = Earlier
  { lines :: Seq Text
  , starts :: IntSet
  , definitions :: IntMap (Maybe Text)
  , unread :: Maybe Int
  }

-- | What the editor keeps of a check: of the text checked, its starts and
-- its definitions.
earlierFrom :: Text -> [Int] -> [(Int, Maybe Text)] -> Earlier
earlierFrom text starts' definitions' =
  Earlier
    { lines = Seq.fromList (T.splitOn "\n" text)
    , starts = IntSet.fromList starts'
    , definitions = IntMap.fromListWith (\_ first' -> first') definitions'
    , unread = head [l | (l, Nothing) <- sortOn fst definitions']
    }

-- | Nothing checked yet: no start but the text's first line.
noEarlier :: Earlier
noEarlier = Earlier {lines = Seq.empty, starts = IntSet.empty, definitions = IntMap.empty, unread = Nothing}

-- | How many lines above the cursor's the text has kept as the check read
-- them, from the first: a start among them is one still, its own line
-- included. A walk of the lines above, once a request.
keptAbove :: Earlier -> [Text] -> Int
keptAbove earlier above = length (takeWhile identity (zipWith (==) (toList earlier.lines) above))

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
-- A start is not used below a definition whose label is not read, and not
-- where definitions go before it and its line starts with white space: a
-- footnote's definition would take an indented line in.
--
-- The starts are searched from the cursor's line up, and the first that
-- serves is taken; each is looked at in constant time (the lines and the
-- definitions are indexed, 'Earlier'). On 16,000 short paragraphs with a
-- definition at the top, it takes 1.2 ms natively, most of it the one
-- comparison of the lines above with the check's ('keptAbove'), where a
-- walk of the lines for each start took 0.47 s (Codex, #298).
linesFromStart :: Earlier -> [Text] -> Maybe [Text]
linesFromStart earlier above = do
  guard (sum (map ((+ 1) . T.length) handed) <= aboveLimit)
  pure handed
  where
    highest = min (keptAbove earlier above) (maybe maxBound identity earlier.unread)
    candidates = takeWhile (> 1) (IntSet.toDescList (fst (IntSet.split (highest + 1) earlier.starts)))
    firstDefinition = fst <$> IntMap.lookupMin earlier.definitions
    usable s = maybe True (>= s) firstDefinition || not (indented (lineAt s))
    indented = T.any isSpace . T.take 1
    lineAt s = fromMaybe "" (Seq.lookup (s - 1) earlier.lines)
    definitionsAbove s = firstOfEach Set.empty (catMaybes (IntMap.elems (fst (IntMap.split s earlier.definitions))))
    firstOfEach _ [] = []
    firstOfEach seen (d : ds)
      | key d `Set.member` seen = firstOfEach seen ds
      | otherwise = d : firstOfEach (Set.insert (key d) seen) ds
    key d = T.toCaseFold (T.unwords (T.words (fst (T.breakOn "]:" d))))
    handed = case find usable candidates of
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
-- lines below, where it need not read them that far: up to the second
-- start of a block after a blank line below the cursor's line, that
-- start's line not read. The block after the cursor's comes whole, so the
-- parser reads whether the cursor's block ends before it, as it reads it
-- from the longer text: the cursor's block, and every block after it up
-- to that start, read the same whatever follows a blank line and a block
-- with nothing open above it.
--
-- Only where the cursor's line is the one line the text has changed
-- since the check, as far as the parser reads (the lines above it, and
-- below it to the bound, are the lines the check read, as typing on the
-- line leaves them; the line may be new, a blank line beside it too),
-- since a block typed elsewhere could take in what follows. A start after
-- a blank line, not an item's after another (which a cut would make a
-- tight list of a loose one), and not a definition's (which makes no
-- block). And not where a definition comes after that start within the
-- bound: it counts above it too. 'Nothing' otherwise: the parser reads
-- the lines below to the bound.
--
-- Nothing below the bound is looked at: the lines compared, the starts
-- and the definitions are those within it, the starts and definitions
-- found by their index. On a tight list of 16,000 items with the cursor
-- in the first, it takes 0.08 ms natively, where a walk of the lines
-- below for each item took 0.43 s (Codex, #298); the lines above are
-- compared with the check's once ('keptAbove').
linesBelow :: Int -> Earlier -> [Text] -> Text -> Text -> [Text] -> Maybe Int
linesBelow bound earlier above before after below = do
  -- A carriage return on the cursor's line is a line break to the parser
  -- and not to the editor, whose lines the check's are compared with.
  guard (not (T.any (== '\r') (before <> after)))
  let k = length above
      -- The lines below within the bound, as the parser reads them.
      reach = length (takeWhile (<= bound) (scanl (\n l -> n + 1 + T.length l) (T.length after) below))
      within = Seq.fromList (take reach below)
      checkedAt l = Seq.lookup (l - 1) earlier.lines
      -- How far a line below has moved since the check: the text checked
      -- was this text with the cursor's line as it was (0), or without it
      -- (1), or without it and a blank line beside it (2: a paragraph begun
      -- on a blank line takes a blank line after it), a line of the text
      -- now (from 1) being the line that many lines above it then.
      matches moved =
        (moved < 2 || maybe False (T.null . T.strip) (checkedAt k))
          && and [checkedAt (k + 2 + j - moved) == Just l | (j, l) <- zip [0 ..] (toList within)]
  guard (keptAbove earlier above >= k)
  moved <- find matches [0, 1, 2]
  let -- Whether the line above a line of the text now is a blank line
      -- below the cursor's.
      blankAbove l = l - 1 > k + 1 && maybe False (T.null . T.strip) (Seq.lookup (l - k - 3) within)
      lastRead = k + 1 + reach
      -- Starts below the cursor's line within the bound, as lines of the
      -- text now.
      startsWithin = IntSet.toAscList (fst (IntSet.split (lastRead - moved + 1) (snd (IntSet.split (k + 1 - moved) earlier.starts))))
      after' = [l | s <- startsWithin, let l = s + moved, blankAbove l, not (s `IntMap.member` earlier.definitions)]
  end <- atMay after' 1
  guard (maybe True ((> lastRead - moved) . fst) (IntMap.lookupGE (end - moved) earlier.definitions))
  pure (end - k - 2)
