-- | What Enter writes in the editor's text (WP5.124 (a) of docs/PLAN.md,
-- issue #187, and the first cut of #301): the next item of a list, the
-- next line of a quotation or a box, an item split in two, or, on an
-- empty item or an empty quoted line, the way up one level. CodeMirror's
-- keymap calls 'edit' as the key is pressed, with the text of the
-- top-level block the cursor is in (web/src/editor.js). The function is
-- pure, so every rule is tested natively, with the wiki's own parser as
-- the oracle (decision 83).
--
-- Which quotations and items a line is in, and whether it is code, is the
-- wiki's parser's reading ("Wiki.Core.Markdown.Ast"'s 'parseShapes'), not
-- a pattern over the characters: CommonMark's rules for containers (the
-- spaces a mark may have before it and after it, lazy lines, indented
-- code, where an item ends) are the parser's. From the characters,
-- 'enter' reads only how a line the parser put in a container writes that
-- container's mark: the spaces and the @>@ of a quotation, an item's
-- bullet or number where the parser says the item starts.
--
-- The contract. Enter writes more than a line break only with the cursor
-- at the end of its line, or in the text of an item's first line (a
-- selection is the editor's business: it sends one to the plain line
-- break). It changes only the cursor's line and the new line it makes,
-- and the numbers of the items after a new numbered item.
--
-- * At the end of the last line with text of a list's item: a new item of
--   the same list at the same indentation, with the same bullet, or the
--   next number and the same delimiter (@.@ or @)@), and an empty box
--   after a task, done or not (@- [ ] @). The parser reads a task only in
--   a bulleted list: after @1. [x] done@ comes @2. @, as after any text.
--   The containers around the list go on before it: a quotation's marks
--   as the cursor's line writes them (@>  > @), an outer item's
--   indentation.
-- * In the text of an item's first line, with text after the cursor on
--   the line: the item split in two ('SplitWith'). The text before the
--   cursor stays in the item, as it is, blanks at its end included (as at
--   the end of a line, where Enter leaves them). A new item of the same
--   list starts on the next line, written as at the end of the line, and
--   takes the rest of the line, without the blanks right after the
--   cursor (they would widen the gap after its mark, and five make code),
--   and every later line of the item: its continuation lines, its nested
--   lists, its further paragraphs. The cursor goes to the start of the
--   new item's text. With the cursor at the start of the item's text,
--   among its marks, or before blanks alone, Enter is a plain line break,
--   as it was before. The parser reads the answer back ('splitIn'): the
--   two items' first paragraphs, put together with the blanks taken out,
--   must be the old one's, inline for inline, so a cut inside code, math,
--   a link, emphasis or anything else written over several characters is
--   a plain line break; so is a cut after a bracket left open (a link's
--   reference, whose definition the text given may not hold), and one
--   between @]@ and @[@.
-- * After a new item of a numbered list (at the end of a line, or split
--   off), the items after it in that list are renumbered ('Renumbered'),
--   each the number before it plus one, in the same change, so that one
--   undo takes both back. Only where they were numbered one after another
--   from the item the cursor is in, at most 'renumberLimit' of them, and
--   only where the parser reads the text renumbered as it reads it
--   without: a wider number moves its item's text one column right, and
--   the item's later lines are left as they are, so where one of them
--   would leave the item (a second paragraph indented to the old column),
--   no number is changed. The list's end must be in the text given, or
--   the text must reach the end of the page; otherwise the editor asks
--   again with a longer text ('Edit''s @longer@).
-- * At the end of a line of a quotation or a box (lazy lines of its
--   paragraph too): its marks on the new line, as the cursor's line writes
--   them, @> @ where it is lazy, after the indentation of the items around
--   it.
-- * On an empty item (its mark, its box if it is a task, and nothing
--   else) and on an empty line of a quotation (its marks and nothing
--   else): up one level. The line becomes what Enter would start after
--   the container around: the next item of the item around, the marks of
--   the quotation around, or, at the top level, an empty line. The items
--   after an item started this way are not renumbered.
--
-- Items inside quotations and quotations inside items, at any depth, and
-- loose lists are read as the parser reads them. Everything else is a
-- plain line break:
--
-- * a cursor before the end of its line, outside the text of an item's
--   first line: in a paragraph, a quotation, a box, a heading, an item's
--   later line;
-- * code, fenced or indented, alone or in a container; HTML; a fence the
--   wiki renders; a table, a rule, the wiki's marker lines, a footnote's
--   definition;
-- * a line of an item before its last line with text: a new mark there
--   would take what follows (a nested list, a second paragraph) into the
--   new item, or out of the list;
-- * a blank line in an item, which is no item's own;
-- * a tab among the marks and the indentation of the cursor's line, or of
--   the first line of an item around it (a tab's width depends on its
--   column);
-- * a numbered item whose next number would have ten digits, which
--   CommonMark does not read as a number;
-- * a text longer than 'textLimit';
-- * a text holding a carriage return: CommonMark counts one alone as a
--   line break, and the editor does not, so their lines differ;
-- * an answer the parser does not read back as meant: 'enter' parses the
--   text it would write, and keeps its answer only when the new line is in
--   the containers the answer says, and every line below with anything on
--   it is read as before, and every block that took attributes takes the
--   same ones ('readsAsMeant'): an empty item's text is
--   indented by its mark and one space, so a line below indented that far
--   would go into it; a quoted line in the middle of a paragraph can make
--   the lines below code; a line gone up can join two lists, or take an
--   item nested below into the new one; an empty item gone from under a
--   line of attributes alone hands the attributes to the block below.
--
-- So an empty item or quoted line in the middle of a list or a quotation
-- goes up only when the lines after it keep their reading; otherwise
-- Enter there is a plain line break.
module Wiki.Core.Markdown.Enter
  ( Enter (..)
  , enter
  , written
  , textLimit
  , Edit (..)
  , Change (..)
  , edit
  , renumberLimit
  , utf16Length
  )
where

import Data.List (span, zip3)
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Protolude
import Wiki.Core.Markdown.Ast (Block, ItemMark (..), Shape (..), ShapeKind (..), parseShapesAndSpans, parseWithSpans, shapeLines)
import Wiki.Core.Markdown.Ast qualified as A

-- | 'Enter' is what Enter writes at the cursor.
data Enter
  = -- | A line break and nothing more, as everywhere outside the contract.
    NewLine
  | -- | A line break with this text after it, the cursor after the text:
    -- the start of the next item or of the next quoted line.
    NewLineWith Text
  | -- | The cursor's line replaced by this text, the cursor at its end:
    -- an empty item or an empty quoted line gone up one level.
    LineBecomes Text
  | -- | The cursor's item split in two: so many blanks right after the
    -- cursor taken out, and a line break with this text put in their
    -- place, the cursor after the text. The text is the start of the next
    -- item; the rest of the line is its text.
    SplitWith Int Text
  deriving stock (Eq, Show)

-- | What Enter writes, given the text before the cursor and the text after
-- it: whole lines of the page around the cursor, or the whole page. The
-- editor gives the top-level block the cursor is in as CodeMirror's tree
-- reads it, with the block above it when no blank line is between them
-- (whether the block's first line is an item or a lazy line depends on a
-- paragraph above), from the nearest footnote's definition or start
-- marker of the wiki's own blocks above, which that tree does not know,
-- and to the first line of the block below (which 'readsAsMeant' checks
-- a new line does not change). The answer is the one the whole page would
-- give; the exception is a label defined twice: the parser drops the
-- second footnote's definition of a label, with its blocks. The two texts
-- are given apart so that no offset crosses from the browser's UTF-16
-- units to Haskell's characters.
enter :: Text -> Text -> Enter
enter before after = maybe NewLine (.answer) (answered before after)

-- | 'Answered' is an answer other than a plain line break, with what was
-- parsed on the way to it, which the renumbering after it reads again:
-- the shapes of the text before the answer, and the blocks of the text
-- after it.
data Answered = Answered
  { answer :: Enter
  , shapes :: [Shape]
  , blocks :: [(Maybe (Int, Int), Block)]
  }

-- | 'enter''s answer, with what was parsed for it; nothing for a plain
-- line break.
answered :: Text -> Text -> Maybe Answered
answered before after
  | T.length before + T.length after > textLimit = Nothing
  | T.any (== '\r') (before <> after) = Nothing
  | not (T.null after || "\n" `T.isPrefixOf` after) = split before after
  | otherwise = do
      let text = before <> after
          n = T.count "\n" before + 1
      (shapes, _) <- rightToMaybe (parseShapesAndSpans text)
      place <- placeOf shapes n
      (answer, expected) <- decide (T.splitOn "\n" text) n place
      blocks <- readsAsMeant before after shapes answer expected
      pure Answered {answer, shapes, blocks}

-- | 'textLimit' is the most text, in characters, 'enter' reads: past it,
-- Enter is a plain line break. The text is parsed twice, about 25 to 35
-- microseconds a character on the everyday WebAssembly build (measured
-- 2026-11-16: 3 ms for 149 characters, 10 to 17 ms for 691, 330 to 380
-- ms for 10,900, a whole list of a thousand items), so that Enter stays
-- under about 50 ms. A new item of a numbered list with items after it
-- costs more: the text renumbered is parsed a third time, and where the
-- list may go on past the text the editor asks again with a longer one,
-- of up to this many characters too ('Wiki.Core.Markdown.Starts.longerEnd'),
-- so at most about six parses of 2,000 characters (not measured on
-- WebAssembly). web/src/editor.js hands over no more than this. A
-- list's items are where it may cut the text (Wiki.Core.Markdown.Starts),
-- so what is still past the limit is one long block: a paragraph, a
-- quotation or a box, a single item.
textLimit :: Int
textLimit = 2000

-- | The text after Enter's answer, as the text before the cursor and the
-- text after it.
written :: Text -> Text -> Enter -> (Text, Text)
written before after = \case
  NewLine -> (before <> "\n", after)
  NewLineWith start -> (before <> "\n" <> start, after)
  LineBecomes line -> (T.dropWhileEnd (/= '\n') before <> line, after)
  SplitWith blanks start -> (before <> "\n" <> start, T.drop blanks after)

----------------------------------------------------------------------
-- What the editor is told

-- | 'Change' is one change Enter makes to the text: a range of the text
-- before the change, from @from@ to @to@, in UTF-16 units from the
-- cursor (negative before it), the text that range holds, and the text
-- put in its place. The editor's offsets are UTF-16 units and Haskell's
-- are characters: the editor checks that each range still holds @old@
-- before it changes anything, so that a wrong offset writes nothing.
data Change = Change
  { from :: Int
  , to :: Int
  , old :: Text
  , new :: Text
  }
  deriving stock (Eq, Show)

-- | 'Edit' is what Enter does to the text, other than a plain line
-- break: the change on the cursor's line (the new line, the item split,
-- the line replaced), the numbers of the list's later items rewritten,
-- and where the cursor goes, in UTF-16 units from where it was, in the
-- text after the changes. All the changes are one transaction of the
-- editor, so one undo takes them back.
--
-- @longer@ is set when the new item's list may go on past the text given
-- and its numbers were not looked at: the editor asks again with a
-- longer text, and takes the renumbering of that answer if its change on
-- the cursor's line is the same.
data Edit = Edit
  { change :: Change
  , renumbered :: [Change]
  , cursor :: Int
  , longer :: Bool
  }
  deriving stock (Eq, Show)

-- | What Enter does, given whether the text given ends where the page
-- ends, the text before the cursor and the text after it; nothing for a
-- plain line break ('enter' is its answer on the cursor's line).
edit :: Bool -> Text -> Text -> Maybe Edit
edit ends before after = do
  done <- answered before after
  let line = T.takeWhileEnd (/= '\n') before
  (change, cursor) <- case done.answer of
    NewLine -> Nothing
    NewLineWith start -> Just (Change 0 0 "" ("\n" <> start), utf16Length ("\n" <> start))
    SplitWith blanks start -> Just (Change 0 blanks (T.take blanks after) ("\n" <> start), utf16Length ("\n" <> start))
    LineBecomes new -> Just (Change (negate (utf16Length line)) 0 line new, utf16Length new - utf16Length line)
  pure $ case following ends before after done of
    PastText -> Edit {change, renumbered = [], cursor, longer = True}
    Following numbers ->
      Edit
        { change
        , renumbered = [Change at (at + utf16Length was) was now | Renumbered offset was now <- numbers, let at = utf16Length (T.take offset after)]
        , cursor
        , longer = False
        }

-- | The length of a text in UTF-16 units, the editor's: two for a
-- character past the first 65,536, one for any other.
utf16Length :: Text -> Int
utf16Length = T.foldl' (\n c -> n + if ord c > 0xFFFF then 2 else 1) 0

----------------------------------------------------------------------
-- Where the cursor's line is

-- | 'Container' is a quotation or a list's item that a line is in, as the
-- parser read it.
data Container
  = -- | A quotation or a box, and the line it starts on.
    Quotation Int
  | -- | A list's item.
    Item ItemAt
  deriving stock (Eq, Show)

-- | 'ItemAt' is a list's item as the parser read it: the line its mark is
-- on and the mark's column, its list's mark and the line its list starts
-- on, whether it is a task, and the last line with any of its text.
data ItemAt = ItemAt
  { firstLine :: Int
  , markColumn :: Int
  , mark :: ItemMark
  , listLine :: Int
  , task :: Bool
  , lastText :: Int
  }
  deriving stock (Eq, Show)

-- | 'Place' is where a line is: the containers it is in, the outermost
-- first, and whether it is in text (a paragraph, an item's text, a
-- heading) or on a container's own line (a box's marker, an empty item, a
-- blank line).
data Place = Place
  { containers :: [Container]
  , inText :: Bool
  }
  deriving stock (Show)

-- | Where a line is, as the shapes read it; none when the line is in code,
-- in a footnote's definition, or in anything that is neither text nor a
-- quotation or a list.
placeOf :: [Shape] -> Int -> Maybe Place
placeOf shapes n = go [] Nothing shapes
  where
    go held owner level = case lastMay (filter covers level) of
      Nothing -> Just Place {containers = reverse held, inText = False}
      Just s -> case s.kind of
        QuoteShape -> go (Quotation (fst (extent s)) : held) Nothing s.inner
        ListShape -> go held (Just s) s.inner
        ItemShape mark task -> do
          ((line, column), _) <- head s.parts
          ((listStart, _), _) <- head . (.parts) =<< owner
          let item = ItemAt {firstLine = line, markColumn = column, mark, listLine = listStart, task, lastText = fromMaybe line (maximumMay (map lastTextOf s.inner))}
          go (Item item : held) Nothing s.inner
        TextShape | null s.inner -> Just Place {containers = reverse held, inText = True}
        -- The wiki's own blocks between marker lines (a box of attributes,
        -- a spoiler) put no mark on the lines of the blocks they hold.
        OtherShape | any covers s.inner -> go held Nothing s.inner
        _ -> Nothing
    covers s = let (a, b) = extent s in a <= n && n <= b

-- | How each line is read, whatever is on it: the quotations, lists,
-- items, footnotes' definitions and the wiki's own blocks over it (0 to
-- 4), each by the line it
-- starts on, the outermost first; the kind of the innermost block over
-- it; and the column the innermost block that starts a part on the line
-- starts it at. A list counts, so that two lists a new blank line makes
-- one read otherwise; the column, so that lines a footnote's definition
-- takes in read otherwise (the parser places no footnote, only what is in
-- it). One walk of the shapes, a later or deeper shape over a line
-- taking it from those before (as 'placeOf' takes the last of the shapes
-- over a line).
readings :: [Shape] -> Map Int ([(Int, Int)], ShapeKind, Int)
readings = foldl' (go []) Map.empty
  where
    go held acc s =
      let (a, b) = extent s
          here = case s.kind of
            QuoteShape -> held <> [(0, a)]
            ListShape -> held <> [(1, a)]
            ItemShape _ _ -> held <> [(2, a)]
            NoteShape -> held <> [(3, a)]
            OtherShape | not (null s.inner) -> held <> [(4, a)]
            _ -> held
          column l = maybe 0 (\(_, c) -> c) . lastMay $ [(l', c) | ((l', c), _) <- s.parts, l' == l]
          kept l = maybe 0 (\(_, _, c) -> c) (Map.lookup l acc)
          marked = foldl' (\m l -> Map.insert l (here, s.kind, if column l > 0 then column l else kept l) m) acc [a .. b]
       in foldl' (go here) marked s.inner

-- | The blocks that took attributes, at any depth: the line each starts
-- on, and the attributes.
attached :: [Shape] -> [(Int, [(Text, Text)])]
attached = sort . concatMap go
  where
    go s = [(fst (extent s), s.attributes) | not (null s.attributes)] <> concatMap go s.inner

-- | The first and the last line of a shape ('shapeLines').
extent :: Shape -> (Int, Int)
extent = shapeLines

-- | The last line of a shape with text on it. A container's blank lines
-- at its end are not text; an empty one's is its first line.
lastTextOf :: Shape -> Int
lastTextOf s = case (s.kind, s.inner) of
  (ItemShape _ _, []) -> fst (extent s)
  (QuoteShape, []) -> fst (extent s)
  (_, []) -> snd (extent s)
  (_, inner) -> fromMaybe (snd (extent s)) (maximumMay (map lastTextOf inner))

----------------------------------------------------------------------
-- The marks on a line

-- | 'Taken' is what a line holds of a container: a quotation's mark as
-- written (the spaces before the @>@, the @>@ and a space after it), an
-- item's mark with the spaces around it on the item's first line, an
-- item's indentation on its later lines. 'Lazy' when the line does not
-- hold the container: a lazy line of a paragraph, a blank line.
data Taken = Taken Text | Lazy
  deriving stock (Eq, Show)

-- | 'Layout' is how an item's first line writes its mark: the spaces
-- before it (after the containers around), the mark, and the number of
-- spaces after it that CommonMark counts in the item's indentation.
data Layout = Layout
  { before :: Int
  , markText :: Text
  , gap :: Int
  }
  deriving stock (Show)

-- | The width of an item's indentation: what its later lines start with.
indentOf :: Layout -> Int
indentOf l = l.before + T.length l.markText + l.gap

-- | How each item among some containers writes its mark on its first
-- line, in order (none for a quotation); nothing when a first line has a
-- tab among its marks, or not the mark the parser read there.
layouts :: [Text] -> [Container] -> Maybe [Maybe Layout]
layouts textLines = fmap (map snd) . foldM next []
  where
    next done c = case c of
      Quotation _ -> pure (done <> [(c, Nothing)])
      Item item -> do
        line <- atMay textLines (item.firstLine - 1)
        (taken, rest) <- walkLine item.firstLine line done
        guard (Lazy `notElem` taken)
        let at = T.length line - T.length rest
            before = item.markColumn - 1 - at
        guard (before >= 0 && T.all (== ' ') (T.take before rest))
        let markText = markWritten item.mark (T.drop before rest)
            afterMark = T.drop (before + T.length markText) rest
            spaces = T.length (T.takeWhile (== ' ') afterMark)
        -- The marks end where the item's text starts: after the blanks
        -- after the mark, and for a task after its box and the blanks
        -- after that. A tab among them is a tab among the marks, whose
        -- width depends on its column; a tab in the text is none.
        let blanks = T.takeWhile (\ch -> ch == ' ' || ch == '\t')
            gapRun = blanks afterMark
            boxRun = if item.task then blanks (T.drop 1 (T.dropWhile (/= ']') (T.drop (T.length gapRun) afterMark))) else ""
        guard (not (T.null markText) && T.all (/= '\t') (gapRun <> boxRun))
        -- CommonMark's rule for the spaces an item's indentation counts:
        -- one to four before its text, or one when there are more (its
        -- text is then indented code) or none at all.
        let gap = if T.null (T.strip afterMark) || spaces > 4 then 1 else spaces
        pure (done <> [(c, Just Layout {before, markText, gap})])

-- | Walk a line through containers, each with its layout: what the line
-- holds of each, and the rest of the line. A container the line does not
-- hold, and every one inside it, is 'Lazy'. Nothing when a tab is among
-- what the line holds.
walkLine :: Int -> Text -> [(Container, Maybe Layout)] -> Maybe ([Taken], Text)
walkLine n = go []
  where
    go acc line = \case
      [] -> pure (reverse acc, line)
      (c, layout) : cs -> do
        let spaces = T.takeWhile (== ' ') line
            next = T.drop (T.length spaces) line
        case (c, layout) of
          (Quotation _, _)
            | T.length spaces <= 3
            , Just afterMark <- T.stripPrefix ">" next -> do
                guard (not ("\t" `T.isPrefixOf` afterMark))
                let gap = T.take 1 afterMark & T.filter (== ' ')
                go (Taken (spaces <> ">" <> gap) : acc) (T.drop (T.length gap) afterMark) cs
          (Item item, Just l)
            | item.firstLine == n -> do
                let held = T.take (indentOf l) line
                guard (T.all (/= '\t') held)
                go (Taken held : acc) (T.drop (indentOf l) line) cs
            | not (T.null (T.strip line)) && T.length spaces >= indentOf l ->
                go (Taken (T.take (indentOf l) line) : acc) (T.drop (indentOf l) line) cs
          _ -> do
            guard (not ("\t" `T.isPrefixOf` next))
            pure (reverse acc <> (Lazy <$ ((c, layout) : cs)), line)

-- | A list's mark as written at the start of a text: its bullet, or its
-- digits and its delimiter; empty when the text does not start with one.
markWritten :: ItemMark -> Text -> Text
markWritten mark t = case mark of
  Bullet c
    | T.take 1 t == T.singleton c -> T.singleton c
    | otherwise -> ""
  Numbered d ->
    let digits = T.takeWhile isDigit t
     in if not (T.null digits) && T.length digits <= 9 && T.take 1 (T.drop (T.length digits) t) == T.singleton d
          then digits <> T.singleton d
          else ""

----------------------------------------------------------------------
-- The answer

-- | 'Expected' is how the parser should read the text an answer writes, on
-- the line the cursor ends on.
data Expected
  = -- | In these containers (their signatures), then in a new item of the
    -- list starting on this line, a task or not, with nothing after its
    -- mark (and box).
    NewItem [(Bool, Int)] Int Bool
  | -- | As 'NewItem', with text after its mark (and box): the item split
    -- off.
    SplitItem [(Bool, Int)] Int Bool
  | -- | In these containers, the last a quotation whose marks the line
    -- has, and nothing after them.
    QuotedLine [(Bool, Int)]
  | -- | In no container: an empty line.
    Outside
  deriving stock (Show)

-- | The answer for a line, and how the parser should read what it writes;
-- nothing for a plain line break.
decide :: [Text] -> Int -> Place -> Maybe (Enter, Expected)
decide textLines n place = do
  layout <- layouts textLines place.containers
  line <- atMay textLines (n - 1)
  (taken, rest) <- walkLine n line (zip place.containers layout)
  let blank = T.null (T.strip rest)
      kept = effective n blank place.containers
      held = zip3 kept taken layout
      prefix = prefixOf held
      nextItem = nextItemOf held
      -- The line one level up from the innermost container.
      goUp = case lastMay (initSafe kept) of
        Nothing -> Just (LineBecomes "", Outside)
        Just (Item _) -> first LineBecomes <$> nextItem (length kept - 2)
        Just (Quotation _) -> Just (LineBecomes (prefix (length kept - 1)), QuotedLine (signature (initSafe kept)))
  case lastMay held of
    Nothing -> Nothing
    Just (Item item, _, _)
      | item.firstLine == n && emptyItem item rest -> if item.lastText == n then goUp else Nothing
      | blank || item.lastText /= n || not (place.inText || item.firstLine == n) -> Nothing
      | otherwise -> first NewLineWith <$> nextItem (length kept - 1)
    Just (Quotation _, Taken _, _)
      | blank -> goUp
    Just (Quotation _, _, _)
      | blank -> Nothing
      | otherwise -> Just (NewLineWith (prefix (length kept)), QuotedLine (signature kept))

-- | What a new line starts with in the first so many containers a line is
-- in, each with what the line holds of it and its layout: a quotation's
-- marks as the line writes them, or @> @ where it is lazy; an item's
-- indentation.
prefixOf :: [(Container, Taken, Maybe Layout)] -> Int -> Text
prefixOf held k =
  mconcat
    [ case (c, t, l) of
        (Quotation _, Taken marks, _) -> marks
        (Item _, _, Just l') -> T.replicate (indentOf l') " "
        _ -> "> "
    | (c, t, l) <- take k held
    ]

-- | The start of the next item after the item at this depth of the
-- containers a line is in ('prefixOf'), and how the parser should read
-- it.
nextItemOf :: [(Container, Taken, Maybe Layout)] -> Int -> Maybe (Text, Expected)
nextItemOf held k = case atMay held k of
  Just (Item item, _, Just l) -> do
    next <- case item.mark of
      Bullet c -> Just (T.singleton c)
      Numbered d -> do
        number <- readMaybe (T.unpack (T.dropEnd 1 l.markText)) :: Maybe Integer
        guard (number + 1 <= 999999999)
        Just (show (number + 1) <> T.singleton d)
    let written' = prefixOf held k <> T.replicate l.before " " <> next <> T.replicate l.gap " " <> (if item.task then "[ ] " else "")
    Just (written', NewItem (signature (take k (map (\(c, _, _) -> c) held))) item.listLine item.task)
  _ -> Nothing

-- | Whether an item's first line, after its mark, holds nothing (or its
-- box alone, for a task).
--
-- A task's box is what the parser read as one, from its @[@ to the first
-- @]@: one character or more between them (@[ ]@, @[x]@, @[  ]@).
emptyItem :: ItemAt -> Text -> Bool
emptyItem item rest = T.null (T.strip (if item.task then T.drop 1 (T.dropWhile (/= ']') (T.strip rest)) else rest))

-- | The containers a line is in, without the items at its end that do not
-- have their mark on it when the line has nothing after its marks: such a
-- line is a blank line of those items, not one of their own.
effective :: Int -> Bool -> [Container] -> [Container]
effective n blank cs
  | blank = reverse (dropWhile unmarked (reverse cs))
  | otherwise = cs
  where
    unmarked = \case
      Item item -> item.firstLine /= n
      Quotation _ -> False

-- | What tells containers apart: a quotation by its first line, an item by
-- the line of its mark.
signature :: [Container] -> [(Bool, Int)]
signature = map $ \case
  Quotation line -> (True, line)
  Item item -> (False, item.firstLine)

----------------------------------------------------------------------
-- An item split in two

-- | Enter in the text of an item's first line, with text after the
-- cursor on the line: the item split in two ('SplitWith'), or nothing.
split :: Text -> Text -> Maybe Answered
split before after = do
  let lineBefore = T.takeWhileEnd (/= '\n') before
      lineAfter = T.takeWhile (/= '\n') after
      blanks = T.length (T.takeWhile (== ' ') lineAfter)
      moved = T.drop blanks lineAfter
  -- Text after the cursor, which does not start with a tab: a tab right
  -- after the new item's mark is a tab among its marks.
  guard (not (T.null (T.strip moved)) && not ("\t" `T.isPrefixOf` moved))
  let text = before <> after
      n = T.count "\n" before + 1
      textLines = T.splitOn "\n" text
  (shapes, spans) <- rightToMaybe (parseShapesAndSpans text)
  place <- placeOf shapes n
  guard place.inText
  layout <- layouts textLines place.containers
  line <- atMay textLines (n - 1)
  (taken, rest) <- walkLine n line (zip place.containers layout)
  let held = zip3 place.containers taken layout
  (Item item, _, _) <- lastMay held
  guard (item.firstLine == n)
  -- Where the item's text starts on the line: after its mark, and after
  -- a task's box and the blanks after it.
  let afterBox = if item.task then T.dropWhile (\c -> c == ' ' || c == '\t') (T.drop 1 (T.dropWhile (/= ']') rest)) else rest
      textStart = T.length line - T.length afterBox
      left = T.drop textStart lineBefore
  guard (T.length lineBefore > textStart)
  -- What the text given may not show: the definition of a link's
  -- reference above it. Inside brackets left open the cursor may be in
  -- such a link's text (@[la‸bel]@); between @]@ and @[@, a cut would
  -- part a link's text from its label.
  guard (not (opensBracket left))
  guard (not ("]" `T.isSuffixOf` left && "[" `T.isPrefixOf` lineAfter))
  (start, expected) <- nextItemOf held (length held - 1)
  splitExpected <- case expected of
    NewItem outer listLine task -> Just (SplitItem outer listLine task)
    _ -> Nothing
  let answer = SplitWith blanks start
  spans' <- readsAsMeant before after shapes answer splitExpected
  -- The top-level block the cursor is in: the one item split in two, and
  -- nothing else changed, as the parser reads both. The last block whose
  -- lines run over the cursor's, as in 'placeOf': a block's lines may run
  -- to the start of the block after it (a list of tasks right after a
  -- list of plain items is a list of its own).
  let covers l (at, _) = maybe False (\(a, b) -> a <= l && l <= b) at
      cut = T.takeWhileEnd (\c -> c == ' ' || c == '\t') left <> T.take blanks lineAfter
  guard (length spans' == length spans)
  i <- lastMay [j | (j, s) <- zip [0 ..] spans, covers n s]
  (_, old) <- atMay spans i
  (_, new) <- find (covers n) (take 1 (drop i spans'))
  guard (splitIn cut old new)
  pure Answered {answer, shapes, blocks = spans'}

-- | Whether a text leaves a bracket open: a @[@ with no @]@ after it, a
-- character after a backslash not counted, nor a bracket in code between
-- backticks. Brackets in math are counted, which errs towards a plain
-- line break.
opensBracket :: Text -> Bool
opensBracket = go (0 :: Int) . T.unpack
  where
    go depth = \case
      '\\' : _ : cs -> go depth cs
      '[' : cs -> go (depth + 1) cs
      ']' : cs -> go (max 0 (depth - 1)) cs
      cs@('`' : _) ->
        let (ticks, rest) = span (== '`') cs
         in maybe (go depth rest) (go depth) (closing (length ticks) rest)
      _ : cs -> go depth cs
      [] -> depth > 0
    -- What follows the run of so many backticks that closes a code span.
    closing n cs = case break (== '`') cs of
      (_, []) -> Nothing
      (_, more) ->
        let (ticks, rest) = span (== '`') more
         in if length ticks == n then Just rest else closing n rest

-- | Whether a block after Enter is the block before it with one list's
-- item split in two at a cut where these blanks were taken out: the first
-- item keeps the start of its first paragraph, alone; the second, after
-- it, has the rest of that paragraph, then every other block of the item,
-- and an empty box where the item is a task. The two paragraphs, joined
-- by the blanks, are the old paragraph, inline for inline: a cut inside
-- code, math, a link or emphasis makes inlines of another kind, and a
-- cut that makes a block of another kind (a heading, a nested list, a
-- quotation, a box) leaves no paragraph to join. Everything else is the
-- same.
splitIn :: Text -> Block -> Block -> Bool
splitIn cut old new = case (old, new) of
  (A.List kind items, A.List kind' items')
    | kind == kind' -> splitItems items items' || oneDiffers inItem items items'
  (A.BlockQuote bs, A.BlockQuote bs') -> oneDiffers (splitIn cut) bs bs'
  (A.Alert k h bs, A.Alert k' h' bs') | k == k' && h == h' -> oneDiffers (splitIn cut) bs bs'
  (A.Div a bs, A.Div a' bs') | a == a' -> oneDiffers (splitIn cut) bs bs'
  (A.Spoiler h bs, A.Spoiler h' bs') | h == h' -> oneDiffers (splitIn cut) bs bs'
  _ -> False
  where
    inItem (A.ListItem task bs) (A.ListItem task' bs') = task == task' && oneDiffers (splitIn cut) bs bs'
    splitItems items items' =
      length items' == length items + 1
        && or
          [ take i items == take i items' && drop (i + 1) items == drop (i + 2) items' && pieces o a b
          | (i, o) <- zip [0 ..] items
          , Just a <- [atMay items' i]
          , Just b <- [atMay items' (i + 1)]
          ]
    pieces (A.ListItem task (p : rest)) (A.ListItem task1 [p1]) (A.ListItem task2 (p2 : rest')) =
      task1 == task && task2 == (False <$ task) && rest == rest' && joined p p1 p2
    pieces _ _ _ = False
    joined p p1 p2 = case (p, p1, p2) of
      (A.Para xs, A.Para ys, A.Para zs) -> same xs ys zs
      (A.Plain xs, A.Plain ys, A.Plain zs) -> same xs ys zs
      _ -> False
    same xs ys zs = not (leavesTagOpen ys) && merged xs == merged (ys <> [A.Str cut] <> zs)
    -- Raw HTML is read a tag at a time, so a cut between an opening tag
    -- and its closing tag (@<span class="x">a‸b</span>@, as the toolbar's
    -- colours write) joins back to the same inlines, and parts the
    -- element between two items: the first paragraph may not open more
    -- tags than it closes. A tag that needs no closing one (@<br>@) counts
    -- as opened, which leaves the line break plain.
    leavesTagOpen ys =
      let tags = [t | A.RawHtml t <- ys]
          opening t = "<" `T.isPrefixOf` t && not ("</" `T.isPrefixOf` t) && not ("<!" `T.isPrefixOf` t) && not ("/>" `T.isSuffixOf` t)
       in length (filter opening tags) > length (filter ("</" `T.isPrefixOf`) tags)
    merged = \case
      A.Str a : A.Str b : rest -> merged (A.Str (a <> b) : rest)
      A.Str "" : rest -> merged rest
      x : rest -> x : merged rest
      [] -> []

-- | Whether two lists of the same length differ in exactly one place, and
-- there as this says.
oneDiffers :: (Eq a) => (a -> a -> Bool) -> [a] -> [a] -> Bool
oneDiffers f xs ys =
  length xs == length ys && case [(x, y) | (x, y) <- zip xs ys, x /= y] of
    [(x, y)] -> f x y
    _ -> False

----------------------------------------------------------------------
-- The numbers after a new item

-- | 'renumberLimit' is the most items after a new item Enter renumbers:
-- past it, none. The page numbers a list from its first item whatever
-- the numbers after it say.
renumberLimit :: Int
renumberLimit = 1000

-- | 'Following' is what becomes of the numbers of the items after a new
-- item.
data Following
  = -- | These are rewritten (none, where the list is not renumbered).
    Following [Renumbered]
  | -- | The list may go on past the text given, and nothing was looked at.
    PastText

-- | 'Renumbered' is a number of an item that Enter rewrites: where it
-- starts, in characters after the cursor, the number as written, and the
-- number written in its place.
data Renumbered = Renumbered Int Text Text

-- | The numbers after an answer that adds an item to a numbered list,
-- given whether the text ends where the page ends: each later item of
-- that list (the same list, not one nested in an item, nor one after it)
-- the number before it plus one. Only where those items were numbered one
-- after another from the item the cursor is in, the new number and every
-- one after it have at most nine digits, and they are at most
-- 'renumberLimit'; and where the parser reads the text renumbered as it
-- reads it without, block for block: a number one digit wider moves its
-- item's text one column right, and leaves the item's later lines as
-- they are, which keeps a lazy line in its paragraph and takes a second
-- paragraph indented to the old column out of the item.
following :: Bool -> Text -> Text -> Answered -> Following
following ends before after done = fromMaybe (Following []) $ do
  guard $ case done.answer of
    NewLineWith _ -> True
    SplitWith _ _ -> True
    _ -> False
  let text = before <> after
      n = T.count "\n" before + 1
      textLines = T.splitOn "\n" text
      lineStarts = scanl (\at l -> at + T.length l + 1) 0 textLines
  place <- placeOf done.shapes n
  Item item <- lastMay place.containers
  Numbered delimiter <- Just item.mark
  -- The list the cursor's item is in: the one with an item whose mark
  -- is where the item's is, line and column. A line is not enough: a
  -- list may start on its outer item's first line (@1. 1. a@), and both
  -- lists start there.
  let own = (item.firstLine, item.markColumn)
  numbered <- find (\s -> s.kind == ListShape && any ((== Just own) . markOf) s.inner) (everyShape done.shapes)
  let marks = mapMaybe markOf numbered.inner
      later = drop 1 (dropWhile (/= own) marks)
      numberAt (l, c) = do
        line <- atMay textLines (l - 1)
        let mark = markWritten (Numbered delimiter) (T.drop (c - 1) line)
        guard (not (T.null mark))
        let digits = T.dropEnd 1 mark
        number <- readMaybe (T.unpack digits) :: Maybe Integer
        pure (digits, number)
  (_, first') <- numberAt (item.firstLine, item.markColumn)
  numbers <- traverse numberAt later
  guard (map snd numbers == take (length numbers) [first' + 1 ..])
  -- The list ends in the text, before a line of something else, or the
  -- text ends where the page does. The list's last line is its last
  -- item's: the parser may run a list's own lines on to the start of the
  -- block after it.
  let lastLine = fromMaybe 0 (maximumMay (map (snd . extent) numbered.inner))
      whole = ends || any (not . T.null . T.strip) (drop lastLine textLines)
  if not whole
    then pure PastText
    else do
      guard (not (null numbers) && length numbers <= renumberLimit && first' + 1 + fromIntegral (length numbers) <= 999999999)
      let cursorAt = T.length before
      changes <- for (zip later numbers) $ \((l, c), (digits, number)) -> do
        at <- atMay lineStarts (l - 1)
        let offset = at + c - 1 - cursorAt
        guard (offset >= T.length (T.takeWhile (/= '\n') after))
        pure (Renumbered offset digits (show (number + 1)))
      let renumberedAfter = replaced [(offset, offset + T.length was, now) | Renumbered offset was now <- changes] after
          (before', after') = written before renumberedAfter done.answer
      spans <- rightToMaybe (parseWithSpans (before' <> after'))
      guard (map snd spans == map snd done.blocks)
      pure (Following changes)

-- | Where an item's mark is: its line and its column.
markOf :: Shape -> Maybe (Int, Int)
markOf s = fst <$> head s.parts

-- | Every shape, at any depth.
everyShape :: [Shape] -> [Shape]
everyShape = concatMap (\s -> s : everyShape s.inner)

-- | A text with ranges replaced: each its start and end, in characters,
-- and what goes there, in order, none overlapping.
replaced :: [(Int, Int, Text)] -> Text -> Text
replaced changes t = go 0 changes
  where
    go at = \case
      [] -> T.drop at t
      (a, b, new) : rest -> T.take (a - at) (T.drop at t) <> new <> go b rest

----------------------------------------------------------------------
-- Reading the answer back

-- | Whether the parser reads the text an answer writes as expected, on the
-- line the cursor ends on; and, for a new line, whether it reads the line
-- after it in the same containers as before (given the shapes of the text
-- before the answer). A new item before a line indented as deep as its
-- text would take that line in (@2)    code@ before @   1. text@, which
-- the item @2)@ did not hold).
readsAsMeant :: Text -> Text -> [Shape] -> Enter -> Expected -> Maybe [(Maybe (Int, Int), Block)]
readsAsMeant before after oldShapes answer expected = do
  let (before', after') = written before after answer
      text = before' <> after'
      textLines = T.splitOn "\n" text
      n = T.count "\n" before' + 1
  (shapes, spans) <- rightToMaybe (parseShapesAndSpans text)
  -- Every line after the cursor's with anything on it is read as before:
  -- in the same quotations and items, each by the line it starts on in the
  -- text before the answer (0 for the new line's own), and as the same
  -- kind of block (text, code). A blank line belongs to no item. After a
  -- split, the item the new line starts takes the place of the item split
  -- for the lines below: they were the old item's, and are the new one's.
  let cursor = T.count "\n" before + 1
      oldLines = T.splitOn "\n" (before <> after)
      below = [m | (m, l) <- zip [cursor + 1 ..] (drop cursor oldLines), not (T.null (T.strip l))]
      (moved, renumber) = case answer of
        NewLineWith _ -> (1, \l -> if l == n then 0 else if l > n then l - 1 else l)
        SplitWith _ _ -> (1, \l -> if l >= n then l - 1 else l)
        _ -> (0, identity)
      old = readings oldShapes
      new = readings shapes
      unshifted (containers, kind, column) = (map (second renumber) containers, kind, column)
  guard (all (\m -> Map.lookup m old == fmap unshifted (Map.lookup (m + moved) new)) below)
  -- Every block that took attributes takes the same ones, by the line it
  -- starts on. A line of attributes alone goes to the block after it,
  -- over blank lines: an empty item emptied away would hand its list's
  -- attributes to the list below (@{#a}@ over @- [ ] @ and @- b@, two
  -- lists), and @{.incremental}@ changes how a list is shown.
  guard (attached oldShapes == sort [(renumber l, a) | (l, a) <- attached shapes])
  line <- atMay textLines (n - 1)
  -- The line's containers, what it holds of each, and the rest of it.
  let marks = do
        place <- placeOf shapes n
        layout <- layouts textLines place.containers
        (taken, rest) <- walkLine n line (zip place.containers layout)
        pure (effective n (T.null (T.strip rest)) place.containers, taken, rest)
  meant <- case expected of
    NewItem outer listLine task -> do
      (kept, _, rest) <- marks
      pure $ case lastMay kept of
        Just (Item item) -> signature (initSafe kept) == outer && item.firstLine == n && item.listLine == listLine && item.task == task && emptyItem item rest
        _ -> False
    SplitItem outer listLine task -> do
      (kept, _, rest) <- marks
      pure $ case lastMay kept of
        Just (Item item) -> signature (initSafe kept) == outer && item.firstLine == n && item.listLine == listLine && item.task == task && not (emptyItem item rest)
        _ -> False
    QuotedLine outer -> do
      (kept, taken, rest) <- marks
      pure (signature kept == outer && Lazy `notElem` take (length kept) taken && T.null (T.strip rest))
    -- A line of blanks holds no marks: the items and the footnote's
    -- definition it is in, it is a blank line of. It is in no code, and no
    -- quotation goes on over it (the parser reads one on over a blank line
    -- after an empty item in it, commonmark 0.3.1). What is above the
    -- text 'enter' is given decides nothing more: a footnote's definition
    -- above takes in a blank line as one of its own.
    Outside -> pure (T.null (T.strip line) && maybe True (\(cs, kind, _) -> kind /= CodeShape && not (any ((== 0) . fst) cs)) (Map.lookup n (readings shapes)))
  guard meant
  pure spans
