-- | What Enter writes in the editor's text (WP5.124 (a) of docs/PLAN.md,
-- issue #187): the next item of a list, the next line of a quotation or a
-- box, or, on an empty item or an empty quoted line, the way up one level.
-- CodeMirror's keymap calls 'enter' as the key is pressed, with the text
-- of the top-level block the cursor is in (web/src/editor.js). The
-- function is pure, so every rule is tested natively, with the wiki's own
-- parser as the oracle (decision 83).
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
-- at the end of its line (a selection is the editor's business: it sends
-- one to the plain line break), and it changes only the cursor's line and
-- the new line it makes. No other line changes, and no other item's
-- number: Markdown numbers a list from its first item, so the page shows
-- the same numbers.
--
-- * At the end of the last line with text of a list's item: a new item of
--   the same list at the same indentation, with the same bullet, or the
--   next number and the same delimiter (@.@ or @)@), and an empty box
--   after a task, done or not (@- [ ] @). The parser reads a task only in
--   a bulleted list: after @1. [x] done@ comes @2. @, as after any text.
--   The containers around the list go on before it: a quotation's marks
--   as the cursor's line writes them (@>  > @), an outer item's
--   indentation.
-- * At the end of a line of a quotation or a box (lazy lines of its
--   paragraph too): its marks on the new line, as the cursor's line writes
--   them, @> @ where it is lazy, after the indentation of the items around
--   it.
-- * On an empty item (its mark, its box if it is a task, and nothing
--   else) and on an empty line of a quotation (its marks and nothing
--   else): up one level. The line becomes what Enter would start after
--   the container around: the next item of the item around, the marks of
--   the quotation around, or, at the top level, an empty line.
--
-- Items inside quotations and quotations inside items, at any depth, and
-- loose lists are read as the parser reads them. Everything else is a
-- plain line break:
--
-- * a cursor before the end of its line;
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
-- * an answer the parser does not read back as meant: 'enter' parses the
--   text it would write, and keeps its answer only when the new line is in
--   the containers the answer says, and every line below with anything on
--   it is read as before ('readsAsMeant'): an empty item's text is
--   indented by its mark and one space, so a line below indented that far
--   would go into it; a quoted line in the middle of a paragraph can make
--   the lines below code; a line gone up can join two lists, or take an
--   item nested below into the new one.
--
-- So an empty item or quoted line in the middle of a list or a quotation
-- goes up only when the lines after it keep their reading; otherwise
-- Enter there is a plain line break.
module Wiki.Core.Markdown.Enter
  ( Enter (..)
  , enter
  , written
  )
where

import Data.List (zip3)
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Protolude
import Wiki.Core.Markdown.Ast (ItemMark (..), Shape (..), ShapeKind (..), parseShapes)

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
enter before after
  | not (T.null after || "\n" `T.isPrefixOf` after) = NewLine
  | otherwise = fromMaybe NewLine $ do
      let text = before <> after
          n = T.count "\n" before + 1
      shapes <- rightToMaybe (parseShapes text)
      place <- placeOf shapes n
      (answer, expected) <- decide (T.splitOn "\n" text) n place
      guard (readsAsMeant before after shapes answer expected)
      pure answer

-- | The text after Enter's answer, as the text before the cursor and the
-- text after it.
written :: Text -> Text -> Enter -> (Text, Text)
written before after = \case
  NewLine -> (before <> "\n", after)
  NewLineWith start -> (before <> "\n" <> start, after)
  LineBecomes line -> (T.dropWhileEnd (/= '\n') before <> line, after)

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
-- items and the wiki's own blocks over it (0 to 4), each by the line it
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
            OtherShape | not (null s.inner) -> held <> [(4, a)]
            _ -> held
          column l = maybe 0 (\(_, c) -> c) . lastMay $ [(l', c) | ((l', c), _) <- s.parts, l' == l]
          kept l = maybe 0 (\(_, _, c) -> c) (Map.lookup l acc)
          marked = foldl' (\m l -> Map.insert l (here, s.kind, if column l > 0 then column l else kept l) m) acc [a .. b]
       in foldl' (go here) marked s.inner

-- | The first and the last line of a shape, its inner shapes' included.
extent :: Shape -> (Int, Int)
extent s =
  let own = [(l, if c == 1 then l' - 1 else l') | ((l, _), (l', c)) <- s.parts]
   in ( fromMaybe 0 (minimumMay (map fst own <> map (fst . extent) s.inner))
      , fromMaybe 0 (maximumMay (map snd own <> map (snd . extent) s.inner))
      )

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
        guard (not (T.null markText) && T.all (/= '\t') (T.take 5 afterMark))
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
      -- What a new line starts with in some of the containers (a prefix
      -- of these): a quotation's marks as the cursor's line writes them,
      -- or @> @ where it is lazy; an item's indentation.
      prefix k =
        mconcat
          [ case (c, t, l) of
              (Quotation _, Taken marks, _) -> marks
              (Item _, _, Just l') -> T.replicate (indentOf l') " "
              _ -> "> "
          | (c, t, l) <- take k held
          ]
      -- The next item after the item at this depth.
      nextItem k = case atMay held k of
        Just (Item item, _, Just l) -> do
          next <- case item.mark of
            Bullet c -> Just (T.singleton c)
            Numbered d -> do
              number <- readMaybe (T.unpack (T.dropEnd 1 l.markText)) :: Maybe Integer
              guard (number + 1 <= 999999999)
              Just (show (number + 1) <> T.singleton d)
          let written' = prefix k <> T.replicate l.before " " <> next <> T.replicate l.gap " " <> (if item.task then "[ ] " else "")
          Just (written', NewItem (signature (take k kept)) item.listLine item.task)
        _ -> Nothing
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

-- | Whether an item's first line, after its mark, holds nothing (or its
-- box alone, for a task).
emptyItem :: ItemAt -> Text -> Bool
emptyItem item rest = T.null (T.strip (if item.task then T.drop 3 (T.strip rest) else rest))

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
-- Reading the answer back

-- | Whether the parser reads the text an answer writes as expected, on the
-- line the cursor ends on; and, for a new line, whether it reads the line
-- after it in the same containers as before (given the shapes of the text
-- before the answer). A new item before a line indented as deep as its
-- text would take that line in (@2)    code@ before @   1. text@, which
-- the item @2)@ did not hold).
readsAsMeant :: Text -> Text -> [Shape] -> Enter -> Expected -> Bool
readsAsMeant before after oldShapes answer expected = fromMaybe False $ do
  let (before', after') = written before after answer
      text = before' <> after'
      textLines = T.splitOn "\n" text
      n = T.count "\n" before' + 1
  shapes <- rightToMaybe (parseShapes text)
  place <- placeOf shapes n
  -- Every line after the cursor's with anything on it is read as before:
  -- in the same quotations and items, each by the line it starts on in the
  -- text before the answer (0 for the new line's own), and as the same
  -- kind of block (text, code). A blank line belongs to no item.
  let cursor = T.count "\n" before + 1
      oldLines = T.splitOn "\n" (before <> after)
      below = [m | (m, l) <- zip [cursor + 1 ..] (drop cursor oldLines), not (T.null (T.strip l))]
      (moved, renumber) = case answer of
        NewLineWith _ -> (1, \l -> if l == n then 0 else if l > n then l - 1 else l)
        _ -> (0, identity)
      old = readings oldShapes
      new = readings shapes
      unshifted (containers, kind, column) = (map (second renumber) containers, kind, column)
  guard (all (\m -> Map.lookup m old == fmap unshifted (Map.lookup (m + moved) new)) below)
  line <- atMay textLines (n - 1)
  -- The line's containers, what it holds of each, and the rest of it.
  let marks = do
        layout <- layouts textLines place.containers
        (taken, rest) <- walkLine n line (zip place.containers layout)
        pure (effective n (T.null (T.strip rest)) place.containers, taken, rest)
  case expected of
    NewItem outer listLine task -> do
      (kept, _, rest) <- marks
      pure $ case lastMay kept of
        Just (Item item) -> signature (initSafe kept) == outer && item.firstLine == n && item.listLine == listLine && item.task == task && emptyItem item rest
        _ -> False
    QuotedLine outer -> do
      (kept, taken, rest) <- marks
      pure (signature kept == outer && Lazy `notElem` take (length kept) taken && T.null (T.strip rest))
    -- A line of blanks holds no marks: the items it is in, it is a blank
    -- line of (placeOf has said it is in no code). No quotation goes on
    -- over it (the parser reads one on over a blank line after an empty
    -- item in it, commonmark 0.3.1).
    Outside -> pure (T.null (T.strip line) && maybe True (\(cs, _, _) -> not (any ((== 0) . fst) cs)) (Map.lookup n (readings shapes)))
