-- | Enter in the editor's text ("Wiki.Core.Markdown.Enter"): unit cases
-- written with the cursor at @‸@, each the text before Enter and after
-- it; and properties over generated documents, with the wiki's own parser
-- as the oracle (decision 83).
--
-- The documents are made of chunks, each a few lines in containers: the
-- containers it keeps of the chunk before (a quotation's marks again, an
-- item's indentation) and new ones (a quotation's mark, an item's mark,
-- a task's box), written with the spacing CommonMark allows and some it
-- reads otherwise (spaces before a mark, one to five after it, a tab),
-- and a body: a paragraph, one with a lazy line after it, a heading, a
-- blank line, nothing (an empty item or quoted line), indented code, a
-- fence, a fence left open, HTML, a table, a box's marker (a box where
-- the chunk starts a quotation). Lists are nested at odd
-- indentations, numbered with @.@ and @)@ up to nine digits, and loose
-- where a blank chunk falls between items.
module Test.Markdown.Enter (spec) where

import Data.Text qualified as T
import GHC.Conc (getAllocationCounter)
import Protolude
import Test.Cases (coverage)
import Test.Hspec hiding (after, before)
import Test.Hspec.QuickCheck (modifyMaxSuccess, prop)
import Test.QuickCheck (Arbitrary (..), Gen, Property, choose, conjoin, counterexample, cover, elements, frequency, listOf1, oneof, property, (.&&.), (===))
import Wiki.Core.Markdown.Ast (Block, Shape (..), ShapeKind (..), parseMarkdown, parseShapes, parseWithSpans)
import Wiki.Core.Markdown.Ast qualified as A
import Wiki.Core.Markdown.Enter
import Wiki.Core.Markdown.Problems qualified as Problems
import Wiki.Core.Markdown.Starts (Kept (..), blockStarts, textRange)
import Wiki.Core.PageKind (PageKind (..))

spec :: Spec
spec = describe "Wiki.Core.Markdown.Enter" $ do
  describe "a list's item" $ do
    "- one‸" `becomes` "- one\n- ‸"
    "* a‸" `becomes` "* a\n* ‸"
    "+ a‸" `becomes` "+ a\n+ ‸"
    "1. a‸" `becomes` "1. a\n2. ‸"
    "1) a‸" `becomes` "1) a\n2) ‸"
    "- [x] done‸" `becomes` "- [x] done\n- [ ] ‸"
    "- [ ] todo‸" `becomes` "- [ ] todo\n- [ ] ‸"
    "- one\n  - two‸" `becomes` "- one\n  - two\n  - ‸"
    "  -   spaced‸" `becomes` "  -   spaced\n  -   ‸"
    "> - a‸" `becomes` "> - a\n> - ‸"
    "- > - a‸" `becomes` "- > - a\n  > - ‸"
    it "goes on after an item's later line" $ press "- one\n  more‸" `shouldBe` "- one\n  more\n- ‸"
    it "goes on after a loose list's item" $ press "- a\n\n- b‸" `shouldBe` "- a\n\n- b\n- ‸"
    it "goes on after a loose list's first item, the blank line after the new one" $ press "- a‸\n\n- b" `shouldBe` "- a\n- ‸\n\n- b"
    it "leaves the numbers below as they are" $ press "1. a‸\n2. b" `shouldBe` "1. a\n2. ‸\n2. b"
    it "goes on after an item's second paragraph" $ press "- one\n\n  more‸" `shouldBe` "- one\n\n  more\n- ‸"
    it "goes on after a lazy line of an item" $ press "- one\nlazy‸" `shouldBe` "- one\nlazy\n- ‸"

  describe "an empty item" $ do
    "- one\n- ‸" `becomes` "- one\n‸"
    "- one\n-‸" `becomes` "- one\n‸"
    "- one\n  - two\n  - ‸" `becomes` "- one\n  - two\n- ‸"
    "1. a\n   - b\n   - ‸" `becomes` "1. a\n   - b\n2. ‸"
    "- [x] a\n  - [x] b\n  - [ ] ‸" `becomes` "- [x] a\n  - [x] b\n- [ ] ‸"
    "> - a\n> - ‸" `becomes` "> - a\n> ‸"
    "- [ ] ‸" `becomes` "‸"
    "- [  ] ‸" `becomes` "‸"
    "- [   ] ‸" `becomes` "‸"
    it "breaks the line alone with an item below that going up would take in" $
      press "- a\n  - ‸\n  - c" `shouldBe` "- a\n  - \n‸\n  - c"

  describe "a quotation or a box" $ do
    "> a‸" `becomes` "> a\n> ‸"
    "> [!NOTE]‸" `becomes` "> [!NOTE]\n> ‸"
    "> [!NOTE]\n> text‸" `becomes` "> [!NOTE]\n> text\n> ‸"
    "> > q‸" `becomes` "> > q\n> > ‸"
    ">  > q‸" `becomes` ">  > q\n>  > ‸"
    "- a\n  > q‸" `becomes` "- a\n  > q\n  > ‸"
    it "goes on after a lazy line" $ press "> a\nlazy‸" `shouldBe` "> a\nlazy\n> ‸"
    it "goes on in the middle of a quotation" $ press "> a‸\n> b" `shouldBe` "> a\n> ‸\n> b"

  describe "an empty quoted line" $ do
    "> a\n> ‸" `becomes` "> a\n‸"
    "> > q\n> > ‸" `becomes` "> > q\n> ‸"
    ">  > q\n>  > ‸" `becomes` ">  > q\n> ‸"
    "> - a\n> ‸" `becomes` "> - a\n‸"
    "- a\n  > q\n  > ‸" `becomes` "- a\n  > q\n- ‸"

  describe "a plain line break" $ do
    "text‸" `becomes` "text\n‸"
    "    code‸" `becomes` "    code\n‸"
    ">     code‸" `becomes` ">     code\n‸"
    "- a\n\n      code‸" `becomes` "- a\n\n      code\n‸"
    "-     a‸" `becomes` "-     a\n‸"
    "```\n- a‸\n```" `becomes` "```\n- a\n‸\n```"
    "> ```\n> ‸" `becomes` "> ```\n> \n‸"
    "<div>\n- a‸\n</div>" `becomes` "<div>\n- a\n‸\n</div>"
    "1.\tone‸" `becomes` "1.\tone\n‸"
    "- [ ]\tone‸" `becomes` "- [ ]\tone\n‸"
    "999999999. a‸" `becomes` "999999999. a\n‸"
    "- on‸e" `becomes` "- on\n‸e"
    "8. a\n9. b‸\n   - child" `becomes` "8. a\n9. b\n‸\n   - child"
    "- a\n  ‸" `becomes` "- a\n  \n‸"
    "[^a]: - note‸" `becomes` "[^a]: - note\n‸"
    "| a | b |\n| - | - |‸" `becomes` "| a | b |\n| - | - |\n‸"

  -- Each case the reviews of #292 found in the JavaScript built on the
  -- library, made of the generator's own chunks, so that the properties
  -- below can meet it: the answer the parser's reading gives now.
  describe "the cases of #292's reviews" $ do
    let review name doc cursor expected = it name $ do
          let (before, after) = cursorAfter doc cursor
          enter before after `shouldBe` expected
    review "two spaces between a quotation's marks" (Doc [Chunk [] [Quote 0 (Spaces 2), Quote 0 (Spaces 1)] Text]) 1 (NewLineWith ">  > ")
    review "indented code in a quotation" (Doc [Chunk [] [Quote 0 (Spaces 1)] Indented]) 1 NewLine
    review "a tab after a number" (Doc [Chunk [] [Item 0 "1." Tab Nothing] Text]) 1 NewLine
    review "a numbered item that looks like a task, which the parser reads as text" (Doc [Chunk [] [Item 0 "1." (Spaces 1) (Just "x")] Text]) 1 (NewLineWith "2. ")
    review "an indented number of nine digits" (Doc [Chunk [] [Item 1 "123456789." (Spaces 1) Nothing] Text]) 1 (NewLineWith " 123456790. ")
    review "a numbered item with a child, which a new item would take" (Doc [Chunk [] [Item 0 "8." (Spaces 1) Nothing] Text, Chunk [] [Item 0 "9." (Spaces 1) Nothing] Text, Chunk [Item 0 "9." (Spaces 1) Nothing] [Item 0 "-" (Spaces 1) Nothing] Text]) 2 NewLine
    review "a loose list" (Doc [Chunk [] [Item 0 "-" (Spaces 1) Nothing] Text, Chunk [] [] Blank, Chunk [] [Item 0 "-" (Spaces 1) Nothing] Text]) 3 (NewLineWith "- ")
    review "a lazy line of a quotation" (Doc [Chunk [] [Quote 0 (Spaces 1)] TextLazy]) 2 (NewLineWith "> ")
    -- Codex's round 7 on #292: emptying the task away handed the line of
    -- attributes to the list below (a task's list and a list of plain
    -- items are two lists).
    let task = Item 0 "-" (Spaces 1) (Just " ")
        plainItem = Item 0 "-" (Spaces 1) Nothing
    review "an empty task under a line of attributes, a list below" (Doc [Chunk [] [] Attrs, Chunk [] [task] Empty, Chunk [] [plainItem] Text]) 2 NewLine
    review "the same in a quotation" (Doc [Chunk [] [Quote 0 (Spaces 1)] Attrs, Chunk [Quote 0 (Spaces 1)] [task] Empty, Chunk [Quote 0 (Spaces 1)] [plainItem] Text]) 2 NewLine

  -- Texts CodeMirror's tree cut where the wiki's parser reads on (Codex's
  -- review of #292's 0dd1018b): from the start the check keeps, the text
  -- handed over answers as the whole text does. CodeMirror gave
  -- "# Heading\n- " (the HTML above read on, for the wiki's parser) and
  -- "- a\n[[/notes]]" (the start marker in the paragraph above), which
  -- answered "- " and LineBecomes "".
  describe "the text handed over from the start the last check kept" $ do
    let kept name marked' expected = it name $ do
          let (before, rest) = T.breakOn "‸" marked'
              after = T.drop 1 rest
          (editorAnswer before after unchanged, enter before after) `shouldBe` (expected, expected)
    kept "HTML that reads on over a heading and a list" "- a\n<a>\n# Heading\n- ‸" NewLine
    kept "an end marker after a start marker in a paragraph" "plain\n[[!notes]]\n\n- a\n[[/notes]]‸" NewLine
    kept "an item after a footnote's definition" "[^1]: text\n\n    more\n\n- a‸" (NewLineWith "- ")
    -- Codex's round 6 on #292: the check read "- a\n\nx\n\n  b", and the
    -- paragraph between was deleted since. Its start and the item's below
    -- map onto one place, which counted twice cut "  b" off.
    it "keeps two starts a deletion put in one place from cutting the item's line below off" $ do
      let (before, after) = ("- a", "\n  b")
      (maybe NewLine (uncurry enter) (handedOverAt before after 3 1 "\n\nx\n\n"), enter before after) `shouldBe` (NewLine, NewLine)
    -- Codex's round 6: CommonMark counts a carriage return alone as a line
    -- break, and the editor does not, so the lines differ.
    it "keeps no starts, and breaks the line alone, in a text holding a carriage return" $ do
      (Problems.checkText Document "p\rp\rp\n\n```\n\n- a\n- b\n```\n\nend").starts `shouldBe` []
      enter "p\rp\n\n- a" "" `shouldBe` NewLine

  -- What Enter reads, and so what it costs, is bounded: by the text the
  -- editor hands over, the cursor's block and the one below it, and past
  -- 'textLimit' by nothing at all.
  describe "what Enter costs" $ do
    it "breaks the line alone past its limit, reading nothing" $ do
      let items = T.intercalate "\n" ["- item " <> show i | i <- [1 .. 1000 :: Int]]
      _ <- evaluate (T.length items)
      (answer, allocated) <- allocations (enter items "")
      -- 1,488 bytes measured natively on 2026-11-16, unoptimised.
      (answer, allocated < 100_000) `shouldBe` (NewLine, True)
    -- A page-level list's items are starts: the editor hands over the
    -- items around the cursor's, not the whole list.
    it "goes on at the end and in the middle of a list of a thousand items, reading a few of them" $ do
      let ls = "# Items" : "" : ["- item " <> show i | i <- [1 .. 1000 :: Int]]
          at n = fromMaybe ("", "") (handedOver (T.intercalate "\n" (take n ls)) (mconcat ["\n" <> l | l <- drop n ls]) unchanged)
      for_ [length ls, 502] $ \n -> do
        let (given, taken) = at n
        _ <- evaluate (T.length given + T.length taken)
        (answer, allocated) <- allocations (enter given taken)
        -- At the end: 22 characters and 0.77 MB measured natively on
        -- 2026-11-17, unoptimised; the middle reads three items.
        (answer, T.length given + T.length taken < 100, allocated < 5_000_000) `shouldBe` (NewLineWith "- ", True, True)
    it "reads only the cursor's block and the one below it in a far slide of a long deck" $ do
      let ls = T.splitOn "\n" longDeck
          -- The end of slide 50's last item.
          n = 2 + 100 * 49 + 36
          before = T.intercalate "\n" (take n ls)
          after = mconcat ["\n" <> l | l <- drop n ls]
          (given, taken) = fromMaybe ("", "") (handedOver before after unchanged)
      _ <- evaluate (T.length given + T.length taken)
      (answer, allocated) <- allocations (enter given taken)
      -- 407 characters and 6.6 MB measured natively on 2026-11-16,
      -- unoptimised; the deck is 300 KB.
      (answer, T.length given + T.length taken < 1000, allocated < 20_000_000) `shouldBe` (NewLineWith "- ", True, True)

  -- Ten times the suite's count: 1,000 documents natively, a quarter of
  -- that on wasm (Test.Cases), each pressed at every line's end.
  modifyMaxSuccess (* 10) $ do
    -- The answer is forced at every line's end, and its text holds no line
    -- break: what it writes stays on the cursor's line and one new line.
    prop "writes only on the cursor's line and one new line, and never throws" $ \doc ->
      atEveryLineEnd doc $ \_ _ answer -> case answer of
        NewLine -> property True
        NewLineWith t -> counterexample "a new line's start with a line break in it" (not ("\n" `T.isInfixOf` t))
        LineBecomes t -> counterexample "a line with a line break in it" (not ("\n" `T.isInfixOf` t))
    -- The parser reads the cursor's line as code or HTML (alone, or in a
    -- quotation or an item): Enter is a plain line break.
    prop "breaks the line alone in code and HTML" $ \doc ->
      atEveryLineEnd doc $ \before after answer ->
        let n = T.count "\n" before + 1
         in if inCode (before <> after) n then answer === NewLine else property True
    -- A new line the parser reads as a new item: the tree is the old tree
    -- with one empty item after one of a list's items (a task after a
    -- task), and nothing else changed, the lines below included.
    prop "adds exactly one empty item, after an item, where it starts a new item" $ \doc ->
      atEveryLineEnd doc $ \before after answer -> case answer of
        NewLineWith _ | newItemOn before after answer -> itemAdded before after answer
        _ -> property True
    -- Any other new line is in the same quotations and items, in order,
    -- as the cursor's line.
    prop "keeps a new quoted line in the quotations and items of the cursor's line" $ \doc ->
      atEveryLineEnd doc $ \before after answer -> case answer of
        NewLineWith _ | not (newItemOn before after answer) -> quotedLineKept before after answer
        _ -> property True
    -- Whatever Enter writes, the top-level blocks before the cursor's and
    -- after it are read as before.
    prop "leaves the top-level blocks around the cursor's as they were" $ \doc ->
      atEveryLineEnd doc $ \before after answer -> case answer of
        NewLine -> property True
        _ -> blocksAroundKept before after answer
    -- A line replaced is one container less deep (an item of the list
    -- around counts as deep as its sibling), with nothing after its marks.
    -- Each block that took attributes, from its own braces or from a line
    -- of attributes alone above it, takes the same ones: a list's are
    -- shown, a paragraph's and a quotation's are dropped.
    prop "leaves every line of attributes to the block it went to" $ \doc ->
      atEveryLineEnd doc $ \before after answer -> case answer of
        NewLine -> property True
        _ -> attributesKept before after answer
    prop "takes an empty item or quoted line up one level" $ \doc ->
      atEveryLineEnd doc $ \before after answer -> case answer of
        LineBecomes _ -> wentUp before after answer
        _ -> property True
    -- What the editor hands over gives the answer the whole page gives:
    -- the text from the start the last check kept at or above the
    -- cursor's line and before the first change since, mapped through the
    -- change ('handedOver'), for a change anywhere (letters typed in the
    -- cursor's block, a block added below, one edited above).
    prop "answers as for the whole text, for the text from the start the last check kept" $ \doc change ->
      atEveryLineEnd doc $ \before after answer -> case handedOver before after change of
        Just (given, taken) -> counterexample (show (given, taken)) (enter given taken === answer)
        Nothing -> property True
    -- Not only safe but useful: in containers written as CommonMark reads
    -- them without doubt, Enter at the end of an item's or a quotation's
    -- text goes on in it, as the containers say.
    prop "goes on in a list or a quotation written plainly" $ \(Plain doc) ->
      let rendered = render doc
          expected = plainExpected doc
       in counterexample (toS (rendered <> "‸"))
            $ coverage
            $ cover 30 (isNewItem expected) "an item"
            $ cover 20 (not (isNewItem expected)) "a quoted line"
            $ enter rendered "" === expected
    -- The documents meet every kind of answer often enough for the
    -- properties above to mean something.
    prop "covers every answer" $ \doc ->
      let answers = [enter b a | (b, a) <- lineEnds (render doc)]
       in coverage
            $ cover 30 (any isNewItem answers) "a new item"
            $ cover 15 (any isQuoted answers) "a new quoted line"
            $ cover 10 (any isUp answers) "a line gone up"
            $ cover 50 (NewLine `elem` answers) "a plain line break" True
  where
    isNewItem = \case
      NewLineWith t -> not (T.null (T.strip (T.filter (/= '>') t)))
      _ -> False
    isQuoted = \case
      NewLineWith t -> T.null (T.strip (T.filter (/= '>') t))
      _ -> False
    isUp = \case
      LineBecomes _ -> True
      _ -> False

----------------------------------------------------------------------
-- Unit cases

-- | The text after Enter at the @‸@, with the cursor at @‸@.
press :: Text -> Text
press marked' =
  let (before, rest) = T.breakOn "‸" marked'
      after = T.drop 1 rest
      (written', rest') = written before after (enter before after)
   in written' <> "‸" <> rest'

becomes :: Text -> Text -> Spec
becomes old new = it (show old <> " becomes " <> show new) (press old `shouldBe` new)

----------------------------------------------------------------------
-- Generated documents

-- | A container a chunk's lines are in.
data Layer
  = -- | A quotation: the spaces before its mark, and what follows it.
    Quote Int Gap
  | -- | A list's item: the spaces before its mark, the mark, what follows
    -- it, and a task's box (what is between its brackets: one to three
    -- spaces, or @x@).
    Item Int Text Gap (Maybe Text)
  | -- | A footnote's definition, @[^7]: @, its later lines indented by
    -- four spaces: a container CodeMirror's Markdown does not know. Its
    -- label is drawn from a million, so that no two are the same: the
    -- parser drops a second definition of a label, blocks and all, which
    -- makes the reading of a line depend on a definition anywhere above
    -- it, not only on the blocks around it.
    Note Int
  deriving stock (Eq, Show)

-- | What follows a mark: spaces, or a tab.
data Gap = Spaces Int | Tab
  deriving stock (Eq, Show)

-- | What a chunk's lines hold after their containers' marks.
data Body = Attrs | Text | TextLazy | Heading | Box | Blank | Empty | Indented | Fenced | OpenFence | Html | Table | Opener | Closer | TextOpener | RawHtml Int
  deriving stock (Eq, Show)

-- | Some lines: the containers kept from the chunk before, written as on
-- a later line, the new ones, written on the first line with their marks,
-- and the body.
data Chunk = Chunk [Layer] [Layer] Body
  deriving stock (Eq, Show)

-- | A document: its chunks.
newtype Doc = Doc [Chunk]
  deriving stock (Eq, Show)

instance Arbitrary Doc where
  arbitrary = do
    k <- choose (1, 8)
    Doc . reverse . fst <$> foldM (\(done, stack) () -> chunk stack >>= \c@(Chunk kept fresh _) -> pure (c : done, kept <> fresh)) ([], []) (replicate k ())

-- | A chunk after a chunk in these containers: some of them kept, and up
-- to two new ones; an item at a depth where one was is often its list's
-- next item.
chunk :: [Layer] -> Gen Chunk
chunk stack = do
  k <- choose (0, length stack)
  let kept = take k stack
  fresh <- do
    n <- frequency [(2, pure 0), (3, pure 1), (2, pure 2)]
    first' <- case drop k stack of
      Item b m g t : _ | n > 0 -> frequency [(2, pure [Item b (nextMark m) g t]), (1, (: []) <$> layer)]
      _ -> replicateM (min n 1) layer
    rest <- replicateM (max 0 (n - 1)) layer
    -- A footnote's definition at the top level only, where a page writes
    -- one (the parser reads one in an item too, and lists it at the end).
    footnote <- if k == 0 then frequency [(5, pure []), (1, (: []) . Note <$> choose (1, 1000000))] else pure []
    pure (footnote <> first' <> rest)
  body <- frequency [(2, pure Attrs), (6, pure Text), (2, pure TextLazy), (1, pure Heading), (1, pure Box), (2, pure Blank), (2, pure Empty), (1, pure Indented), (1, pure Fenced), (1, pure OpenFence), (1, pure Html), (1, pure Table), (1, pure Opener), (1, pure Closer), (1, pure TextOpener), (2, RawHtml <$> choose (1, 7))]
  pure (Chunk kept fresh body)

-- | A new container.
layer :: Gen Layer
layer =
  frequency
    [ (2, Quote <$> frequency [(4, pure 0), (1, choose (1, 3))] <*> gap)
    , (3, Item <$> frequency [(4, pure 0), (1, choose (1, 3))] <*> elements ["-", "-", "*", "+", "1.", "2)", "10.", "123456789.", "999999999."] <*> gap <*> frequency [(3, pure Nothing), (1, Just <$> elements [" ", "x", "  ", "   "])])
    ]
  where
    gap = frequency [(6, pure (Spaces 1)), (2, Spaces <$> choose (0, 5)), (1, pure Tab)]

-- | The mark of a list's next item.
nextMark :: Text -> Text
nextMark m = case T.unsnoc m of
  Just (digits, d) | not (T.null digits), T.all (`elem` ['0' .. '9']) digits -> maybe m (\n -> show (n + 1 :: Integer) <> T.singleton d) (readMaybe (T.unpack digits))
  _ -> m

-- | A document's text.
render :: Doc -> Text
render (Doc chunks) = T.intercalate "\n" (concatMap renderChunk chunks)

-- | A chunk's lines.
renderChunk :: Chunk -> [Text]
renderChunk (Chunk kept fresh body) =
  [ if lazy then line else foldMap laterForm kept <> foldMap (if i == 0 then firstForm else laterForm) fresh <> line
  | (i, (lazy, line)) <- zip [0 :: Int ..] (bodyLines body)
  ]

-- | A body's lines, each with whether it is lazy (written with no marks).
bodyLines :: Body -> [(Bool, Text)]
bodyLines = \case
  -- A line of attributes alone, which the block after it takes.
  Attrs -> [(False, "{#a .b}")]
  Text -> [(False, "Some text.")]
  TextLazy -> [(False, "Some text."), (True, "lazy text")]
  Heading -> [(False, "# A heading")]
  -- A box's marker, where its line is a quotation's first.
  Box -> [(False, "[!NOTE] A box")]
  Blank -> [(False, "")]
  Empty -> [(False, "")]
  Indented -> [(False, "    code")]
  Fenced -> [(False, "```"), (False, "- code"), (False, "```")]
  OpenFence -> [(False, "```"), (False, "> code")]
  Html -> [(False, "<div>"), (False, "- html"), (False, "</div>")]
  Table -> [(False, "| a | b |"), (False, "| - | - |")]
  -- The marker lines of one of the wiki's own blocks, which CodeMirror's
  -- Markdown does not know either.
  Opener -> [(False, "[[!notes]]")]
  Closer -> [(False, "[[/notes]]")]
  -- A start marker right after a paragraph's line.
  TextOpener -> [(False, "Some text."), (False, "[[!notes]]")]
  -- HTML of each of CommonMark's seven kinds, its end not written: the
  -- first five read on to their end marker, the last two to a blank line.
  RawHtml kind -> [(False, fromMaybe "<a>" (atMay ["<script>", "<!-- a comment", "<?php", "<!DOCTYPE html>", "<![CDATA[", "<div>", "<a>"] (kind - 1))), (False, "- html")]

-- | A container's marks on its first line.
firstForm :: Layer -> Text
firstForm = \case
  Quote b g -> T.replicate b " " <> ">" <> gapText g
  Item b m g t -> T.replicate b " " <> m <> gapText g <> maybe "" (\c -> "[" <> c <> "] ") t
  Note label -> "[^" <> show label <> "]: "

-- | A container's marks on a later line: a quotation's again, an item's
-- indentation.
laterForm :: Layer -> Text
laterForm = \case
  q@(Quote _ _) -> firstForm q
  Item b m g _ -> T.replicate (b + T.length m + width g) " "
  Note _ -> "    "
  where
    width = \case
      Spaces k | k >= 1 && k <= 4 -> k
      Spaces _ -> 1
      Tab -> 4

gapText :: Gap -> Text
gapText = \case
  Spaces k -> T.replicate k " "
  Tab -> "\t"

-- | The document split at the end of a line (from 1), as the text before
-- the cursor and after it.
cursorAfter :: Doc -> Int -> (Text, Text)
cursorAfter doc n =
  let ls = T.splitOn "\n" (render doc)
   in (T.intercalate "\n" (take n ls), mconcat ["\n" <> l | l <- drop n ls])

-- | Every line end of a text, as the text before the cursor and after it.
lineEnds :: Text -> [(Text, Text)]
lineEnds text =
  let ls = T.splitOn "\n" text
   in [(T.intercalate "\n" (take n ls), mconcat ["\n" <> l | l <- drop n ls]) | n <- [1 .. length ls]]

-- | A property at every line end of a document, with Enter's answer there.
atEveryLineEnd :: Doc -> (Text -> Text -> Enter -> Property) -> Property
atEveryLineEnd doc holds =
  counterexample (toS (render doc)) $
    conjoin
      [ counterexample (toS (before <> "‸" <> after)) (holds before after (enter before after))
      | (before, after) <- lineEnds (render doc)
      ]

----------------------------------------------------------------------
-- The parser's reading, as the properties check it

-- | The shapes covering a line, the outermost first: those whose lines,
-- or whose inner shapes' lines, run over it.
shapesAt :: [Shape] -> Int -> [Shape]
shapesAt shapes n = case lastMay (filter covers shapes) of
  Nothing -> []
  Just s -> s : shapesAt s.inner n
  where
    covers s = let (a, b) = linesOf s in a <= n && n <= b

-- | A shape's first and last line.
linesOf :: Shape -> (Int, Int)
linesOf s =
  let own = [(l, if c == 1 then l' - 1 else l') | ((l, _), (l', c)) <- s.parts]
      lows = map fst own <> map (fst . linesOf) s.inner
      highs = map snd own <> map (snd . linesOf) s.inner
   in (fromMaybe 0 (minimumMay lows), fromMaybe 0 (maximumMay highs))

-- | Whether the parser reads a line as code or HTML.
inCode :: Text -> Int -> Bool
inCode text n = case parseShapes text of
  Right shapes -> any (\s -> s.kind == CodeShape) (shapesAt shapes n)
  Left _ -> False

-- | The text after an answer.
textAfter :: Text -> Text -> Enter -> Text
textAfter before after answer = let (b, a) = written before after answer in b <> a

-- | Whether an answer's new line starts an item, for the parser.
newItemOn :: Text -> Text -> Enter -> Bool
newItemOn before after answer = case parseShapes (textAfter before after answer) of
  Right shapes -> any (startsItem (T.count "\n" before + 2)) (shapesAt shapes (T.count "\n" before + 2))
  Left _ -> False
  where
    startsItem n s = case s.kind of
      ItemShape _ _ -> fmap (fst . fst) (head s.parts) == Just n
      _ -> False

-- | Every tree made of these blocks with one empty item put in a list,
-- right after one of its items (a task's after a task).
withItemAdded :: [Block] -> [[Block]]
withItemAdded = \case
  [] -> []
  b : bs -> map (: bs) (inBlock b) <> map (b :) (withItemAdded bs)
  where
    inBlock = \case
      A.List kind items ->
        [ A.List kind (take (i + 1) items <> [A.ListItem (False <$ task) []] <> drop (i + 1) items)
        | (i, A.ListItem task _) <- zip [0 ..] items
        ]
          <> [ A.List kind (take i items <> [A.ListItem task inner'] <> drop (i + 1) items)
             | (i, A.ListItem task inner) <- zip [0 ..] items
             , inner' <- withItemAdded inner
             ]
      A.BlockQuote bs -> A.BlockQuote <$> withItemAdded bs
      A.Alert k h bs -> A.Alert k h <$> withItemAdded bs
      A.Div a bs -> A.Div a <$> withItemAdded bs
      A.Spoiler h bs -> A.Spoiler h <$> withItemAdded bs
      A.Footnotes notes -> [A.Footnotes (take i notes <> [A.Footnote n l bs'] <> drop (i + 1) notes) | (i, A.Footnote n l bs) <- zip [0 ..] notes, bs' <- withItemAdded bs]
      _ -> []

-- | The parser reads the text after the answer as the text before it with
-- one empty item added after an item. A paragraph and an item's plain text
-- count as the same: an empty item followed by a blank line does not make
-- its list loose, as the item's text, typed, will (commonmark 0.3.1), so
-- a loose list may read as tight until then.
itemAdded :: Text -> Text -> Enter -> Property
itemAdded before after answer = case (parseMarkdown (before <> after), parseMarkdown (textAfter before after answer)) of
  (Right old, Right new) -> counterexample (show new) (map plain new `elem` map (map plain) (withItemAdded old))
  _ -> counterexample "no parse" False

-- | A block with every paragraph in it as an item's plain text.
plain :: Block -> Block
plain = \case
  A.Para xs -> A.Plain xs
  A.List kind items -> A.List kind [A.ListItem task (map plain bs) | A.ListItem task bs <- items]
  A.BlockQuote bs -> A.BlockQuote (map plain bs)
  A.Alert k h bs -> A.Alert k h (map plain bs)
  A.Div a bs -> A.Div a (map plain bs)
  A.Spoiler h bs -> A.Spoiler h (map plain bs)
  A.Footnotes notes -> A.Footnotes [A.Footnote n l (map plain bs) | A.Footnote n l bs <- notes]
  other -> other

-- | The quotations and items a line is in, for the parser, each with the
-- line it starts on. On a blank line, or one in no block of text or code,
-- only marks,
-- the items at the end that do not start on it are left out: such a line
-- is a blank line of those items, no item's own.
containersAt :: Text -> Int -> Maybe [(Bool, Int)]
containersAt text n = do
  shapes <- rightToMaybe (parseShapes text)
  let found = shapesAt shapes n
      held = [(isQuote s, fst (linesOf s)) | s <- found, isQuote s || isItem s]
      blank = maybe True (T.null . T.strip) (atMay (T.splitOn "\n" text) (n - 1))
      marksOnly = blank || maybe True (\s -> isQuote s || isItem s || s.kind == ListShape) (lastMay found)
  pure (if marksOnly then reverse (dropWhile (\(q, l) -> not q && l /= n) (reverse held)) else held)
  where
    isQuote s = s.kind == QuoteShape
    isItem s = case s.kind of
      ItemShape _ _ -> True
      _ -> False

-- | The new quoted line is in the same quotations and items as the
-- cursor's line, and the blocks of the text before the cursor's top-level
-- block are read as before.
quotedLineKept :: Text -> Text -> Enter -> Property
quotedLineKept before after answer =
  let n = T.count "\n" before + 1
      text = textAfter before after answer
   in counterexample (show (containersAt (before <> after) n, containersAt text (n + 1))) $
        (map fst <$> containersAt text (n + 1)) === (map fst <$> containersAt (before <> after) n)

-- | The cursor's line, after the answer, is one container less deep, or
-- an item of the list around (which counts as deep as the item it is
-- next to); and it holds nothing after its marks.
wentUp :: Text -> Text -> Enter -> Property
wentUp before after answer =
  let n = T.count "\n" before + 1
      text = textAfter before after answer
      was = fromMaybe [] (containersAt (before <> after) n)
      now = fromMaybe [] (containersAt text n)
   in counterexample (show (was, now)) $
        conjoin
          [ counterexample "not one level up" (length now == length was - 1)
          , counterexample "text on the line" (not (inTextAt text n))
          ]

-- | The top-level blocks before the cursor's one, and after it, are the
-- same after the answer as before it. Going up may split the cursor's
-- block in two (a quotation left in its middle), so the blocks after it
-- are compared from the end. The footnotes, which the parser gathers at
-- the end of the page, are left out: the cursor may be in one.
blocksAroundKept :: Text -> Text -> Enter -> Property
blocksAroundKept before after answer = case (placed (before <> after), placed (textAfter before after answer)) of
  (Just old, Just new) ->
    let n = T.count "\n" before + 1
        own = fromMaybe n (lastMay [a | (a, b, _) <- old, a <= n, n <= b])
        above = [blk | (a, _, blk) <- old, a < own]
        below = [blk | (a, _, blk) <- old, a > n]
        blocks = [blk | (_, _, blk) <- new]
     in counterexample (show blocks) $
          take (length above) blocks === above
            .&&. reverse (take (length below) (reverse blocks)) === below
  _ -> counterexample "no parse" False
  where
    placed t = either (const Nothing) (\bs -> Just [(a, b, blk) | (Just (a, b), blk) <- bs, not (isFootnotes blk)]) (parseWithSpans t)
    isFootnotes = \case
      A.Footnotes _ -> True
      _ -> False

-- | The blocks that took attributes are the same after the answer, each
-- by the line it starts on (a new line counted out).
attributesKept :: Text -> Text -> Enter -> Property
attributesKept before after answer = case (parseShapes (before <> after), parseShapes (textAfter before after answer)) of
  (Right old, Right new) ->
    let n = T.count "\n" before + 2
        back l = case answer of
          NewLineWith _ | l == n -> 0
          NewLineWith _ | l > n -> l - 1
          _ -> l
     in sort [(back l, a) | (l, a) <- took new] === took old
  _ -> counterexample "no parse" False
  where
    took = sort . concatMap go
    go s = [(fst (linesOf s), s.attributes) | not (null s.attributes)] <> concatMap go s.inner

-- | Whether a line is in a paragraph, an item's text or a heading.
inTextAt :: Text -> Int -> Bool
inTextAt text n = either (const False) (\shapes -> any (\s -> s.kind == TextShape) (shapesAt shapes n)) (parseShapes text)

-- | A change since the editor's last check: at this per mille of the
-- text now (moved to the start of its line, or not), so many characters
-- of the text now stood in place of this text then. A deletion of up to
-- forty characters spans several starts; one at a line's start puts text
-- in at a start; one across lines replaces lines.
data Change = Change Int Bool Int Text
  deriving stock (Show)

instance Arbitrary Change where
  arbitrary =
    Change
      <$> choose (0, 1000)
      <*> elements [False, True]
      <*> frequency [(2, pure 0), (3, choose (1, 6)), (2, choose (7, 40))]
      <*> elements ["", "x", "\n", "\n\n", "- ", "> ", "    ", "```\n", "<div>\n", "<!--\n", "[[!notes]]\n", "[[/notes]]\n", "[^9]: x\n", "\n\nx\n\n"]

-- | No change since the last check.
unchanged :: Change
unchanged = Change 0 False 0 ""

-- | The text the editor hands to 'enter' for a cursor at the end of a line
-- of the text now, when the last check was of the text before a change:
-- the starts that check kept ('blockStarts', as offsets), mapped through
-- the change as CodeMirror maps a position after what is put in (several
-- onto one place, into a line), each with its line now, cut by
-- 'textRange'. Nothing past the limit.
handedOver :: Text -> Text -> Change -> Maybe (Text, Text)
handedOver before after (Change per toLine removed was) = handedOverAt before after at removed was
  where
    now = before <> after
    near = T.length now * per `div` 1000
    at = if toLine then T.length (T.dropWhileEnd (/= '\n') (T.take near now)) else near

-- | 'handedOver' for a change at an offset of the text now.
handedOverAt :: Text -> Text -> Int -> Int -> Text -> Maybe (Text, Text)
handedOverAt before after at removed was = do
  (begins, ends) <- textRange kept firstChange lineStart lineEnd (T.length now)
  pure (T.take (cursor - begins) (T.drop begins now), T.take (ends - cursor) (T.drop cursor now))
  where
    now = before <> after
    taken = min removed (T.length now - at)
    old = T.take at now <> was <> T.drop (at + taken) now
    changed = taken > 0 || not (T.null was)
    oldLines = T.splitOn "\n" old
    oldStarts = [sum (map ((+ 1) . T.length) (take (l - 1) oldLines)) | l <- either (const [1]) (blockStarts old) (parseShapes old)]
    mapped s
      | not changed || s < at = s
      | s > at + T.length was = s + taken - T.length was
      | otherwise = at + taken
    kept = [Kept {at = m, lineFrom = lineOf m, lineTo = m + T.length (T.takeWhile (/= '\n') (T.drop m now))} | s <- oldStarts, let m = mapped s]
    lineOf m = T.length (T.dropWhileEnd (/= '\n') (T.take m now))
    firstChange = if changed then at else T.length now
    cursor = T.length before
    lineStart = T.length (T.dropWhileEnd (/= '\n') before)
    lineEnd = cursor + T.length (T.takeWhile (/= '\n') after)

-- | What the editor's Enter answers: 'enter' over what it hands over, or
-- a line break alone past the limit.
editorAnswer :: Text -> Text -> Change -> Enter
editorAnswer before after change = maybe NewLine (uncurry enter) (handedOver before after change)

-- | An answer, forced, and the bytes allocated to make it.
allocations :: Enter -> IO (Enter, Int64)
allocations answer = do
  start <- getAllocationCounter
  forced <- evaluate (answer `seq` T.length (show answer :: Text) `seq` answer)
  end <- getAllocationCounter
  pure (forced, start - end)

-- | A presentation of sixty slides of a hundred lines each, as the browser
-- suite's long deck: paragraphs, a list of ten items, a fence, a formula.
longDeck :: Text
longDeck = T.intercalate "\n" (["# Long", ""] <> concatMap slide [1 .. 60 :: Int])
  where
    slide n =
      take 100 $
        ["## Slide " <> show n, ""]
          <> para n 12
          <> [""]
          <> para n 11
          <> [""]
          <> ["- Item " <> show k <> " of slide " <> show n <> "." | k <- [1 .. 10 :: Int]]
          <> ["", "```haskell"]
          <> ["step" <> show i <> " x = x * " <> show n | i <- [0 .. 9 :: Int]]
          <> ["```", ""]
          <> concat [para n 9 <> [""] | _ <- [1 .. 6 :: Int]]
          <> repeat ""
    para :: Int -> Int -> [Text]
    para n k = ["A sentence of slide " <> show n <> ", line " <> show i <> "." | i <- [1 .. k]]

----------------------------------------------------------------------
-- Lists and quotations written plainly

-- | A document ending in a line of text in containers written as
-- CommonMark reads them without doubt: no spaces before a mark, one after
-- it, no tab, numbers far from their limit; the cursor at its end.
newtype Plain = Plain Doc
  deriving stock (Show)

instance Arbitrary Plain where
  arbitrary = do
    stack <- plainLayers
    k <- choose (0, length stack - 1)
    let outer = take k stack
        inner = drop k stack
    above <- elements [[], [Chunk [] stack Text]]
    pure (Plain (Doc (above <> [Chunk (if null above then [] else outer) (if null above then stack else inner) Text])))
    where
      -- A box only on the innermost item; numbers that may start a list
      -- after a paragraph's line.
      plainLayers = do
        ls <- listOf1 plainLayer
        box <- elements [Nothing, Just " ", Just "x"]
        pure [if i == length ls - 1 then withBox box l else l | (i, l) <- zip [0 :: Int ..] ls]
      plainLayer = oneof [pure (Quote 0 (Spaces 1)), Item 0 <$> elements ["-", "*", "1.", "1)"] <*> pure (Spaces 1) <*> pure Nothing]
      withBox box = \case
        Item b m g _ -> Item b m g box
        q -> q

-- | What Enter at the end of such a document writes: the next item of its
-- innermost item, or the next line of its innermost quotation, after the
-- marks of the containers around.
plainExpected :: Doc -> Enter
plainExpected (Doc chunks) = case lastMay chunks of
  Just (Chunk kept fresh _) ->
    let stack = kept <> fresh
     in NewLineWith $ case lastMay stack of
          -- The parser reads no box after a number: a numbered item's
          -- next item has none.
          Just (Item b m g t) -> foldMap laterForm (initSafe stack) <> firstForm (Item b (nextMark m) g (if isBullet m then " " <$ t else Nothing))
          _ -> foldMap laterForm stack
  Nothing -> NewLine
  where
    isBullet m = m `elem` ["-", "*", "+"]
