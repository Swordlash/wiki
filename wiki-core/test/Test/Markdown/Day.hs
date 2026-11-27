-- | A day's entries read from a list ('dayEntries'), the parts of the day
-- ('dayPart') and the date in a page's title ('dayOfTitle'). The lists are
-- parsed by the real parser, so these tests pin what it gives for a
-- mark and a time too. They run on both backends.
module Test.Markdown.Day (spec) where

import Data.Text qualified as T
import Data.Time (TimeOfDay (..), fromGregorian)
import Protolude
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck hiding (label)
import Wiki.Core.Markdown.Ast
import Wiki.Core.Markdown.Day
import Wiki.Core.Markdown.Tasks (taskLabel)

-- | The entries of a text that is lists only, which it must be, read as
-- one list. The parser cuts a list in two where an item with a task box
-- and an item without one meet ('splitsAtBoxes'), so a day's list with
-- tasks in it is several lists in the tree, one after another.
entriesOf :: Text -> [DayEntry]
entriesOf source = either (panic . show) dayEntries (itemsOf source)

-- | The items of a text that is lists only, in the order of the source.
itemsOf :: Text -> Either Text [ListItem]
itemsOf source = case parseMarkdown source of
  Right blocks@(_ : _) | Just items <- traverse listItems blocks -> Right (concat items)
  other -> Left ("not lists only: " <> show other)
  where
    listItems = \case
      List _ items -> Just items
      _ -> Nothing

-- | What the parser gives for a list of items with and without a task
-- box: a list for each run of items alike.
splitsAtBoxes :: Either ParseError [Block]
splitsAtBoxes = parseMarkdown "- a\n- [ ] b\n- [x] c\n- d\n"

-- | The entry of a text that is a list of one item.
item :: Text -> DayEntry
item source = case entriesOf source of
  [e] -> e
  other -> panic ("not one entry: " <> show other)

-- | An entry without a box at the first place of its list.
entry :: Bool -> EntryTime -> Text -> DayEntry
entry highlight time label = DayEntry {place = 0, task = Nothing, highlight, time, label}

-- | A time of day, on the minute.
at :: Int -> Int -> TimeOfDay
at h m = TimeOfDay h m 0

-- | The item, an entry for the whole day labelled with its text as
-- written, not highlighted.
allDay :: Text -> DayEntry
allDay = entry False AllDay

spec :: Spec
spec = describe "Wiki.Core.Markdown.Day" $ do
  describe "dayEntries" $ do
    it "reads each line of the module's example" $ do
      entriesOf
        ( T.unlines
            [ "- 09:00 Standup"
            , "- 9:30-10:30 Review of the plan"
            , "- ! 14:00 Demo for the client"
            , "- ! Ania's birthday"
            , "- [ ] 16:00 Call the bank"
            , "- [x] ! Pay the invoice"
            , "- Buy milk"
            ]
        )
        `shouldBe` [ DayEntry {place = 0, task = Nothing, highlight = False, time = At (at 9 0), label = "Standup"}
                   , DayEntry {place = 1, task = Nothing, highlight = False, time = Span (at 9 30) (at 10 30), label = "Review of the plan"}
                   , DayEntry {place = 2, task = Nothing, highlight = True, time = At (at 14 0), label = "Demo for the client"}
                   , DayEntry {place = 3, task = Nothing, highlight = True, time = AllDay, label = "Ania's birthday"}
                   , DayEntry {place = 4, task = Just False, highlight = False, time = At (at 16 0), label = "Call the bank"}
                   , DayEntry {place = 5, task = Just True, highlight = True, time = AllDay, label = "Pay the invoice"}
                   , DayEntry {place = 6, task = Nothing, highlight = False, time = AllDay, label = "Buy milk"}
                   ]

    it "is given the items of several lists where the parser cuts one at its tasks" $
      -- The renderer and the next packages read this tree: a day's list
      -- with tasks in it is several lists there, and its caller reads
      -- them as one.
      splitsAtBoxes
        `shouldBe` Right
          [ List BulletList [ListItem Nothing [Plain [Str "a"]]]
          , List BulletList [ListItem (Just False) [Plain [Str "b"]], ListItem (Just True) [Plain [Str "c"]]]
          , List BulletList [ListItem Nothing [Plain [Str "d"]]]
          ]

    it "reads a time that is all of the item" $ do
      item "- 09:00" `shouldBe` entry False (At (at 9 0)) ""
      item "- 9:30-10:30" `shouldBe` entry False (Span (at 9 30) (at 10 30)) ""
      item "- ! 09:00" `shouldBe` entry True (At (at 9 0)) ""

    it "reads the earliest and the latest times" $ do
      item "- 0:00 a" `shouldBe` entry False (At (at 0 0)) "a"
      item "- 23:59 a" `shouldBe` entry False (At (at 23 59)) "a"
      item "- 00:00-23:59 a" `shouldBe` entry False (Span (at 0 0) (at 23 59)) "a"

    it "reads a span with leading zeros, which the parser leaves as written" $ do
      -- No smart punctuation, and ":00-10:" is no emoji's name.
      parseMarkdown "- 09:00-10:30 Review" `shouldBe` Right [List BulletList [ListItem Nothing [Plain [Str "09:00-10:30 Review"]]]]
      item "- 09:00-10:30 Review" `shouldBe` entry False (Span (at 9 0) (at 10 30)) "Review"

    it "reads a span with an en dash, which the parser leaves as written" $ do
      parseMarkdown "- 09:00\x2013\&10:30 Review" `shouldBe` Right [List BulletList [ListItem Nothing [Plain [Str "09:00\x2013\&10:30 Review"]]]]
      item "- 09:00\x2013\&10:30 Review" `shouldBe` entry False (Span (at 9 0) (at 10 30)) "Review"

    it "reads the mark and the time from one text inline, as the parser gives them" $
      parseMarkdown "- ! 09:00 Demo" `shouldBe` Right [List BulletList [ListItem Nothing [Plain [Str "! 09:00 Demo"]]]]

    it "skips the spaces between the time and the label, and after the mark" $ do
      item "- 09:00   Demo" `shouldBe` entry False (At (at 9 0)) "Demo"
      item "- !   09:00 Demo" `shouldBe` entry True (At (at 9 0)) "Demo"

    describe "reads no time, and keeps the text as written, for" $ do
      let cases =
            [ "25:00 Demo"
            , "24:00 Demo"
            , "9:5 Demo"
            , "9:60 Demo"
            , "009:00 Demo"
            , "10:30-9:30 Demo"
            , "10:30-10:30 Demo"
            , "09:00- Demo"
            , "-09:00 Demo"
            , "9h Demo"
            , "9 Demo"
            , "10:00:00 Demo"
            , "9:30-10:30-11:00 Demo"
            , "09:00--10:30 Demo"
            , "09:00\x2014\&10:30 Demo"
            , "09:00.Demo"
            , "09:00,Demo"
            ]
      for_ cases $ \written ->
        it (toS written) $ item ("- " <> written) `shouldBe` allDay written

    it "reads no time before a tab, or before a line break" $ do
      item "- 09:00\tDemo" `shouldBe` allDay "09:00\tDemo"
      item "- 09:00\n  Demo" `shouldBe` allDay "09:00 Demo"

    it "reads no time directly before an emoji or code" $ do
      item "- 10:00:smile: Demo" `shouldBe` allDay "10:00\128516 Demo"
      item "- 09:00-10:30:tada: Demo" `shouldBe` allDay "09:00-10:30\127881 Demo"
      item "- 09:00`x` Demo" `shouldBe` allDay "09:00x Demo"

    it "reads a start, and leaves the rest in the label, for a span with spaces" $
      item "- 09:00 - 10:30 Review" `shouldBe` entry False (At (at 9 0)) "- 10:30 Review"

    it "keeps the label's own markup out of the label's text" $
      item "- 09:00 *Demo* with @ania" `shouldBe` entry False (At (at 9 0)) "Demo with @ania"

    describe "the order: box, mark, time" $ do
      it "reads a mark after a time as the label's" $
        item "- 09:00 ! Demo" `shouldBe` entry False (At (at 9 0)) "! Demo"
      it "reads no mark followed directly by text" $ do
        item "- !09:00 Demo" `shouldBe` allDay "!09:00 Demo"
        item "- !09:00" `shouldBe` allDay "!09:00"
        item "- !x" `shouldBe` allDay "!x"
      it "reads no mark followed directly by code, emphasis or an emoji" $ do
        item "- !`x`" `shouldBe` allDay "!x"
        item "- !*x*" `shouldBe` allDay "!x"
        item "- !:smile:" `shouldBe` allDay "!\128516"
      it "reads a ! that is all of the item as a mark, with an empty label" $ do
        -- The parser drops the space at the end, so both are the same tree.
        parseMarkdown "- ! " `shouldBe` parseMarkdown "- !"
        item "- !" `shouldBe` entry True AllDay ""
        item "- [ ] !" `shouldBe` (entry True AllDay "") {task = Just False}
      it "reads one mark only" $
        item "- ! ! Demo" `shouldBe` entry True AllDay "! Demo"
      it "reads the mark and the time after a box" $
        item "- [ ] ! 09:00 Demo" `shouldBe` (entry True (At (at 9 0)) "Demo") {task = Just False}
      it "reads no box after the mark" $
        item "- ! [ ] Demo" `shouldBe` entry True AllDay "[ ] Demo"
      it "reads a mark before a time that is no time" $
        item "- ! 25:00 Demo" `shouldBe` entry True AllDay "25:00 Demo"

    describe "reads no mark or time that is not plain text at the start" $ do
      it "in inline code" $ do
        item "- `09:00` Demo" `shouldBe` allDay "09:00 Demo"
        item "- `!` Demo" `shouldBe` allDay "! Demo"
        item "- ! `09:00` Demo" `shouldBe` entry True AllDay "09:00 Demo"
      it "in emphasis" $ do
        item "- *09:00* Demo" `shouldBe` allDay "09:00 Demo"
        item "- *! 09:00 Demo*" `shouldBe` allDay "! 09:00 Demo"
        item "- **!** Demo" `shouldBe` allDay "! Demo"
      it "in a link's text" $ do
        item "- [09:00](x) Demo" `shouldBe` allDay "09:00 Demo"
        item "- [! 09:00 Demo](x)" `shouldBe` allDay "! 09:00 Demo"
      it "in an image's text" $
        item "- ![09:00](a.png) Demo" `shouldBe` allDay "09:00 Demo"

    it "reads an escaped mark as a mark: the parser gives the same text" $ do
      parseMarkdown "- \\! 09:00 Demo" `shouldBe` parseMarkdown "- ! 09:00 Demo"
      item "- \\! 09:00 Demo" `shouldBe` entry True (At (at 9 0)) "Demo"

    describe "reads the text before display math as all of the first paragraph" $ do
      -- The parser makes display math a block of its own, so the item's
      -- first paragraph ends where the math starts.
      it "as the parser gives it" $ do
        parseMarkdown "- 09:00$$x$$" `shouldBe` Right [List BulletList [ListItem Nothing [Plain [Str "09:00"], MathBlock "x"]]]
        parseMarkdown "- 09:00 $$x$$" `shouldBe` parseMarkdown "- 09:00$$x$$"
        parseMarkdown "- ! $$x$$" `shouldBe` Right [List BulletList [ListItem Nothing [Plain [Str "!"], MathBlock "x"]]]
      it "a time" $ do
        item "- 09:00$$x$$" `shouldBe` entry False (At (at 9 0)) ""
        item "- 09:00 $$x$$" `shouldBe` entry False (At (at 9 0)) ""
      it "a mark" $ do
        parseMarkdown "- !$$x$$" `shouldBe` parseMarkdown "- ! $$x$$"
        item "- ! $$x$$" `shouldBe` entry True AllDay ""
        item "- !$$x$$" `shouldBe` entry True AllDay ""

    it "reads the items of a nested list as part of their item" $
      entriesOf "- 09:00 Standup\n  - 10:00 not an entry\n  - ! nor this\n- Buy milk\n"
        `shouldBe` [ entry False (At (at 9 0)) "Standup"
                   , (allDay "Buy milk") {place = 1}
                   ]

    it "reads the first paragraph of a loose item only" $
      entriesOf "- 09:00 Standup\n\n  More about it.\n\n- 10:00 Review\n"
        `shouldBe` [entry False (At (at 9 0)) "Standup", (entry False (At (at 10 0)) "Review") {place = 1}]

    it "reads an item whose first block is not a paragraph as a whole-day entry without a label" $ do
      entriesOf "- ```\n  09:00 code\n  ```\n- 10:00 Review\n"
        `shouldBe` [allDay "", (entry False (At (at 10 0)) "Review") {place = 1}]
      item "- - 09:00 nested" `shouldBe` allDay ""

    it "labels an item without a mark or a time as a task is labelled" $
      for_ ["- Buy milk", "- *Buy* `milk` [now](x)", "- 25:00 x", "- !09:00", "- ```\n  x\n  ```"] $ \source ->
        case parseMarkdown source of
          Right [List _ [ListItem _ blocks]] -> (item source).label `shouldBe` taskLabel blocks
          other -> expectationFailure ("not one item: " <> show other)

    it "numbers the items of an ordered list from 0" $
      map (\e -> (e.place, e.time)) (entriesOf "3. 09:00 a\n4. b\n")
        `shouldBe` [(0, At (at 9 0)), (1, AllDay)]

    prop "gives back the entries a list was written from" $
      forAll genList $ \(bullet, written) ->
        let source = T.unlines [bullet <> " " <> line | (_, line) <- written]
            expected = zipWith (\i (e, _) -> e {place = i}) [0 ..] written
         in counterexample (toS source) $ case itemsOf source of
              Right items -> dayEntries items === expected
              Left why -> counterexample (toS why) False

  describe "dayPart" $ do
    it "puts each time at a boundary in its part"
      $ for_
        [ (at 0 0, Night)
        , (at 5 59, Night)
        , (at 6 0, Dawn)
        , (at 8 59, Dawn)
        , (at 9 0, Morning)
        , (at 11 59, Morning)
        , (at 12 0, Noon)
        , (at 14 59, Noon)
        , (at 15 0, Afternoon)
        , (at 17 59, Afternoon)
        , (at 18 0, Evening)
        , (at 20 59, Evening)
        , (at 21 0, Night)
        , (at 23 59, Night)
        ]
      $ \(time, part) -> (time, dayPart time) `shouldBe` (time, part)
    it "puts an entry in the part of its start, and a whole-day entry in none" $ do
      entryPart (item "- 20:30-22:00 Dinner") `shouldBe` Just Evening
      entryPart (item "- 5:00 Run") `shouldBe` Just Night
      entryPart (item "- Buy milk") `shouldBe` Nothing

  describe "dayOfTitle" $ do
    it "reads a title that is a date" $ do
      dayOfTitle "2026-11-27" `shouldBe` Just (fromGregorian 2026 11 27)
      dayOfTitle "  2026-11-27 " `shouldBe` Just (fromGregorian 2026 11 27)
      dayOfTitle "2024-02-29" `shouldBe` Just (fromGregorian 2024 2 29)
    it "reads no date that does not exist" $ do
      dayOfTitle "2026-02-30" `shouldBe` Nothing
      dayOfTitle "2026-13-01" `shouldBe` Nothing
      dayOfTitle "2025-02-29" `shouldBe` Nothing
    it "reads no date from a title with more in it, or written otherwise" $
      for_ ["2026-11-27 Standup", "Day 2026-11-27", "2026-11-27T10:00", "2026-2-3", "20261127", "-0001-11-27", "+2026-11-27", "12026-11-27", "27.11.2026", ""] $ \title ->
        (title, dayOfTitle title) `shouldBe` (title, Nothing)

-- | A list of entries, each with the line it is written as, and the
-- list's bullet: one for the whole list, since another bullet starts
-- another list. No numbered list: the parser reads no task box in one.
genList :: Gen (Text, [(DayEntry, Text)])
genList = (,) <$> elements ["-", "*", "+"] <*> resize 8 (listOf1 genWritten)

-- | An entry and its line, without the bullet. The label is up to three
-- words of ASCII letters, which can be read as no mark, no time and no
-- other markup. It is empty only when a mark or a time is written: a mark
-- or a time can be all of the item, but a box alone is never written.
genWritten :: Gen (DayEntry, Text)
genWritten = do
  task <- elements [Nothing, Just False, Just True]
  highlight <- arbitrary
  (time, timeText) <- genTime
  let word = T.pack <$> resize 8 (listOf1 (elements (['a' .. 'z'] <> ['A' .. 'Z'])))
      labelWords = if highlight || time /= AllDay then listOf word else listOf1 word
  label <- T.unwords <$> resize 3 labelWords
  let box = case task of
        Nothing -> []
        Just False -> ["[ ]"]
        Just True -> ["[x]"]
      line = T.unwords (box <> ["!" | highlight] <> [timeText | not (T.null timeText)] <> [label])
  pure (DayEntry {place = 0, task, highlight, time, label}, line)

-- | A time and how it is written: none, a start, or a span with a hyphen
-- or an en dash. An hour below ten is written with a leading zero or
-- without.
genTime :: Gen (EntryTime, Text)
genTime =
  oneof
    [ pure (AllDay, "")
    , (\(t, w) -> (At t, w)) <$> genClock
    , do
        (a, aw) <- genClock
        (b, bw) <- genClock `suchThat` ((/= a) . fst)
        dash <- elements ["-", "\x2013"]
        pure (if a < b then (Span a b, aw <> dash <> bw) else (Span b a, bw <> dash <> aw))
    ]

genClock :: Gen (TimeOfDay, Text)
genClock = do
  h <- choose (0, 23)
  m <- choose (0, 59)
  padded <- arbitrary
  let hours = if h < 10 && padded then "0" <> show h else show h
      minutes = (if m < 10 then "0" else "") <> show m
  pure (at h m, hours <> ":" <> minutes)
