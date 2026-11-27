-- | A page's day lists ('joinDayLists', 'dayListOf', 'pageDayEntries'),
-- an item without its mark ('unmarked'), and the groups the page view
-- shows a day's entries in ('dayGroups'). The pages are parsed by the
-- real parser. These run on both backends.
module Test.Markdown.DayList (spec) where

import Data.Time (TimeOfDay (..))
import Protolude
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck hiding (label)
import Wiki.Core.Markdown.Ast
import Wiki.Core.Markdown.Day

-- | A text's blocks, which must parse.
blocksOf :: Text -> [Block]
blocksOf = either (panic . show) identity . parseMarkdown

-- | How 'joinDayLists' cuts a text's blocks: how many blocks each run has.
runSizes :: Text -> [Int]
runSizes = map (length . snd) . joinDayLists . map ((),) . blocksOf

-- | The day lists of a text's top level: each run's items' first
-- paragraph, as the parser gives it.
dayListsIn :: Text -> [[Text]]
dayListsIn source =
  [ [plainText xs | ListItem _ (Plain xs : _) <- items]
  | ((), run) <- joinDayLists (map ((),) (blocksOf source))
  , Just (_, items) <- [dayListOf run]
  ]

at :: Int -> Int -> TimeOfDay
at h m = TimeOfDay h m 0

-- | An entry without a box or a highlight, at a place.
entryAt :: Int -> EntryTime -> DayEntry
entryAt place time = DayEntry {place, task = Nothing, highlight = False, time, label = ""}

-- | The groups as their names and their entries' places.
shown :: [DayEntry] -> [(Text, [Int])]
shown = map (\(g, es) -> (dayGroupLabel g, map (\e -> e.place) es)) . dayGroups

spec :: Spec
spec = describe "Wiki.Core.Markdown.Day's lists" $ do
  describe "joinDayLists" $ do
    let written = "{.day}\n- 09:00 Standup\n- [ ] 16:00 Call the bank\n- Buy milk\n"
    it "joins the lists the parser cut a {.day} list into" $ do
      runSizes written `shouldBe` [3]
      dayListsIn written `shouldBe` [["09:00 Standup", "16:00 Call the bank", "Buy milk"]]

    it "joins them with blank lines between the items too" $
      dayListsIn "{.day}\n- a\n\n- [ ] b\n\n- c\n" `shouldBe` [["a", "b", "c"]]

    it "keeps what the first block of a run carries" $
      map fst (joinDayLists (zip [1 :: Int ..] (blocksOf ("Before.\n\n" <> written <> "\nAfter.\n"))))
        `shouldBe` [1, 2, 5]

    it "takes in no list after a paragraph" $
      runSizes "{.day}\n- a\n\nText.\n\n- [ ] b\n" `shouldBe` [1, 1, 1]

    it "takes in no list the parser did not cut: another bullet, or another kind" $ do
      runSizes "{.day}\n- a\n* b\n" `shouldBe` [1, 1]
      runSizes "{.day}\n- [ ] a\n1. b\n" `shouldBe` [1, 1]
      runSizes "{.day}\n1. a\n- [ ] b\n" `shouldBe` [1, 1]

    it "joins no list after an ordinary list, or after a sorted one" $ do
      runSizes "- a\n- [ ] b\n" `shouldBe` [1, 1]
      runSizes "{.sorted}\n- a\n- [ ] b\n" `shouldBe` [1, 1]

    it "reads a [[!div]]{.day} that holds the lists as one day list, and takes in nothing after it" $ do
      let boxed = "[[!div]]{.day}\n- 09:00 Standup\n- [ ] 16:00 Call the bank\n- Buy milk\n[[/div]]\n- [ ] after\n"
      runSizes boxed `shouldBe` [1, 1]
      dayListsIn boxed `shouldBe` [["09:00 Standup", "16:00 Call the bank", "Buy milk"]]

    it "takes in a list right after a [[!div]]{.day} of one list, when its box differs: the tree cannot tell it from {.day}" $
      dayListsIn "[[!div]]{.day}\n- a\n[[/div]]\n- [ ] b\n" `shouldBe` [["a", "b"]]

    it "reads no day list from a [[!div]]{.day} that holds more than lists" $ do
      dayListsIn "[[!div]]{.day}\nText.\n\n- 09:00 a\n[[/div]]\n" `shouldBe` []
      dayListsIn "[[!div]]{.day}\n[[/div]]\n" `shouldBe` []

    it "joins day lists in a quotation and in an item, as at the top" $ do
      pageDayEntries (blocksOf "> {.day}\n> - 09:00 a\n> - [ ] 10:00 b\n")
        `shouldBe` [ DayEntry {place = 0, task = Nothing, highlight = False, time = At (at 9 0), label = "a"}
                   , DayEntry {place = 1, task = Just False, highlight = False, time = At (at 10 0), label = "b"}
                   ]
      map (\e -> e.label) (pageDayEntries (blocksOf "- Plan\n\n  {.day}\n  - 09:00 a\n  - [ ] b\n")) `shouldBe` ["a", "b"]

  describe "pageDayEntries" $ do
    it "reads every day list of a page, and no other list" $
      map (\e -> (e.place, e.label)) (pageDayEntries (blocksOf "- 08:00 not a day\n\n{.day}\n- 09:00 a\n- [ ] b\n\nText.\n\n> [!NOTE]\n> {.day}\n> - c\n"))
        `shouldBe` [(0, "a"), (1, "b"), (0, "c")]
    it "reads a day list nested in a day list's item after the item's own" $
      map (\e -> e.label) (pageDayEntries (blocksOf "{.day}\n- a\n\n  {.day}\n  - 09:00 inner\n- b\n")) `shouldBe` ["a", "b", "inner"]

  describe "unmarked" $ do
    let firstItem source = case blocksOf source of
          [List _ (item : _)] -> item
          other -> panic (show other)
    it "takes the mark off a highlighted item's text, and nothing else" $ do
      unmarked (firstItem "- ! 09:00 Demo") `shouldBe` ListItem Nothing [Plain [Str "09:00 Demo"]]
      unmarked (firstItem "- [ ] ! Call *now*\n  - nested") `shouldBe` firstItem "- [ ] Call *now*\n  - nested"
      unmarked (firstItem "- ! *Demo*") `shouldBe` ListItem Nothing [Plain [Emph [Str "Demo"]]]
    it "takes off a mark that is all of the item's text, before display math too" $ do
      unmarked (firstItem "- !") `shouldBe` ListItem Nothing [Plain []]
      unmarked (firstItem "- ! $$x$$") `shouldBe` ListItem Nothing [Plain [], MathBlock "x"]
    it "leaves an item without a mark as it is" $
      for_ ["- 09:00 ! Demo", "- !09:00", "- `!` Demo", "- Buy milk", "- ```\n  ! code\n  ```"] $ \source ->
        unmarked (firstItem source) `shouldBe` firstItem source

  describe "dayGroups" $ do
    it "shows the whole day first, then the parts of the day in the order of the clock" $
      shown
        [ entryAt 0 (At (at 14 0))
        , entryAt 1 AllDay
        , entryAt 2 (At (at 23 30))
        , entryAt 3 (Span (at 9 30) (at 10 30))
        , entryAt 4 (At (at 5 0))
        , entryAt 5 AllDay
        , entryAt 6 (At (at 6 0))
        , entryAt 7 (At (at 15 0))
        , entryAt 8 (At (at 19 0))
        ]
        `shouldBe` [ ("All day", [1, 5])
                   , ("Night", [4])
                   , ("Dawn", [6])
                   , ("Morning", [3])
                   , ("Noon", [0])
                   , ("Afternoon", [7])
                   , ("Evening", [8])
                   , ("Night", [2])
                   ]

    it "keeps the night before the day and the night after it apart, with nothing between them" $ do
      let entries = [entryAt 0 (At (at 22 0)), entryAt 1 (At (at 2 0))]
      shown entries `shouldBe` [("Night", [1]), ("Night", [0])]
      map fst (dayGroups entries) `shouldBe` [During Night, LateNight]

    it "orders a part by start, equal starts in the order of the source" $
      shown [entryAt 0 (At (at 10 30)), entryAt 1 (Span (at 9 0) (at 11 0)), entryAt 2 (At (at 9 0)), entryAt 3 (At (at 9 15))]
        `shouldBe` [("Morning", [1, 2, 3, 0])]

    it "shows no part without an entry, and nothing for no entries" $ do
      shown [entryAt 0 (At (at 12 0))] `shouldBe` [("Noon", [0])]
      shown [entryAt 0 AllDay] `shouldBe` [("All day", [0])]
      shown [] `shouldBe` []

    it "groups the template's day" $
      shown (pageDayEntries (blocksOf "# 2026-11-27\n\n{.day}\n- ! 09:00 Standup\n- 10:00-11:30 Review of the plan\n- [ ] 15:00 Call the bank\n- Buy milk\n"))
        `shouldBe` [("All day", [3]), ("Morning", [0, 1]), ("Afternoon", [2])]

    prop "puts every entry in one group, in the clock's order, equal starts as in the source" $
      forAll genEntries $ \entries ->
        let groups = dayGroups entries
            flat = concatMap snd groups
            keys = map fst groups
            timed = [(s, e.place) | (g, es) <- groups, g /= WholeDay, e <- es, Just s <- [startOf e]]
         in conjoin
              [ counterexample "each entry once" (sort (map (\e -> e.place) flat) === map (\e -> e.place) entries)
              , counterexample "no empty group" (all (not . null . snd) groups)
              , counterexample "the whole day first, in the source's order" $
                  takeWhile ((== WholeDay) . fst) groups == take 1 [g | g@(WholeDay, _) <- groups]
                    && [e.place | (WholeDay, es) <- groups, e <- es] == [e.place | e <- entries, e.time == AllDay]
              , counterexample "timed entries by start, then source" (timed === sort timed)
              , counterexample "each entry in its start's group" (and [groupFor s == g | (g, es) <- groups, e <- es, Just s <- [startOf e]])
              , counterexample "the groups in one order, each once, whatever else the day holds" (keys === filter (`elem` keys) clockOrder)
              ]
  where
    startOf e = case e.time of
      AllDay -> Nothing
      At s -> Just s
      Span s _ -> Just s
    -- The groups in the order they are shown, and the group of a start,
    -- from the hours the owner gave.
    clockOrder = [WholeDay, During Night, During Dawn, During Morning, During Noon, During Afternoon, During Evening, LateNight]
    groupFor s
      | s.todHour < 6 = During Night
      | s.todHour < 9 = During Dawn
      | s.todHour < 12 = During Morning
      | s.todHour < 15 = During Noon
      | s.todHour < 18 = During Afternoon
      | s.todHour < 21 = During Evening
      | otherwise = LateNight

-- | Entries at their places, each for the whole day, at a time or over a
-- span. The times are on the quarter hour, so that equal starts are
-- common.
genEntries :: Gen [DayEntry]
genEntries = do
  -- Some days hold night hours only: their night before and night after
  -- are then next to each other.
  hours <- elements [[0 .. 23], [0 .. 5] <> [21 .. 23]]
  times <- listOf (genTime hours)
  pure (zipWith entryAt [0 ..] times)
  where
    genTime hours = do
      start <- (\h q -> at h (q * 15)) <$> elements hours <*> choose (0, 3)
      elements [AllDay, At start, Span start (at 23 59)] `suchThat` valid
    valid = \case
      Span s e -> e > s
      _ -> True
