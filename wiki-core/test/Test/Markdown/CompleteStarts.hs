-- | The completion read from a start of a block rather than from the top
-- ("Wiki.Core.Markdown.Starts"' 'linesFromStart', WP5.124 (f) of
-- docs/PLAN.md): the lists it opens are the ones it opens on the whole
-- text above the cursor. The starts come from a check of the same text,
-- of a text one change away (the check is made once the typing pauses, so
-- the text has often changed since), or from no check at all; in each of
-- the three, the answer is the whole text's.
module Test.Markdown.CompleteStarts
  ( spec
  , Change (..)
  , earlierOf
  , changedFrom
  , Mode (..)
  , earlierIn
  , coverStarts
  , Case (..)
  , caseAt
  , alikeEverywhere
  )
where

import Data.Text qualified as T
import Protolude
import System.Mem (getAllocationCounter, setAllocationCounter)
import Test.Cases (coverage, longText, longTexts)
import Test.Hspec hiding (after, before)
import Test.Hspec.QuickCheck (modifyMaxSuccess, prop)
import Test.QuickCheck (Arbitrary (..), Gen, Property, choose, conjoin, counterexample, cover, elements, forAll, forAllShow, frequency, property, (.&&.), (===))
import Wiki.Core.Markdown.Ast (parseMarkdown, parseShapes)
import Wiki.Core.Markdown.Complete
import Wiki.Core.Markdown.Edit qualified as Edit
import Wiki.Core.Markdown.Problems (Checked (..), checkText)
import Wiki.Core.Markdown.Starts (Earlier (..), blockDefinitions, blockStarts, earlierFrom, linesBelow, linesFromStart, noEarlier)
import Wiki.Core.PageKind (PageKind (..))

spec :: Spec
spec = describe "Wiki.Core.Markdown.Complete, from a start of a block" $ do
  describe "the definitions handed" $ do
    it "reads each definition of a link's reference and of a footnote, by line, in a line of its own" $
      fmap (blockDefinitions text) (parseShapes text)
        `shouldBe` Right [(3, Just "[a]: x"), (5, Nothing), (8, Just "[c]: x"), (11, Just "[^n]: x"), (14, Nothing)]
    it "hands the definitions above the start, the first of each label, and never goes to the top for one" $ do
      let doc = ["[a]: /u", "", "[^n]: A note.", "    More of it.", "", "[A]: /other", "", "Some text.", "", "More text [a][^n]."]
          e = earlierOf (T.intercalate "\n" doc)
      linesFromStart e doc `shouldBe` Just ["[a]: x", "[^n]: x", "", "More text [a][^n]."]
    it "takes no start whose line is indented after a footnote's definition, which would take it in" $ do
      let doc = ["[^n]: A note.", "", "Text.", "", "    code", "", "Last"]
          e = earlierOf (T.intercalate "\n" doc)
      linesFromStart e (take 5 doc) `shouldBe` Just ["[^n]: x", "", "Text.", "", "    code"]
    it "reads no line where two definitions start, or one inside a quotation, an item or a footnote (Codex, #298's second round)" $ do
      let doc = "[^n]: > [^m]: note\n\n[^o]: [a]: /u\n\n> [b]: /q\n\n[c]: /v"
      fmap (blockDefinitions doc) (parseShapes doc) `shouldBe` Right [(1, Nothing), (3, Nothing), (5, Nothing), (7, Just "[c]: x")]
    it "reads no line holding ]: that no shape reports, as a definition in a footnote defined twice, and counts none in a listing (Codex, #298's third round; #301)" $ do
      let doc = "[^n]: first\n\n[^n]: [r]: /url\n\nParagraph.\n\n```\n[s]: /listed\n```\n\n[c]: /v"
      fmap (blockDefinitions doc) (parseShapes doc) `shouldBe` Right [(1, Just "[^n]: x"), (3, Nothing), (11, Just "[c]: x")]
    it "suspects a line holding ]: only in a run of lines one of which starts with [ after its container marks (#298's third round)" $ do
      let doc =
            T.intercalate
              "\n"
              [ "Prose with `[x]: ` in inline code."
              , ""
              , "[foo"
              , "bar]: /u"
              , ""
              , "> [q]: /quoted"
              , ""
              , "- [i]: /item"
              , ""
              , "1. a"
              , "   2) [j]: /nested"
              , ""
              , "```ts"
              , "[key: string]: number"
              , "```"
              , ""
              , "[c]: /v"
              ]
      -- The fence's line is in code, and no suspect (#301).
      fmap (blockDefinitions doc) (parseShapes doc) `shouldBe` Right [(4, Nothing), (6, Nothing), (8, Nothing), (11, Nothing), (17, Just "[c]: x")]
    it "reads no label holding an escape, which the parser reads and this does not (Codex, #298)" $
      fmap (blockDefinitions "[^a\\]:b]: old\n\n[b\\]]: /u\n\n[c]: /v") (parseShapes "[^a\\]:b]: old\n\n[b\\]]: /u\n\n[c]: /v")
        `shouldBe` Right [(1, Nothing), (3, Nothing), (5, Just "[c]: x")]
    it "finds the start in a long text from the cursor up, and the end below within the bound (Codex, #298)" $ do
      -- 16,000 short paragraphs under a definition (1,600 on wasm,
      -- "Test.Cases"): the start is the last paragraph's, after the
      -- definition.
      let n = longText 16000
          paragraphs = ["[a]: /u", ""] <> concat [["Paragraph " <> show i <> " [a].", ""] | i <- [1 .. n]]
      linesFromStart (earlierOf (T.intercalate "\n" (paragraphs <> ["/t"]))) paragraphs
        `shouldBe` Just ["[a]: x", "", "Paragraph " <> show n <> " [a].", ""]
      -- The first item of a tight list of 16,000 (1,600 on wasm, still
      -- far past the bound): no item follows a blank line, and no start
      -- is taken below; after the list, two paragraphs.
      let items = ["- item " <> show i | i <- [1 .. n]]
          listed = items <> ["", "After.", "", "Last."]
      linesBelow afterBound (earlierOf (T.intercalate "\n" ("- first" : listed))) [] "- first /t" "" listed `shouldBe` Nothing
      let short = take 3 items <> ["", "After.", "", "Last."]
      linesBelow afterBound (earlierOf (T.intercalate "\n" ("- first" : short))) [] "- first /t" "" short `shouldBe` Just 6
    it "keeps what the editor's check finds" $ do
      let found = checkText Document text
      startsOf text `shouldBe` found.starts
      definitionsOf text `shouldBe` found.definitions
    it "reads the whole text above where nothing was checked, up to the limit" $ do
      linesFromStart noEarlier ["Some", "text"] `shouldBe` Just ["Some", "text"]
      linesFromStart noEarlier (replicate 100 (T.replicate 60 "x")) `shouldBe` Nothing

  -- Issue #301: a line that looks like a definition inside a listing (a
  -- TypeScript index signature in a fence) is no definition to the parser,
  -- and costs the text below it no start.
  describe "a definition-like line in a listing" $ do
    it "answers below the issue's TypeScript fence on a long page from a start, as the whole text answers" $ do
      let fence = ["```ts", "interface Prices {", "  [key: string]: number", "}", "```", ""]
          filler = concat (replicate 40 [T.replicate 6 "Some more words. ", ""])
          above = ["# Prices", ""] <> fence <> filler
          slash = [SlashEntry {label = show command, keywords = [], sample = "", does = WritesCommand command} | command <- [Edit.Bold, Edit.Table]]
          asked =
            [ ("/" :: Text, [slashSource slash], "/ta")
            , ("\\", [mathSource (mathCommands [])], "$\\fr")
            , ("[[!", [wikiSource Presentation []], "[[!")
            , ("[!", [wikiSource Presentation []], "> [!")
            ]
      for_ asked $ \(name, srcs, typed) -> do
        let page = T.intercalate "\n" (above <> [typed])
            handed = linesFromStart (earlierOf page) above
            at above' = At {place = InText, above = above', before = typed, after = "", below = [], cutBelow = False}
            whole = complete srcs (readLine above) (at above)
        -- The text handed is the last paragraph alone, below the fence, and
        -- a list opens there.
        (name, length <$> handed, (\h -> complete srcs (readLine h) (at h)) <$> handed, isJust whole)
          `shouldBe` (name, Just 2, Just whole, True)
    it "counts no line inside a fence, indented code or HTML, and every line the parser reads as a definition" $ do
      let doc =
            T.intercalate
              "\n"
              [ "```ts"
              , "[key: string]: number"
              , "```"
              , "[r]: /right-after"
              , ""
              , "- ```"
              , "  [i]: /in-the-item"
              , "[o]: /out-of-the-item"
              , ""
              , "> ~~~~"
              , "> [q]: /quoted"
              , "> ~~~"
              , "> [q2]: /still-quoted"
              , "> ~~~~"
              , ""
              , "    interface A {"
              , "    [k]: number"
              , ""
              , "[^n]: A note."
              , ""
              , "    ```"
              , "    [f]: /in-the-note"
              , "    ```"
              , ""
              , "[a]: /u"
              , "$$x$$"
              , ""
              , "<!--"
              , "[h]: /in-a-comment"
              , "-->"
              , "[h2]: /after-it"
              , ""
              , "```"
              , "[open]: /to-the-end"
              ]
      fmap (blockDefinitions doc) (parseShapes doc)
        `shouldBe` Right [(4, Just "[r]: x"), (8, Just "[o]: x"), (19, Just "[^n]: x"), (25, Just "[a]: x"), (31, Just "[h2]: x")]
    -- The check finds the definitions after every pause in the typing, in
    -- one pass over the lines, looking up only a line that holds ]: among
    -- the code shapes ("Wiki.Core.Markdown.Starts" says how). Counted, not
    -- timed: the bytes the work allocates, and those of the parse the check
    -- makes first, which the shapes come from.
    it "finds the definitions of a page of 16,000 fences, each holding a line like one, allocating in proportion to the page and little beside its parse" $ do
      let n = longText 16000
          page k = T.intercalate "\n" (concat (replicate k ["```ts", "[key: string]: number", "```", ""]))
          allocatedFor k = do
            let fences = page k
            _ <- evaluate (T.length fences)
            setAllocationCounter 0
            shapes <- evaluate (fromRight [] (parseShapes fences))
            _ <- evaluate (T.length (show shapes))
            parsing <- negate <$> getAllocationCounter
            setAllocationCounter 0
            found <- evaluate (sum [l + maybe 0 T.length d | (l, d) <- blockDefinitions fences shapes])
            left <- getAllocationCounter
            pure (found, negate left, parsing)
      (found, small, _) <- allocatedFor (n `div` 10)
      (found', large, parsing) <- allocatedFor n
      putStrLn ("the definitions of " <> show (n `div` 10) <> " and " <> show n <> " fences: " <> show (small `div` 1024) <> " KiB and " <> show (large `div` 1024) <> " KiB allocated, the parse " <> show (parsing `div` 1024) <> " KiB" :: Text)
      -- Measured natively on 2026-11-25, unoptimised: 11.7 MB and 118 MB
      -- for 1,600 and 16,000 fences (8.4 MB and 84 MB before #301, which
      -- counted each fenced line), the parse 295 MB and 2.9 GB.
      (found, found', large < 15 * small, large < parsing `div` 10) `shouldBe` (0, 0, True, True)
    modifyMaxSuccess (* 5) . prop "counts every line the parser reads as a definition, and leaves out lines in listings" $
      coverage (forAll genLabelled countsEveryDefinition)
    longTexts . prop "answers below a listing from a start, as the whole text answers" $
      coverage (forAllShow genBelowListing (show . fst) belowListing)

  -- Every trigger of the wiki's own completion, on generated places, with
  -- the starts of each mode: 200 cases natively, five on wasm
  -- ("Test.Cases"' long texts). The
  -- labels say how often a case reads from a start, with definitions,
  -- and with the lines below cut (about 80, 30 and 15 in a hundred); a
  -- check of the coverage would run the cases on to many more.
  modifyMaxSuccess (* 2) $ do
    longTexts . prop "opens the list the whole text opens, for markers, kinds of box, end markers, the / menu and the commands of math" $
      forAll genCase $
        \c -> alikeEverywhere (sourcesOf c) c
  where
    text = T.intercalate "\n" ["Para.", "", "[a]: /u", "", "> [b]: /v \"t\"", "> text", "", "[c]: /w", "text after", "", "[^n]: note", "    more", "", "- [d]: /x", "", "End [a] [^n]"]

----------------------------------------------------------------------
-- The modes

-- | Where the starts come from: a check of the text as it is, of the text
-- before a change, of the text before the cursor's line was typed (as it
-- was, or not there yet), of the text before some lines above were typed
-- (a fence, a marker, HTML, a container), of the text before a line below
-- was changed into one of those, or none.
data Mode = Same | Before Change | Typing Int | TypedAbove | ChangedBelow | Unchecked
  deriving stock (Show)

-- | A change since the check: at a thousandth of the text (moved to the
-- start of its line, or not), this many characters of the text now stood
-- in place of this text then. It may fall above the cursor, on its line
-- or below it, and add or take away blank lines, containers, fences and
-- the wiki's markers.
data Change = Change Int Bool Int Text
  deriving stock (Show)

instance Arbitrary Change where
  arbitrary =
    Change
      <$> choose (0, 1000)
      <*> elements [False, True]
      <*> frequency [(2, pure 0), (3, choose (1, 6)), (2, choose (7, 40))]
      <*> elements ["", "x", "\n", "\n\n", "- ", "> ", "    ", "```\n", "<div>\n", "<!--\n", "[[!notes]]\n", "[[/notes]]\n", "[^9]: x\n", "[a]: /z\n", "\n\nx\n\n"]

-- | The text a check read, given the text now and a change since.
changedFrom :: Text -> Change -> Text
changedFrom now (Change per toLine removed was) = T.take at now <> was <> T.drop (at + min removed (T.length now - at)) now
  where
    near = T.length now * per `div` 1000
    at = if toLine then T.length (T.dropWhileEnd (/= '\n') (T.take near now)) else near

-- | What the editor keeps of a check of a text: the starts and the
-- definitions the check finds ("Wiki.Core.Markdown.Problems"'
-- 'checkText', from the same parse; checked once against it here), made
-- from the parse alone, which the properties ask for many texts.
earlierOf :: Text -> Earlier
earlierOf old = earlierFrom old (startsOf old) (definitionsOf old)

-- | The starts and the definitions a check of a text finds, from the
-- parse alone.
startsOf :: Text -> [Int]
startsOf old = either (const [1]) (blockStarts old) (parseShapes old)

definitionsOf :: Text -> [(Int, Maybe Text)]
definitionsOf old = either (const []) (blockDefinitions old) (parseShapes old)

-- | The earlier check of a mode, for the text now.
earlierIn :: Text -> Mode -> Earlier
earlierIn now = \case
  Same -> earlierOf now
  Before change -> earlierOf (changedFrom now change)
  Unchecked -> noEarlier
  Typing _ -> earlierOf now
  TypedAbove -> earlierOf now
  ChangedBelow -> earlierOf now

-- | Labels the coverage of a case by what its starts made of the text
-- above: read from a start below the first line, with definitions before
-- it, or whole.
coverStarts :: [Text] -> Maybe [Text] -> Property -> Property
coverStarts above handed =
  cover 30 (maybe False (\h -> length h < length above) handed) "read from a start"
    . cover 3 (maybe False (any (T.isSuffixOf "]: x")) handed) "with definitions"

----------------------------------------------------------------------
-- The cases

-- | A place: the lines above the cursor's, the cursor's line before the
-- cursor (a trigger at its end) and after it, the lines below, the
-- change since the check, and the sources.
data Case = Case
  { above :: [Text]
  , before :: Text
  , after :: Text
  , below :: [Text]
  , change :: Change
  , kind :: Kind
  , trigger :: Text
  , typedAbove :: (Int, [Text])
  -- ^ Lines typed above since the check, and where they are in 'above'.
  }
  deriving stock (Show)

modes :: Case -> [Mode]
modes c = [Same, Before c.change, Typing 0, Typing 1, Typing 2, TypedAbove, ChangedBelow, Unchecked]

-- | The sources a generated case asks.
sourcesOf :: Case -> [Source]
sourcesOf c = case c.kind of
  Markers -> [wikiSource Presentation []]
  Slash -> [slashSource entries]
  Commands -> [mathSource (mathCommands [])]
  where
    entries = [SlashEntry {label = show command, keywords = [], sample = "", does = WritesCommand command} | command <- [Edit.Bold, Edit.Table, Edit.Quote, Edit.CodeBlock, Edit.SpoilerBlock, Edit.Box "NOTE"]]

-- | A case at a marked text (the lines above, the cursor's line before
-- the mark and after it, the lines below), the trigger the line ends
-- with, and a change since the check: for the properties of the other
-- modules, whose generators make the texts.
caseAt :: Text -> Text -> Change -> Case
caseAt marked trigger change =
  Case {above = initDef [] ls, before = fromMaybe "" (lastMay ls), after = T.takeWhile (/= '\n') (T.drop 1 rest), below = drop 1 (T.splitOn "\n" (T.drop 1 rest)), change, kind = Markers, trigger, typedAbove = (0, [])}
  where
    (upTo, rest) = T.breakOn "‸" marked
    ls = T.splitOn "\n" upTo

-- | The answer from the starts of every mode is the whole text's.
alikeEverywhere :: [Source] -> Case -> Property
alikeEverywhere srcs c =
  counterexample (toS (T.intercalate "\n" (c.above <> [c.before <> "‸" <> c.after] <> c.below))) . cover 30 (isJust whole) "a list opens" $
    -- Each text checked once (modes may check the same text), and the
    -- whole text's answer worked out once.
    conjoin [counterexample (show mode) (sameAnswer srcs c whole (earlierOf old)) | (mode, old) <- ordNubOn snd [(mode, checkedIn c mode) | mode <- modes c, checked mode]]
      .&&. sameAnswer srcs c whole noEarlier
  where
    whole = complete srcs (readLine c.above) At {place = InText, above = c.above, before = c.before, after = c.after, below = c.below, cutBelow = False}
    checked = \case
      Unchecked -> False
      _ -> True
    ordNubOn f = go []
      where
        go _ [] = []
        go seen (x : xs)
          | f x `elem` seen = go seen xs
          | otherwise = x : go (f x : seen) xs

-- | The text the check of a mode read (none checked: no text).
checkedIn :: Case -> Mode -> Text
checkedIn c = \case
  Same -> now
  Before change -> changedFrom now change
  -- The line before the trigger was typed, or before the line was (and a
  -- blank line after it, if there is one).
  Typing 0 -> T.intercalate "\n" (c.above <> [T.dropEnd (T.length c.trigger) c.before <> c.after] <> c.below)
  Typing 1 -> T.intercalate "\n" (c.above <> c.below)
  Typing _ -> T.intercalate "\n" (c.above <> (if take 1 c.below == [""] then drop 1 c.below else c.below))
  ChangedBelow -> let (k, _) = c.typedAbove in T.intercalate "\n" (c.above <> [c.before <> c.after] <> [if i == k `mod` max 1 (length c.below) then "Text." else l | (i, l) <- zip [0 ..] c.below])
  TypedAbove -> let (k, typed) = c.typedAbove in T.intercalate "\n" (take k c.above <> drop (k + length typed) c.above <> [c.before <> c.after] <> c.below)
  Unchecked -> ""
  where
    now = T.intercalate "\n" (c.above <> [c.before <> c.after] <> c.below)

-- | The answer from the starts of an earlier check is the whole text's.
sameAnswer :: [Source] -> Case -> Maybe Completion -> Earlier -> Property
sameAnswer srcs c whole earlier =
  coverStarts c.above handed . cover 10 (maybe False (< length c.below) keptBelow) "lines below cut" $ case handed of
    Just h -> complete srcs (readLine h) (placed h) === whole
    Nothing -> property True
  where
    keptBelow = linesBelow afterBound earlier c.above c.before c.after c.below
    placed above' = At {place = InText, above = above', before = c.before, after = c.after, below = maybe c.below (`take` c.below) keptBelow, cutBelow = isJust keptBelow && pastBound c.after c.below}
    handed = linesFromStart earlier c.above

genCase :: Gen Case
genCase = do
  above' <- concat <$> (choose (0, 14) >>= \k -> replicateM k piece)
  at' <- choose (0, length above')
  typed <- elements [["```"], ["```math"], ["$$"], ["$$", "x"], ["[[!div]]"], ["[[/div]]"], ["<div>"], ["<!--"], ["- item"], ["> quoted"], ["[^z]: note"], ["    code"], ["$$"], [""], ["Text"]]
  let above = take at' above' <> typed <> drop at' above'
  kind <- elements [Markers, Slash, Commands]
  lead <- elements ["", "", "> ", "- ", "1. ", "  ", "    ", "[^n]: ", "> - ", "[^m]: ", "[^n]: > "]
  words' <- case kind of
    Markers -> pure ""
    Slash -> elements ["", "Some text ", "See [a] and [^n] ", "Text [b][^m] ", "[See "]
    Commands -> elements ["", "$", "Text $a + ", "See [a] $", "Costs $5 and ", "*so $", "$$ "]
  trigger <- case kind of
    Markers -> elements ["[[!", "[!", "[[/", "[[!d"]
    Slash -> elements ["/", "/ta", "/b"]
    Commands -> elements ["\\fr", "\\alp", "\\beg", "\\sqrt{\\x"]
  after <- elements ["", "", " more", "]]", "][r]", "$", "}{} b$"]
  below <- concat <$> (choose (0, 6) >>= \k -> replicateM k piece)
  farther <- elements [[], [], [], ["", T.intercalate "\n\n" (replicate 70 "Words of a long text.")]]
  change <- arbitrary
  pure Case {above, before = lead <> words' <> trigger, after, below = below <> farther, change, kind, trigger, typedAbove = (at', typed)}

-- | Which completion a case asks: the wiki's markers and kinds of box,
-- the @/@ menu, or the commands of math.
data Kind = Markers | Slash | Commands
  deriving stock (Show)

-- | Some lines of a page: paragraphs, blank lines, definitions of links
-- and footnotes (and references to them), lists loose and tight,
-- quotations and boxes, code fenced and indented, HTML, and the wiki's
-- markers open and closed.
piece :: Gen [Text]
piece =
  frequency
    [ (6, pure ["Some text of a paragraph.", ""])
    , (4, pure [""])
    , (2, pure [T.intercalate " " (replicate 12 "A longer line of words."), ""])
    , (2, elements [["[a]: /url", ""], ["[A]: /other \"title\"", ""], ["[^n]: A note.", "    More of it.", ""], ["[^m]: Another note.", ""], ["> [b]: /quoted", ""], ["- [c]: /listed", ""]])
    , -- Definitions the parser reads otherwise than a line's text says:
      -- two on one line (a definition in a footnote's body, with and
      -- without a quotation's mark), one in a quotation or an item, a
      -- label defined twice.
      (2, elements [["[^n]: [a]: /u", ""], ["[^n]: [^m]: note", ""], ["[^n]: > [^m]: note", ""], ["> [^m]: quoted note", ""], ["- [^m]: listed note", ""], ["[^n]: Again.", ""], ["[a]: /again", ""], ["[^n]: [a]: /u", "[^n]: [^m]: note", ""]])
    , -- A link to a reference, its label defined or not (the cursor may be
      -- in its text, on the line above), and a definition in the body of
      -- a footnote defined twice, which the parser drops and still reads.
      (2, elements [["[r]: /ref", ""], ["next][r]", ""], ["[^n]: first", "", "[^n]: [r]: /url", ""], ["[^n]: first", "", "[^n]: [r]: /url", "", "Paragraph."]])
    , (2, elements [["See [a], [b] and [^n].", ""], ["[a][^m]", ""]])
    , (2, elements [["- one", "- two", ""], ["- one", "", "- two", "", "- three", ""], ["1. one", "2. two", ""], ["- one", "", "  more of one", "- two"], ["- one", "", "- two"], ["1. one", "", "2. two", "3. three"], ["- one", "- two"]])
    , (2, elements [["> quoted", "> more", ""], ["> [!NOTE]", "> A box.", ""], ["> quoted", "lazy", ""]])
    , (2, elements [["```", "code", "```", ""], ["```math", "x^2", "```", ""], ["    indented code", ""], ["```"]])
    , -- A line like a definition in a listing, or let out of one (#301).
      (2, map (T.replace "{L}" "a") <$> listing True)
    , (1, elements [["<div>", "", "html", "", "</div>", ""], ["<!--", "comment", "-->", ""]])
    , (2, elements [["[[!div]]", "", "Inside.", "", "[[/div]]", ""], ["[[!div]]", ""], ["[[/div]]", ""], ["[[!notes]]", "Notes.", "[[/notes]]", ""], ["[[!spoiler]]{s}(More)", "Hidden.", ""]])
    , (1, elements [["# Heading", ""], ["Title", "===", ""], ["---", ""], ["| a | b |", "| - | - |", "| c | d |", ""]])
    ]

----------------------------------------------------------------------
-- Listings (#301)

-- | Some lines with a line like a definition in a listing, @{L}@ standing
-- for its label: in a fence of backticks or tildes, three or four of
-- them, with an info string or none, closed by its mark, by a longer one,
-- by a shorter one (which does not close it) or left open; with a fence
-- inside that closes a fence of three; at the top level, in a quotation,
-- an item, a quotation's item, a footnote's definition, indented by two
-- spaces; the line like a definition written without its container's
-- prefix, which ends the container and makes it a definition; in
-- indented code; followed by a blank line, by a definition right after
-- the listing, by a paragraph, or by nothing. Where a line like a
-- definition may not be let out of its listing ('False'), it is never
-- written without its container's prefix, and no fence inside closes the
-- listing's.
listing :: Bool -> Gen [Text]
listing loose = do
  (first', other) <- elements [("", ""), ("", ""), ("> ", "> "), ("- ", "  "), ("1. ", "   "), ("> - ", ">   "), ("[^f]: ", "    "), ("  ", "  ")]
  mark <- elements ["```", "~~~", "````", "~~~~"]
  info <- elements ["", "ts", "math", "html", "mermaid"]
  body <-
    elements $
      [ ["[{L}]: /listed"]
      , ["interface A {", "  [{L}]: number", "}"]
      , ["[^{L}]: listed note"]
      ]
        <> [[T.take 3 mark, "[{L}]: /listed", T.take 3 mark] | loose || T.length mark > 3]
  close <- elements [Just mark, Just (mark <> T.take 1 mark), Just (T.take 3 mark), Nothing]
  dropped <- if loose then frequency [(5, pure False), (1, pure True)] else pure False
  indented <- frequency [(5, pure False), (1, pure True)]
  after' <- elements [[""], ["[{L}]: /real", ""], ["Text.", ""], []]
  let prefixed l = (if dropped && "]:" `T.isInfixOf` l then "" else other) <> l
      fenced = (first' <> mark <> info) : map prefixed body <> [other <> c | Just c <- [close]]
      code = (first' <> "    interface A {") : map (prefixed . ("    " <>)) body
  pure ((if indented then code else fenced) <> after')

-- | Whether a line is one of 'listing''s lines like a definition, inside
-- its listing or not.
listedLine :: Text -> Bool
listedLine l = any (`T.isInfixOf` l) ["]: /listed", "]: number", "]: listed note"]

-- | A page of listings, paragraphs, definitions and display math, each
-- line like a definition with a label of its own (@k1@, @k2@, …).
genLabelled :: Gen [Text]
genLabelled = do
  pieces <- choose (1, 6) >>= \k -> replicateM k (frequency [(4, listing True), (1, pure ["Some text.", ""]), (1, elements definitions)])
  pure (snd (mapAccumL label (1 :: Int) (concat pieces)))
  where
    -- Definitions, and the code shapes that hold definitions, follow one
    -- on its line, or come right before one: display math that is all of
    -- a paragraph begun by two definitions (with a line of attributes
    -- before it, or none); a fence on a footnote's line; HTML that runs
    -- to a blank line, a comment, HTML around a definition; display math
    -- in brackets; a fence after a quotation's mark and a tab.
    definitions =
      [ ["[{L}]: /real", ""]
      , ["[{L}]: /real", "[{L}]: /second", "$$x$$", ""]
      , ["{.cls}", "[{L}]: /real", "[{L}]: /second", "$$x$$", ""]
      , ["[^{L}]: ```", "    [{L}]: /listed", "    ```", ""]
      , ["$$", "[{L}]: /in-math", "$$", ""]
      , ["<div>", "[{L}]: /in-html", "</div>", "[{L}]: /after", ""]
      , ["<!--", "[{L}]: /in-comment", "-->", "[{L}]: /real", ""]
      , ["<div>", "", "[{L}]: /real", "", "</div>", ""]
      , ["\\[", "[{L}]: /in-math", "\\]", ""]
      , [">\t```", ">\t[{L}]: /listed", ">\t```", ""]
      , [">\t```", "[{L}]: /real", ""]
      ]
    label n l
      | "{L}" `T.isInfixOf` l = (n + 1, T.replace "{L}" ("k" <> show n) l)
      | otherwise = (n, l)

-- | The lines the check counts as definitions ('blockDefinitions') hold
-- every definition the parser reads: a label defined on a line is one a
-- reference before the page reads as a link or a footnote's number. Each
-- label is on one line of the page, so the line is the one that defines
-- it. The coverage asks for pages with a definition read, and with a line
-- like one left out.
countsEveryDefinition :: [Text] -> Property
countsEveryDefinition doc =
  counterexample (toS text)
    . cover 20 (not (null real)) "a definition read"
    . cover 40 (any (\(l, _) -> l `notElem` counted) labelled) "a line like a definition left out"
    $ conjoin [counterexample (toS ("line " <> show l <> " defines " <> label)) (l `elem` counted) | (l, label) <- real]
  where
    text = T.intercalate "\n" doc
    counted = map fst (definitionsOf text)
    labelled = [(l, label) | (l, line) <- zip [1 :: Int ..] doc, Just label <- [labelOf line]]
    real = filter (defines . snd) labelled
    labelOf line = do
      let label = T.takeWhileEnd (/= '[') (fst (T.breakOn "]:" line))
      guard ("k" `T.isPrefixOf` T.dropWhile (== '^') label && "]:" `T.isInfixOf` line)
      pure label
    defines label = firstBlock ("[" <> label <> "]\n\n" <> text) /= firstBlock ("[" <> label <> "]")
    firstBlock t = either (const Nothing) head (parseMarkdown t)

-- | A place below a listing, or between two: the listing (its lines like
-- a definition inside it), a paragraph or a definition, a paragraph, the
-- cursor's line with a trigger, and the lines below (another listing, a
-- text longer than the completion reads below). The labels are those the
-- cursor's line refers to.
genBelowListing :: Gen (Case, [Source])
genBelowListing = do
  ls <- map (T.replace "{L}" "a") <$> listing False
  more <- elements [[], ["Some text of a paragraph.", ""], ["[b]: /real", ""]]
  let long = T.intercalate "\n\n" (replicate 70 "Words of a long text.")
  below <- elements [[], ["", "Next."], ["", "```", "[a]: /listed", "```"], ["", "Next.", "", "After.", "", long], ["", "```", "[a]: /listed", "```", "", "Next.", "", long]]
  (srcs, trigger, typed) <-
    elements
      [ ([slashSource [SlashEntry {label = show command, keywords = [], sample = "", does = WritesCommand command} | command <- [Edit.Bold, Edit.Table]]], "/ta", "/ta")
      , ([mathSource (mathCommands [])], "\\fr", "$\\fr")
      , ([wikiSource Presentation []], "[[!", "[[!")
      , ([wikiSource Presentation []], "[!", "> [!")
      ]
  words' <- elements ["", "See [a] and [^a] "]
  change <- arbitrary
  let marked = T.intercalate "\n" (ls <> more <> ["Text before.", "", words' <> typed <> "‸"] <> below)
  pure (caseAt marked trigger change, srcs)

-- | Below a listing the answer is the whole text's; the coverage asks that
-- a start below the listing is used, for most places.
belowListing :: (Case, [Source]) -> Property
belowListing (c, srcs) =
  cover 40 (any listedLine c.above && maybe False (\h -> length h < length c.above && not (any listedLine h)) handed) "read from a start below the listing" $
    alikeEverywhere srcs c
  where
    handed = linesFromStart (earlierOf (T.intercalate "\n" (c.above <> [c.before <> c.after] <> c.below))) c.above
