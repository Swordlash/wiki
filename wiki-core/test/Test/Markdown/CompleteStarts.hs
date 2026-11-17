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
import Test.Hspec hiding (after, before)
import Test.Hspec.QuickCheck (modifyMaxSuccess, prop)
import Test.QuickCheck (Arbitrary (..), Gen, Property, choose, conjoin, counterexample, cover, elements, forAll, frequency, property, (.&&.), (===))
import Wiki.Core.Markdown.Ast (parseShapes)
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
    it "reads no line holding ]: that no shape reports, as a definition in a footnote defined twice, or one in a listing (Codex, #298's third round)" $ do
      let doc = "[^n]: first\n\n[^n]: [r]: /url\n\nParagraph.\n\n```\n[s]: /listed\n```\n\n[c]: /v"
      fmap (blockDefinitions doc) (parseShapes doc) `shouldBe` Right [(1, Just "[^n]: x"), (3, Nothing), (8, Nothing), (11, Just "[c]: x")]
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
      fmap (blockDefinitions doc) (parseShapes doc) `shouldBe` Right [(4, Nothing), (6, Nothing), (8, Nothing), (11, Nothing), (14, Nothing), (17, Just "[c]: x")]
    it "reads no label holding an escape, which the parser reads and this does not (Codex, #298)" $
      fmap (blockDefinitions "[^a\\]:b]: old\n\n[b\\]]: /u\n\n[c]: /v") (parseShapes "[^a\\]:b]: old\n\n[b\\]]: /u\n\n[c]: /v")
        `shouldBe` Right [(1, Nothing), (3, Nothing), (5, Just "[c]: x")]
    it "finds the start in a long text from the cursor up, and the end below within the bound (Codex, #298)" $ do
      -- 16,000 short paragraphs under a definition: the start is the last
      -- paragraph's, after the definition.
      let paragraphs = ["[a]: /u", ""] <> concat [["Paragraph " <> show i <> " [a].", ""] | i <- [1 .. 16000 :: Int]]
      linesFromStart (earlierOf (T.intercalate "\n" (paragraphs <> ["/t"]))) paragraphs
        `shouldBe` Just ["[a]: x", "", "Paragraph 16000 [a].", ""]
      -- The first item of a tight list of 16,000: no item follows a blank
      -- line, and no start is taken below; after the list, two paragraphs.
      let items = ["- item " <> show i | i <- [1 .. 16000 :: Int]]
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

  -- Every trigger of the wiki's own completion, on generated places, with
  -- the starts of each mode: 200 cases natively, a quarter on wasm. The
  -- labels say how often a case reads from a start, with definitions,
  -- and with the lines below cut (about 80, 30 and 15 in a hundred); a
  -- check of the coverage would run the cases on to many more.
  modifyMaxSuccess (* 2) $ do
    prop "opens the list the whole text opens, for markers, kinds of box, end markers and the / menu" $
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
  typed <- elements [["```"], ["```math"], ["[[!div]]"], ["[[/div]]"], ["<div>"], ["<!--"], ["- item"], ["> quoted"], ["[^z]: note"], ["    code"], ["$$"], [""], ["Text"]]
  let above = take at' above' <> typed <> drop at' above'
  kind <- elements [Markers, Slash]
  lead <- elements ["", "", "> ", "- ", "1. ", "  ", "    ", "[^n]: ", "> - ", "[^m]: ", "[^n]: > "]
  words' <- case kind of
    Markers -> pure ""
    Slash -> elements ["", "Some text ", "See [a] and [^n] ", "Text [b][^m] ", "[See "]
  trigger <- case kind of
    Markers -> elements ["[[!", "[!", "[[/", "[[!d"]
    Slash -> elements ["/", "/ta", "/b"]
  after <- elements ["", "", " more", "]]", "][r]"]
  below <- concat <$> (choose (0, 6) >>= \k -> replicateM k piece)
  farther <- elements [[], [], [], ["", T.intercalate "\n\n" (replicate 70 "Words of a long text.")]]
  change <- arbitrary
  pure Case {above, before = lead <> words' <> trigger, after, below = below <> farther, change, kind, trigger, typedAbove = (at', typed)}

-- | Which completion a case asks: the wiki's markers and kinds of box, or
-- the @/@ menu.
data Kind = Markers | Slash
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
    , (1, elements [["<div>", "", "html", "", "</div>", ""], ["<!--", "comment", "-->", ""]])
    , (2, elements [["[[!div]]", "", "Inside.", "", "[[/div]]", ""], ["[[!div]]", ""], ["[[/div]]", ""], ["[[!notes]]", "Notes.", "[[/notes]]", ""], ["[[!spoiler]]{s}(More)", "Hidden.", ""]])
    , (1, elements [["# Heading", ""], ["Title", "===", ""], ["---", ""], ["| a | b |", "| - | - |", "| c | d |", ""]])
    ]
