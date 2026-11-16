-- | The completion of the wiki's markers and kinds of box, against the
-- wiki's own parser as the oracle ("Wiki.Core.Markdown.Complete",
-- WP5.124 (b) of docs/PLAN.md, after Codex's review of #293). Documents
-- are generated with what a container holds and how its lines are
-- written: quotations with up to three spaces before and after their
-- marks, nested; lists' items; lazy lines of a quotation's paragraph;
-- indented code alone and in containers; fences; and the wiki's markers
-- in all of these. At a cursor line typed at the end of such a document,
-- the completion offers:
--
-- * a start marker exactly where the parser reads a marker on that line;
-- * a kind of box exactly where the parser reads the quotation's line as
--   a box's;
-- * the end markers exactly of the blocks an end marker on that line
--   ends, for the parser;
--
-- and a block's start marker, chosen, is written with its end marker so
-- that the parser reads the two as one block.
module Test.Markdown.CompleteOracle (spec) where

import Data.Text qualified as T
import Protolude
import Test.Hspec
import Test.Hspec.QuickCheck (modifyMaxSuccess, prop)
import Test.QuickCheck (Arbitrary (..), Gen, Property, checkCoverage, choose, conjoin, counterexample, cover, elements, forAll, frequency, property, (===))
import Wiki.Core.Markdown.Ast (Block (..), BoxMarker (..), MarkerLine (..), MarkerName (..), MarkerRole (..), closingMarker, holdsBlocks, markerNameText, parseWithAllMarkers)
import Wiki.Core.Markdown.Complete
import Wiki.Core.Markdown.Edit qualified as Edit
import Wiki.Core.Markdown.Problems (everyBlock)
import Wiki.Core.PageKind (PageKind (..))

spec :: Spec
spec = describe "Wiki.Core.Markdown.Complete, by the wiki's parser" $ do
  describe "Codex's cases (#293)" $ do
    it "offers the end of a block an indented code example inside it seems to end" $
      (ends ["[[!div]]", "", "    [[/div]]", ""] "", oracleEnds ["[[!div]]", "", "    [[/div]]", ""] "") `shouldBe` (["div"], ["div"])
    it "offers no box on a quotation's line after a lazy line" $
      (box ["> paragraph", "lazy"] "> ", oracleBox ["> paragraph", "lazy"] "> ") `shouldBe` (False, False)
    it "offers a marker after a quotation's mark and two spaces, and its end later" $ do
      (marker [] ">  ", oracleMarker [] ">  ") `shouldBe` (True, True)
      (ends [">  [[!div]]", "> Text"] "> ", oracleEnds [">  [[!div]]", "> Text"] "> ") `shouldBe` (["div"], ["div"])

  describe "Codex's cases (#293, second round)" $ do
    it "writes a block after a tab after a list's mark in a quotation as the parser reads it" $
      readAsWritten [] ">-\t" `shouldBe` []
    it "writes a block in a footnote's definition as the parser reads it" $
      readAsWritten ["Text.[^a]", ""] "  [^a]: " `shouldBe` []

  -- Ten times the suite's count: 1,000 cases natively, a quarter of that on
  -- wasm, where Test.Cases cuts the count first.
  modifyMaxSuccess (* 10) $ do
    prop "offers a start marker exactly where the parser reads one" $ \(Doc doc) (Line cursor) ->
      counterexample (toS (T.unlines (doc <> [cursor <> "[[!‸"]))) $
        marker doc cursor === oracleMarker doc cursor
    prop "offers a kind of box exactly where the parser reads a box's marker" $ \(Doc doc) (Line cursor) ->
      counterexample (toS (T.unlines (doc <> [cursor <> "[!‸"]))) $
        box doc cursor === oracleBox doc cursor
    prop "offers the end markers of exactly the blocks an end marker there ends" $ \(Doc doc) (Line cursor) ->
      counterexample (toS (T.unlines (doc <> [cursor <> "[[/‸"]))) $
        sort (ends doc cursor) === sort (oracleEnds doc cursor)
    prop "writes every marker and kind of box offered as the parser then reads it" $ \(Doc doc) (Line cursor) ->
      counterexample (toS (T.unlines (doc <> [cursor <> "‸"]))) $
        readAsWritten doc cursor === []
    prop "writes each entry of the / menu as the toolbar's command writes it on the whole text, selection included" $
      checkCoverage (forAll genSlashCase slashWritesAsApply)

  describe "the / menu" $ do
    it "opens at the end of a line of a paragraph, a list's item, a quotation, a box's text or a spoiler's body, and on an empty line" $
      map slashOpens ["Some text /ta‸", "- an item /‸", "> a quotation /‸", "> [!NOTE]\n> Its text /‸", "[[!spoiler]]{s}(More)\nIts body /‸", "/‸", "Text\n\n/‸   ", "Text\n/‸"]
        `shouldBe` replicate 8 True
    it "opens nowhere else: text after the cursor, code, math, HTML, a link, emphasis, a heading, a table's cell, a box's title, a spoiler's description, a footnote, a word, an address, a construct the lines below close" $
      map slashOpens ["*Some /‸\nmore*", "[see /‸\nmore](https://example.com)", "Title /‸\n===", "| a | b /‸\n| --- | --- |", "Some /‸ text", "Some `code /‸` here", "```\n/‸", "Text\n\n    /‸", "Math $a /‸$ here", "[see /‸ more](https://example.com)", "[a]( /‸)", "<span title=\"a /‸\">b</span>", "*Some /‸*", "# Title /‸", "| a | b |\n| --- | --- |\n| x /‸", "> [!NOTE] Its title /‸", "[[!spoiler]]{s}(See /‸", "[^n]: A note /‸", "and/‸", "https://example.com/pa‸", "Text /a‸b"]
        `shouldBe` replicate 21 False
    it "opens before a text longer than the bound, and not in a paragraph still open where the text read is cut" $ do
      let lines' = T.intercalate "\n" (replicate 60 "More words of a paragraph that goes on and on.")
      slashOpens ("Text /‸\n\n" <> lines') `shouldBe` True
      slashOpens ("Text /‸\n" <> lines') `shouldBe` False
      -- In a long list, quotation or box, the block after the cursor's
      -- paragraph is inside the same top-level block.
      slashOpens ("- An item /‸\n" <> T.intercalate "\n" (replicate 60 "- Another item of the same long list")) `shouldBe` True
      slashOpens ("> A quotation /‸\n>\n" <> T.intercalate "\n>\n" (replicate 60 "> Another paragraph of the same quotation")) `shouldBe` True
      slashOpens ("> [!NOTE]\n> A box /‸\n>\n" <> T.intercalate "\n>\n" (replicate 60 "> Another paragraph of the same box")) `shouldBe` True
    modifyMaxSuccess (* 10) . prop "opens where the place is text and not elsewhere, over generated places" $
      forAll genSlashPlace $
        \(marked, expected) -> counterexample (toS marked) (slashOpens marked === expected)
    it "writes each entry as its text in place of the line, the command's selection its one place, never as a snippet" $ do
      slashPlaces "/ta‸" Edit.Table `shouldBe` Just ["Column 1"]
      slashPlaces "/‸" Edit.SpoilerBlock `shouldBe` Just ["name"]
      slashPlaces "Go /‸" Edit.Link `shouldBe` Just ["link text"]
      slashPlaces "/‸" Edit.CodeBlock `shouldBe` Just [""]
      slashPlaces "/‸" Edit.Bold `shouldBe` Just ["bold text"]
      slashPlaces "/‸" Edit.Mermaid `shouldBe` Just ["graph LR\n  A --> B"]
      (.writing) <$> slashCandidate "  - item /ta‸" Edit.Table `shouldBe` Just (LineAsText "  - item /ta")
    it "writes what the command writes after a no-break space at the line's start, and with a carriage return in the line (#295)" $ do
      let asWritten c = (c.writing, fst (skeletonText c.skeleton))
      (asWritten <$> slashCandidate "\160hello /ta‸" Edit.Table)
        `shouldBe` Just (LineAsText "\160hello /ta", fst (Edit.apply Edit.Table "\160hello " (Edit.cursor 7)))
      (asWritten <$> slashCandidate "Some\rtext /b‸" Edit.Bold)
        `shouldBe` Just (LineAsText "Some\rtext /b", fst (Edit.apply Edit.Bold "Some\rtext " (Edit.cursor 10)))
    it "closes with Escape so that the same / does not open it again" $
      (.escape) <$> listOf' "/ta‸" `shouldBe` Just EscapeDismisses
    it "chooses with Enter only once a letter follows the /, or the reader moved in the list: a / alone at a line's end and Enter is a new line" $
      (.enter) <$> listOf' "Either /‸" `shouldBe` Just (EnterChoosesAfter 2)

----------------------------------------------------------------------
-- The completion, and the parser's reading

-- | The list at a cursor typed after the document's lines.
listAt :: [Text] -> Text -> Maybe Completion
listAt doc cursor = complete [wikiSource Presentation []] (readLine doc) At {place = InText, above = doc, before = cursor, after = "", below = []}

-- | Whether the list after a line's prefix and @[[!@ offers markers.
marker :: [Text] -> Text -> Bool
marker doc cursor = maybe False (any (T.isPrefixOf "[[!" . (.label)) . (.candidates)) (listAt doc (cursor <> "[[!"))

-- | Whether the list after a line's prefix and @[!@ offers kinds of box.
box :: [Text] -> Text -> Bool
box doc cursor = maybe False (any (T.isPrefixOf "[!" . (.label)) . (.candidates)) (listAt doc (cursor <> "[!"))

-- | The names whose end markers the list after @[[/@ offers.
ends :: [Text] -> Text -> [Text]
ends doc cursor = maybe [] (mapMaybe (T.stripSuffix "]]" <=< T.stripPrefix "[[/" . (.label)) . (.candidates)) (listAt doc (cursor <> "[[/"))

-- | What the parser reads on the last line of the document with this line
-- after it: its marker lines' roles there, and whether a box's marker is
-- there.
readLast :: [Text] -> Text -> ([MarkerRole], Bool)
readLast doc lastLine = case parseWithAllMarkers source of
  Right (_, boxes, lines', _) -> ([r | MarkerLine l _ r <- lines', l == n], any (\b -> b.markerLine == n) boxes)
  Left _ -> ([], False)
  where
    source = T.intercalate "\n" (doc <> [lastLine])
    n = length doc + 1

-- | Whether the parser reads @[[!toc]]@ after the prefix as a marker.
oracleMarker :: [Text] -> Text -> Bool
oracleMarker doc cursor = PlacesContents `elem` fst (readLast doc (cursor <> "[[!toc]]"))

-- | Whether the parser reads @[!NOTE]@ after the prefix as a box's marker.
oracleBox :: [Text] -> Text -> Bool
oracleBox doc cursor = snd (readLast doc (cursor <> "[!NOTE]"))

-- | The names whose end marker after the prefix the parser reads as the
-- end of a block.
oracleEnds :: [Text] -> Text -> [Text]
oracleEnds doc cursor =
  [ markerNameText name
  | name <- filter holdsBlocks [minBound .. maxBound]
  , any isEnd (fst (readLast doc (cursor <> closingMarker name)))
  ]
  where
    isEnd = \case
      Ends _ -> True
      _ -> False

-- | The candidates offered after @[[!@ and @[!@ at a cursor that the
-- parser does not read, once written, as they say: a skeleton with an end
-- marker opens a block on its first line that its last line ends; a
-- marker alone on its line is read as one; a box with a line for its text
-- is read as a box holding that text; a box's marker alone is read as one.
-- Each is written as CodeMirror's snippet writes it, the cursor line's
-- indentation before each further line. None is the expected answer.
readAsWritten :: [Text] -> Text -> [Text]
readAsWritten doc cursor =
  [ c.label
  | trigger <- ["[[!", "[!"]
  , Just found <- [listAt doc (cursor <> trigger)]
  , c <- found.candidates
  , not (readsAs found c)
  ]
  where
    n = length doc + 1
    readsAs found c =
      let (written, _) = skeletonText c.skeleton
          -- As CodeMirror writes it: a snippet's lines split at any line
          -- break, each further one after the line's leading white space
          -- as JavaScript's \s reads it; a text as it is.
          further = case c.writing of
            AsSnippet -> map ((T.takeWhile jsSpace cursor <>) <$>) (splitLines written)
            _ -> map Right (T.splitOn "\n" written)
          splitLines t = case T.splitOn "\n" (T.replace "\r" "\n" (T.replace "\r\n" "\n" t)) of
            first' : rest -> Left first' : map Right rest
            [] -> []
          jsSpace ch = isSpace ch || ch `elem` ['\xfeff', '\x2028', '\x2029']
          sentinel = "Zqxjv"
          lines' = case map (either identity identity) further of
            first' : rest -> (T.take found.from cursor <> first') : [l <> (if T.isPrefixOf "[!" c.label then sentinel else "") | l <- rest]
            [] -> []
          source = T.intercalate "\n" (doc <> lines')
       in case parseWithAllMarkers source of
            Left _ -> False
            Right (blocks, boxes, markerLines, _) ->
              let rolesAt k = [r | MarkerLine l _ r <- markerLines, l == k]
               in if T.isPrefixOf "[!" c.label
                    then
                      any (\b -> b.markerLine == n) boxes
                        && (length lines' < 2 || any (holds sentinel) (concatMap everyBlock blocks))
                    else case lines' of
                      [_] -> not (null (rolesAt n))
                      _ -> any isStart (rolesAt n) && Ends n `elem` rolesAt (n + length lines' - 1)
    isStart = \case
      Opens _ -> True
      Starts _ _ -> True
      _ -> False
    holds sentinel = \case
      Alert _ _ bs -> any (\b -> sentinel `T.isInfixOf` show b) bs
      _ -> False

----------------------------------------------------------------------
-- The / menu

-- | Every command the toolbar's @/@ menu offers, each as an entry by its
-- name ("Wiki.App.Editor.Tools" makes the editor's, from the toolbar).
slashCommands :: [Edit.Command]
slashCommands =
  [Edit.Bold, Edit.Italic, Edit.Strikethrough, Edit.Spoiler, Edit.Heading, Edit.BulletList, Edit.NumberedList, Edit.TaskList, Edit.Quote, Edit.InlineCode, Edit.CodeBlock, Edit.Link, Edit.Table]
    <> [Edit.InlineMath, Edit.DisplayMath, Edit.Mermaid, Edit.Tikz, Edit.HtmlBlock, Edit.HorizontalRule, Edit.SpoilerBlock, Edit.Box "NOTE", Edit.Box "EXERCISE"]
    <> map Edit.Html [minBound .. maxBound]

slashEntries' :: [SlashEntry]
slashEntries' = [SlashEntry {label = show command, keywords = [], sample = "", does = WritesCommand command} | command <- slashCommands]

-- | A marked text as a place: the lines above the cursor's, the cursor's
-- before the mark and after it, and the lines below.
slashAt :: Text -> At
slashAt marked =
  At {place = InText, above = initDef [] ls, before = fromMaybe "" (lastMay ls), after = T.takeWhile (/= '\n') (T.drop 1 rest), below = drop 1 (T.splitOn "\n" (T.drop 1 rest))}
  where
    (upTo, rest) = T.breakOn "‸" marked
    ls = T.splitOn "\n" upTo

listOf' :: Text -> Maybe Completion
listOf' marked = let at = slashAt marked in complete [slashSource slashEntries'] (readLine at.above) at

-- | Whether the @/@ menu opens at the mark.
slashOpens :: Text -> Bool
slashOpens = isJust . listOf'

-- | The candidate of a command at the mark.
slashCandidate :: Text -> Edit.Command -> Maybe Candidate
slashCandidate marked command = listOf' marked >>= find ((== show command) . (.label)) . (.candidates)

-- | What the places of a command's candidate hold, in their order.
slashPlaces :: Text -> Edit.Command -> Maybe [Text]
slashPlaces marked command = do
  c <- slashCandidate marked command
  let (text, ranges) = skeletonText c.skeleton
  Just [T.take (e - s) (T.drop s text) | (_, (s, e)) <- ranges]

-- | A place for the @/@ menu: the lines above, the cursor's line before the
-- @/@ (in containers), what follows the cursor on its line, and the lines
-- below. Above are the document's generated lines and what may stand
-- right above a block (a heading, a rule, a fence, a line of spaces).
genSlashCase :: Gen ([Text], Text, Text, [Text])
genSlashCase = do
  Doc doc <- arbitrary
  last' <- elements [[], ["# Title"], ["---"], ["```", "code", "```"], ["Text", ""], ["   "], ["Text"]]
  Line cursor <- arbitrary
  -- Before the line's own text: nothing, a no-break space, which is no
  -- indentation for the parser but is for CodeMirror's snippet; and words
  -- with a carriage return, which the toolbar's command leaves as it is.
  lead <- elements ["", "", "\160"]
  words' <- elements ["", "Some words ", "Some\rtext "]
  after' <- elements ["", "  "]
  below <- elements [[], [""], ["", "", "Next"], ["Next"], ["  ", "- item"], ["", "  "], "" : T.splitOn "\n" longText]
  longAbove <- elements [[], T.splitOn "\n" longText <> [""]]
  pure (longAbove <> doc <> last', lead <> cursor <> words', after', below)

-- | Wherever the menu opens on a generated place, each entry that writes
-- writes, in place of the cursor's line, what the toolbar's command
-- ('Edit.apply') writes on the whole text with the @/@ taken away and the
-- cursor where it was, and selects what it selects: the skeleton's text
-- (its places holding what the command writes there, a snippet's further
-- lines after the cursor line's indentation as CodeMirror writes them)
-- and its first place. The entry is worked out on the lines around the
-- cursor's alone ('Window'), so this is the check that they are enough.
slashWritesAsApply :: ([Text], Text, Text, [Text]) -> Property
slashWritesAsApply (above, typedBefore, after', below) = cover 50 (isJust opened) "the menu opens" $ case opened of
  Nothing -> property True
  Just found ->
    conjoin $
      -- With no letters, every entry: none is left out for a text its
      -- command could not be written in.
      (map (.label) found.candidates === map show slashCommands)
        : [ counterexample (toS (c.label <> ":\n" <> whole)) $ written c === Just (Edit.apply command whole (Edit.cursor at'))
          | c <- found.candidates
          , command <- [k | k <- slashCommands, show k == c.label]
          ]
  where
    opened = complete [slashSource slashEntries'] (readLine above) at
    at = At {place = InText, above, before = typedBefore <> "/", after = after', below}
    lineStart = sum (map ((+ 1) . T.length) above)
    at' = lineStart + T.length typedBefore
    whole = T.intercalate "\n" (above <> [typedBefore <> after'] <> below)
    -- Every entry is written as its text, in place of the line, its first
    -- place selected.
    written c = do
      let (text, ranges) = skeletonText c.skeleton
      text' <- case c.writing of
        LineAsText _ -> Just text
        _ -> Nothing
      (s, e) <- snd <$> head ranges
      Just (T.intercalate "\n" (above <> [text'] <> below), Edit.Selection (lineStart + s) (lineStart + e))

-- | A place where the @/@ menu opens (text of a paragraph, a list's item,
-- a quotation, a table's cell, a heading) or does not (code of both kinds,
-- math, a link's text and its address, HTML, an address, a word), marked,
-- and whether it opens.
genSlashPlace :: Gen (Text, Bool)
genSlashPlace = do
  word <- elements ["text", "Some words", "a"]
  letters <- elements ["", "ta", "b"]
  -- Before it, nothing, a paragraph, or a text longer than every bound;
  -- after it, nothing, a paragraph, or such a text.
  intro <- elements ["", "Intro.\n\n", longText <> "\n\n"]
  outro <- elements ["", "\n\nAfter.", "\n\n" <> longText]
  let at' = "/" <> letters <> "‸"
  (marked, opens) <-
    elements
      [ (word <> " " <> at', True)
      , ("- " <> word <> " " <> at', True)
      , ("1. " <> word <> " " <> at', True)
      , ("> " <> word <> " " <> at', True)
      , ("> [!NOTE]\n> " <> word <> " " <> at', True)
      , ("- " <> word <> " " <> at' <> "\n" <> longList, True)
      , ("> " <> word <> " " <> at' <> "\n>\n" <> longQuote, True)
      , ("> [!NOTE]\n> " <> word <> " " <> at' <> "\n>\n" <> longQuote, True)
      , (word <> " " <> at' <> "\n" <> longLines, False)
      , ("[[!spoiler]]{s}(More)\n" <> word <> " " <> at' <> "\n[[/spoiler]]", True)
      , ("[[!div]]\n" <> word <> " " <> at' <> "\n[[/div]]", True)
      , (at', True)
      , (word <> "\n" <> at', True)
      , (word <> " " <> at' <> " more", False)
      , ("`" <> word <> " " <> at' <> "`", False)
      , ("```\n" <> word <> " " <> at' <> "\n```", False)
      , ("    " <> word <> " " <> at', False)
      , ("$" <> word <> " " <> at' <> "$", False)
      , ("[" <> word <> " " <> at' <> "](https://example.com)", False)
      , ("*" <> word <> " " <> at' <> "*", False)
      , ("# " <> word <> " " <> at', False)
      , ("| a | b |\n| --- | --- |\n| " <> word <> " " <> at', False)
      , ("> [!NOTE] " <> word <> " " <> at', False)
      , ("[[!spoiler]]{s}(" <> word <> " " <> at', False)
      , ("[^n]: " <> word <> " " <> at', False)
      , ("<span title=\"" <> word <> " " <> at' <> "\">x</span>", False)
      , ("https://example.com" <> at', False)
      , (word <> at', False)
      ]
  pure (intro <> marked <> outro, opens)

-- | A list, a quotation's paragraphs and a paragraph's lines, each
-- longer than every bound of the completion.
longList, longQuote, longLines :: Text
longList = T.intercalate "\n" (replicate 60 "- Another item of the same long list")
longQuote = T.intercalate "\n>\n" (replicate 60 "> Another paragraph of the same quotation")
longLines = T.intercalate "\n" (replicate 60 "More words of a paragraph that goes on and on.")

-- | A text longer than every bound of the completion: forty paragraphs.
longText :: Text
longText = T.intercalate "\n\n" (replicate 40 (T.replicate 6 "Some more words. "))

----------------------------------------------------------------------
-- Generated documents

-- | A container a line is in: a quotation, with the spaces before its
-- mark and after it (0 to 3), or a list's item.
data Layer
  = -- | A quotation: the spaces before its mark, and after it the spaces
    -- or a tab.
    Quote Int (Either Int ())
  | -- | A list's item: the spaces before its mark, the mark, and after it
    -- one to three spaces or a tab.
    Item Int Text (Either Int ())
  | -- | A footnote's definition, its later lines indented four spaces.
    Footnote
  deriving stock (Eq, Show)

-- | A document: its lines.
newtype Doc = Doc [Text]
  deriving stock (Show)

-- | The start of a line, before what is typed on it: in containers, or a
-- lazy line, or none.
newtype Line = Line Text
  deriving stock (Show)

instance Arbitrary Doc where
  arbitrary = Doc . concat <$> (choose (0, 8) >>= \k -> replicateM k chunk)

-- | A cursor's line starts in up to three containers, or none (which is a
-- lazy line after a quotation's paragraph), sometimes after a no-break
-- space, which CodeMirror's snippet takes as indentation and the parser
-- as text.
instance Arbitrary Line where
  arbitrary = Line <$> ((<>) <$> frequency [(8, pure ""), (1, pure "\160")] <*> (prefix True <$> layers))

-- | Up to three containers, the outermost first.
layers :: Gen [Layer]
layers = do
  k <- choose (0, 3)
  replicateM k $
    frequency
      [ (3, Quote <$> choose (0, 3) <*> spacing)
      , (3, Item <$> choose (0, 3) <*> elements ["-", "*", "1.", "2)"] <*> spacing)
      , (1, pure Footnote)
      ]
  where
    spacing = frequency [(4, Left <$> choose (0, 3)), (1, pure (Right ()))]

-- | The text a line in these containers starts with: a quotation's mark
-- with its spaces or tab, an item's marker on its first line and its
-- indentation on the others, a footnote's label on its first line and four
-- spaces on the others.
prefix :: Bool -> [Layer] -> Text
prefix firstLine = foldMap $ \case
  Quote spaces gap -> T.replicate spaces " " <> ">" <> blanks gap
  Item spaces mark gap
    | firstLine -> T.replicate spaces " " <> mark <> either (\k -> T.replicate (max 1 k) " ") (const "\t") gap
    | otherwise -> T.replicate (spaces + T.length mark + either (max 1) (const 4) gap) " "
  Footnote -> if firstLine then "[^a]: " else "    "
  where
    blanks = either (`T.replicate` " ") (const "\t")

-- | Some lines of a document, in containers: a paragraph (with a lazy line
-- after it, sometimes), a marker of a block's start or end, indented code
-- holding one, a fence holding one, or a blank line.
chunk :: Gen [Text]
chunk = do
  ls <- layers
  let holding = filter holdsBlocks [minBound .. maxBound]
      opener name = if name == SpoilerName then "[[!spoiler]]{s}(A spoiler)" else "[[!" <> markerNameText name <> "]]"
  body <-
    frequency
      [ (3, pure ["Some text."])
      , (2, (\name -> [opener name]) <$> elements holding)
      , (2, (\name -> [closingMarker name]) <$> elements holding)
      , (1, (\name -> ["    " <> closingMarker name]) <$> elements holding)
      , (1, (\name -> ["    " <> opener name]) <$> elements holding)
      , (1, (\name -> ["```", closingMarker name, "```"]) <$> elements holding)
      , (2, pure [""])
      ]
  lazy <- frequency [(4, pure []), (1, pure ["lazy text"])]
  pure (zipWith (\i l -> prefix (i == (0 :: Int)) ls <> l) [0 ..] body <> [l | body == ["Some text."], l <- lazy])
