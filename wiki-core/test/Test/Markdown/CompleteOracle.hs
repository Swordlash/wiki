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
import Test.QuickCheck (Arbitrary (..), Gen, choose, counterexample, elements, frequency, (===))
import Wiki.Core.Markdown.Ast (Block (..), BoxMarker (..), MarkerLine (..), MarkerName (..), MarkerRole (..), closingMarker, holdsBlocks, markerNameText, parseWithAllMarkers)
import Wiki.Core.Markdown.Complete
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

----------------------------------------------------------------------
-- The completion, and the parser's reading

-- | The list at a cursor typed after the document's lines.
listAt :: [Text] -> Text -> Maybe Completion
listAt doc cursor = complete [wikiSource Presentation []] (readLine doc) At {place = InText, above = doc, before = cursor, after = ""}

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
    indentation = T.takeWhile (\c -> c == ' ' || c == '\t') cursor
    n = length doc + 1
    readsAs found c =
      let (written, _) = skeletonText c.skeleton
          further = T.splitOn "\n" written
          sentinel = "Zqxjv"
          lines' = case further of
            first' : rest -> (T.take found.from cursor <> first') : [indentation <> l <> (if T.isPrefixOf "[!" c.label then sentinel else "") | l <- rest]
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
-- lazy line after a quotation's paragraph).
instance Arbitrary Line where
  arbitrary = Line . prefix True <$> layers

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
