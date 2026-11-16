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
import Test.QuickCheck (Arbitrary (..), Gen, Property, choose, counterexample, elements, frequency, oneof, (===))
import Wiki.Core.Markdown.Ast (BoxMarker (..), MarkerLine (..), MarkerName (..), MarkerRole (..), closingMarker, holdsBlocks, markerNameText, parseWithAllMarkers)
import Wiki.Core.Markdown.Complete
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

  modifyMaxSuccess (const 1000) $ do
    prop "offers a start marker exactly where the parser reads one" $ \(Doc doc) (Line cursor) ->
      counterexample (toS (T.unlines (doc <> [cursor <> "[[!‸"]))) $
        marker doc cursor === oracleMarker doc cursor
    prop "offers a kind of box exactly where the parser reads a box's marker" $ \(Doc doc) (Line cursor) ->
      counterexample (toS (T.unlines (doc <> [cursor <> "[!‸"]))) $
        box doc cursor === oracleBox doc cursor
    prop "offers the end markers of exactly the blocks an end marker there ends" $ \(Doc doc) (Line cursor) ->
      counterexample (toS (T.unlines (doc <> [cursor <> "[[/‸"]))) $
        sort (ends doc cursor) === sort (oracleEnds doc cursor)
    prop "writes a block's start marker with its end marker, one block for the parser" $ \(Doc doc) (Line cursor) ->
      writesOneBlock doc cursor

----------------------------------------------------------------------
-- The completion, and the parser's reading

-- | The list at a cursor typed after the document's lines.
listAt :: [Text] -> Text -> Maybe Completion
listAt doc cursor = complete [wikiSource Presentation []] (readLine doc) At {place = InText, above = doc, before = cursor}

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

-- | That the div's start marker, chosen where it is offered, is written
-- with its end marker so that the parser reads the two as one block: the
-- start marker opens a block that the end marker two lines below ends.
-- CodeMirror's snippet puts the cursor line's indentation before each
-- further line, as this does.
writesOneBlock :: [Text] -> Text -> Property
writesOneBlock doc cursor = case listAt doc (cursor <> "[[!di") of
  Just found
    | Just chosen <- find ((== "[[!div]]") . (.label)) found.candidates ->
        let (written, _) = skeletonText chosen.skeleton
            indentation = T.takeWhile (\c -> c == ' ' || c == '\t') cursor
            further = T.intercalate ("\n" <> indentation) (T.splitOn "\n" written)
            source = T.intercalate "\n" doc <> (if null doc then "" else "\n") <> T.take found.from (cursor <> "[[!di") <> further
            n = length doc + 1
            roles = case parseWithAllMarkers source of
              Right (_, _, lines', _) -> [(l, r) | MarkerLine l _ r <- lines', l == n || l == n + 2]
              Left _ -> []
         in counterexample (toS source) $
              (any (isOpens . snd) [x | x@(l, _) <- roles, l == n], [r | (l, r) <- roles, l == n + 2]) === (True, [Ends n])
  _ -> counterexample "not offered" True
  where
    isOpens = \case
      Opens _ -> True
      _ -> False

----------------------------------------------------------------------
-- Generated documents

-- | A container a line is in: a quotation, with the spaces before its
-- mark and after it (0 to 3), or a list's item.
data Layer = Quote Int Int | Item
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
  replicateM k (oneof [Quote <$> choose (0, 3) <*> choose (0, 3), pure Item])

-- | The text a line in these containers starts with: a quotation's mark
-- with its spaces, an item's marker on its first line and its indentation
-- on the others.
prefix :: Bool -> [Layer] -> Text
prefix firstLine = foldMap $ \case
  Quote spaces spacesAfter -> T.replicate spaces " " <> ">" <> T.replicate spacesAfter " "
  Item -> if firstLine then "- " else "  "

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
