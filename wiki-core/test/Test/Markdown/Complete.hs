-- | The editor's completion of the wiki's own Markdown, written with the
-- cursor marked in the text: @‸@ where the cursor is. What choosing an
-- entry writes is shown with its first place to fill marked too, @«…»@
-- around what the place holds, or @‸@ for an empty one, as the editor
-- shows it after the choice.
module Test.Markdown.Complete (spec) where

import Data.Text qualified as T
import Protolude
import Test.Hspec
import Test.Hspec.QuickCheck (modifyMaxSuccess, prop)
import Test.QuickCheck (Gen, Property, choose, counterexample, elements, forAll, frequency, listOf, sized, (===))
import Wiki.Core.Markdown.Ast (MarkerName (..), closingMarker, holdsBlocks, openingMarker)
import Wiki.Core.Markdown.Box (BoxColour (..), BoxIcon (..), BoxKind (..))
import Wiki.Core.Markdown.Complete
import Wiki.Core.Markdown.Problems (Checked (..), Problem (..), checkText)
import Wiki.Core.PageKind (PageKind (..))

spec :: Spec
spec = describe "Wiki.Core.Markdown.Complete" $ do
  describe "triggerAt" $ do
    it "opens after [[! at a line's start, with the letters typed" $ do
      typedAt "[[!no‸" `shouldBe` Just (StartMarker, "no", 0)
      typedAt "[[!‸" `shouldBe` Just (StartMarker, "", 0)
    it "opens after [[/ at a line's start" $
      typedAt "[[/sp‸" `shouldBe` Just (EndMarker, "sp", 0)
    it "takes a line's start after its indentation and its quotations' >" $ do
      typedAt "   [[!‸" `shouldBe` Just (StartMarker, "", 3)
      typedAt "> [[!di‸" `shouldBe` Just (StartMarker, "di", 2)
      typedAt ">> [[/‸" `shouldBe` Just (EndMarker, "", 3)
    it "opens after [! on a quotation's first line" $ do
      typedAt "> [!ti‸" `shouldBe` Just (BoxMarker, "ti", 2)
      typedAt ">[!‸" `shouldBe` Just (BoxMarker, "", 1)
      typedAt "Text\n> [!Ex‸" `shouldBe` Just (BoxMarker, "Ex", 2)
      typedAt "> Outer\n> > [!‸" `shouldBe` Just (BoxMarker, "", 4)
    it "opens no box after [! on a quotation's later line, or outside a quotation" $ do
      typedAt "> Text\n> [!no‸" `shouldBe` Nothing
      typedAt "[!no‸" `shouldBe` Nothing
    it "opens nothing in the middle of a line" $ do
      typedAt "Text [[!no‸" `shouldBe` Nothing
      typedAt "> Text [!no‸" `shouldBe` Nothing
      typedAt "- [[!no‸" `shouldBe` Nothing
    it "closes once a character no name has is typed" $ do
      typedAt "[[!no ‸" `shouldBe` Nothing
      typedAt "[[!notes]‸" `shouldBe` Nothing
      typedAt "[[!No‸" `shouldBe` Nothing
    it "opens nothing where the syntax tree says the text is its own" $
      for_ ["FencedCode", "CodeBlock", "InlineCode", "HTMLBlock", "CommentBlock", "TeXMath", "URL", "LinkReference"] $ \node ->
        (node, triggerAt (atIn [node, "Document"] "[[!no‸")) `shouldBe` (node, Nothing)

  describe "placeOf" $ do
    it "is text in a paragraph, a quotation, a list and a link's text" $
      placeOf ["Link", "Paragraph", "Blockquote", "ListItem", "BulletList", "Document"] `shouldBe` InText
    it "is the text's own in code, HTML, a comment, math and a link's address" $
      for_ [["CodeText", "FencedCode", "Document"], ["HTMLBlock", "Document"], ["URL", "Link", "Paragraph"], ["TeXMath", "Paragraph"]] $ \nodes ->
        placeOf nodes `shouldBe` InOwnText

  describe "the markers offered" $ do
    it "offers a document the markers a document reads" $
      labels Document "[[!‸" `shouldBe` ["[[!spoiler]]", "[[!div]]", "[[!toc]]"]
    it "offers a presentation every marker" $
      labels Presentation "[[!‸" `shouldBe` map openingMarker [minBound .. maxBound]
    it "offers the markers whose names start with the letters" $ do
      labels Presentation "[[!no‸" `shouldBe` ["[[!notes]]"]
      labels Presentation "[[!s‸" `shouldBe` ["[[!spoiler]]", "[[!style]]"]
    it "offers a page of a kind it does not know what it offers a document" $
      labels (OtherKind "board") "[[!‸" `shouldBe` labels Document "[[!‸"
    it "opens no list when no marker has the letters, or the page's kind reads none of them" $ do
      completeIn Presentation "[[!x‸" `shouldBe` Nothing
      completeIn Document "[[!no‸" `shouldBe` Nothing

  describe "what a marker writes" $ do
    it "writes a block with its end marker, the cursor between them" $
      written Presentation "[[!no‸" `shouldBe` Just "[[!notes]]\n‸\n[[/notes]]"
    it "writes a spoiler with its name to fill first, then its description and its text" $ do
      written Document "[[!sp‸" `shouldBe` Just "[[!spoiler]]{«name»}(Description)\n\n[[/spoiler]]"
      places Document "[[!sp‸" `shouldBe` Just ["name", "Description", ""]
    it "writes a one-line marker alone" $ do
      written Document "[[!to‸" `shouldBe` Just "[[!toc]]‸"
      written Presentation "[[!pa‸" `shouldBe` Just "[[!pause]]‸"
    it "writes a presentation's look with its theme to fill" $
      written Presentation "[[!sty‸" `shouldBe` Just "[[!style]]{theme=«dark»}"
    it "keeps a block written in a quotation in it" $
      written Presentation "> [[!di‸" `shouldBe` Just "> [[!div]]\n> ‸\n> [[/div]]"
    it "replaces what was typed, from the trigger to the cursor" $
      (completeIn Document "  [[!di‸" <&> (.from)) `shouldBe` Just 2

  describe "the end markers offered" $ do
    it "offers the blocks open at the cursor, the innermost first" $
      labels Presentation "[[!notes]]\n\n[[!div]]{.a}\n\nText\n\n[[/‸" `shouldBe` ["[[/div]]", "[[/notes]]"]
    it "offers those whose names start with the letters" $
      labels Presentation "[[!notes]]\n[[!div]]\n[[/n‸" `shouldBe` ["[[/notes]]"]
    it "offers no block that is closed, nor a marker in code" $
      labels Document "[[!div]]\n[[/div]]\n[[!spoiler]]{a}(A)\n```\n[[!div]]\n```\n[[/‸" `shouldBe` ["[[/spoiler]]"]
    it "opens no list when no block is open" $
      completeIn Document "Text\n[[/‸" `shouldBe` Nothing
    it "writes the end marker" $
      written Document "[[!div]]\n[[/‸" `shouldBe` Just "[[!div]]\n[[/div]]‸"

  describe "openBlocks" $ do
    it "reads an end marker that ends no open block of its name as ending nothing" $
      openBlocks ["[[!notes]]", "[[!div]]", "[[/notes]]"] `shouldBe` [DivName, NotesName]
    it "takes no one-line marker as a block" $
      openBlocks ["[[!toc]]", "[[!pause]]", "[[!style]]{theme=dark}"] `shouldBe` []
    it "skips a fence's lines, a fence of tildes, and a longer fence's shorter closing" $
      openBlocks ["~~~~", "[[!div]]", "~~~", "[[!notes]]", "~~~~", "[[!spoiler]]{a}(A)"] `shouldBe` [SpoilerName]
    it "reads markers in a quotation" $
      openBlocks ["> [[!div]]", "> Text"] `shouldBe` [DivName]
    modifyMaxSuccess (const 2000) . prop "is the blocks the lines leave open, by a model of them" $
      forAll genBlocks $
        \bs -> openBlocks (blockLines bs) === modelOpen bs

  describe "the end marker written, by the wiki's parser"
    $ modifyMaxSuccess (const 500) . prop "closes the innermost open block, and is no problem of its own"
    $ forAll genBlocks closesInnermost

  describe "the kinds of box offered" $ do
    let exercise = BoxKind {name = "exercise", label = "Exercise", colour = Teal, icon = Pencil}
    it "offers GitHub's five, then the kinds defined" $
      labelsWith Document [exercise] "> [!‸" `shouldBe` ["[!NOTE]", "[!TIP]", "[!IMPORTANT]", "[!WARNING]", "[!CAUTION]", "[!EXERCISE]"]
    it "offers the kinds whose names start with the letters, whatever their case" $ do
      labelsWith Document [exercise] "> [!ex‸" `shouldBe` ["[!EXERCISE]"]
      labelsWith Document [exercise] "> [!Ti‸" `shouldBe` ["[!TIP]"]
    it "writes the box's marker and its first line of text, the cursor there" $ do
      written Document "> [!wa‸" `shouldBe` Just "> [!WARNING]\n> ‸"
      written Document "> Outer\n> > [!no‸" `shouldBe` Just "> Outer\n> > [!NOTE]\n> > ‸"

  describe "sources and origins" $ do
    let calendar :: Source
        calendar wanted = [Candidate {label = "[[!day]]", skeleton = [Literal "[[!day]]"], origin = Just (FromPlugin "calendar")} | wanted.typed.trigger == StartMarker]
    it "shows a plugin's origin as from the plugin" $
      originText (FromPlugin "calendar") `shouldBe` "from plugin calendar"
    it "lists every source's candidates, in the sources' order, each with its origin" $ do
      let found = complete [wikiSource Document [], calendar] (atIn [] "[[!‸")
      (map (\c -> (c.label, c.origin)) . (.candidates) <$> found)
        `shouldBe` Just [("[[!spoiler]]", Nothing), ("[[!div]]", Nothing), ("[[!toc]]", Nothing), ("[[!day]]", Just (FromPlugin "calendar"))]
    it "opens a list with one source's candidates when the others have none" $
      (map (.label) . (.candidates) <$> complete [wikiSource Document [], calendar] (atIn [] "[[!d‸"))
        `shouldBe` Just ["[[!div]]", "[[!day]]"]
    it "chooses with Enter at once and closes with Escape" $
      ((\c -> (c.enter, c.escape)) <$> completeIn Document "[[!‸") `shouldBe` Just (EnterChooses, EscapeCloses)

  describe "snippetTemplate" $ do
    it "writes a place with what it holds, and an empty place" $
      snippetTemplate [Literal "a", Field 1 "name", Literal "b", Field 2 "", Field 0 ""] `shouldBe` "a${1:name}b${2}${0}"
    it "writes a brace with a backslash before it" $
      snippetTemplate [Literal "[[!spoiler]]{", Field 1 "name", Literal "}"] `shouldBe` "[[!spoiler]]\\{${1:name}\\}"
    it "keeps ${ and #{ in the text from starting a place" $
      snippetTemplate [Literal "${x} #{y}"] `shouldBe` "$\\{x\\} #\\{y\\}"
    it "keeps TeX's own backslashes, the brace after one escaped too" $
      snippetTemplate [Literal "\\{a\\} \\frac{1}{2}"] `shouldBe` "\\\\{a\\\\} \\frac\\{1\\}\\{2\\}"
    it "gives the text and the places' ranges, place 0 last" $
      skeletonText [Literal "ab", Field 0 "", Field 2 "x", Field 1 "yz"] `shouldBe` ("abxyz", [(1, (3, 5)), (2, (2, 3)), (0, (2, 2))])

-- | Where the cursor of a marked text is, for 'triggerAt': in text, under
-- the lines before the cursor's, at the cursor's line up to the mark.
atIn :: [Text] -> Text -> At
atIn nodes marked = At {place = placeOf nodes, above = initDef [] ls, before = fromMaybe "" (lastMay ls)}
  where
    ls = T.splitOn "\n" (fst (T.breakOn "‸" marked))

typedAt :: Text -> Maybe (Trigger, Text, Int)
typedAt marked = (\t -> (t.trigger, t.letters, t.from)) <$> triggerAt (atIn [] marked)

completeIn :: PageKind -> Text -> Maybe Completion
completeIn kind = completeWith kind []

completeWith :: PageKind -> [BoxKind] -> Text -> Maybe Completion
completeWith kind defined marked = complete [wikiSource kind defined] (atIn [] marked)

labels :: PageKind -> Text -> [Text]
labels kind = labelsWith kind []

labelsWith :: PageKind -> [BoxKind] -> Text -> [Text]
labelsWith kind defined marked = maybe [] (map (.label) . (.candidates)) (completeWith kind defined marked)

-- | The text after choosing the first candidate, with its first place
-- marked ('skeletonText' of the skeleton put in place of what was typed;
-- the texts here have no indentation, which CodeMirror would add to the
-- skeleton's further lines).
written :: PageKind -> Text -> Maybe Text
written kind marked = do
  c <- completeIn kind marked
  chosen <- head c.candidates
  let (upTo, rest) = T.breakOn "‸" marked
      lineStart = T.length upTo - T.length (T.takeWhileEnd (/= '\n') upTo)
      at = lineStart + c.from
      (text, ranges) = skeletonText chosen.skeleton
      shown = case head ranges of
        Just (_, (s, e))
          | s == e -> T.take s text <> "‸" <> T.drop s text
          | otherwise -> T.take s text <> "«" <> T.take (e - s) (T.drop s text) <> "»" <> T.drop e text
        Nothing -> text
  Just (T.take at upTo <> shown <> T.drop 1 rest)

-- | What the first candidate's places hold, in their order.
places :: PageKind -> Text -> Maybe [Text]
places kind marked = do
  c <- completeIn kind marked
  chosen <- head c.candidates
  let (text, ranges) = skeletonText chosen.skeleton
  Just [T.take (e - s) (T.drop s text) | (n, (s, e)) <- ranges, n /= 0]

----------------------------------------------------------------------
-- Generated nestings of blocks

-- | A block of a generated text: one that opens (and holds the blocks
-- after it until it is closed), the end of the innermost open block, a
-- paragraph, or a fenced code block whose lines look like markers.
data Block = Opens MarkerName | Closes | Paragraph | Fenced [MarkerName]
  deriving stock (Eq, Show)

-- | Blocks whose ends close only blocks open: a model of a text an author
-- writes, with blocks left open at its end.
genBlocks :: Gen [Block]
genBlocks = sized $ \size -> do
  n <- choose (0, min 20 size)
  go n (0 :: Int)
  where
    holding = filter holdsBlocks [minBound .. maxBound]
    go 0 _ = pure []
    go n depth = do
      b <-
        frequency
          ( [(3, Opens <$> elements holding), (2, pure Paragraph), (1, Fenced <$> listOf (elements holding))]
              <> [(3, pure Closes) | depth > 0]
          )
      let depth' = case b of
            Opens _ -> depth + 1
            Closes -> depth - 1
            _ -> depth
      (b :) <$> go (n - 1 :: Int) depth'

-- | The lines of the blocks, a blank line between two blocks. A spoiler
-- has a name and a description, as it must.
blockLines :: [Block] -> [Text]
blockLines bs = intercalate [""] (snd (mapAccumL line [] (zip [0 :: Int ..] bs)))
  where
    line open (i, b) = case b of
      Opens SpoilerName -> (SpoilerName : open, ["[[!spoiler]]{s" <> show i <> "}(A spoiler)"])
      Opens name -> (name : open, [openingMarker name])
      Closes -> (drop 1 open, [maybe "" closingMarker (head open)])
      Paragraph -> (open, ["Some text."])
      Fenced names -> (open, ["```"] <> map openingMarker names <> ["```"])

-- | The blocks left open, innermost first, by the blocks' own count.
modelOpen :: [Block] -> [MarkerName]
modelOpen = foldl' step []
  where
    step open = \case
      Opens name -> name : open
      Closes -> drop 1 open
      _ -> open

-- | That the end marker the completion writes after the blocks, on a line
-- of its own, closes the innermost open block for the wiki's parser: one
-- fewer block is "not closed", and the line itself has no problem.
closesInnermost :: [Block] -> Property
closesInnermost bs =
  case modelOpen bs of
    [] -> completeIn Presentation (text <> "\n\n[[/‸") === Nothing
    _ ->
      let marked = text <> "\n\n[[/‸"
          completed = fromMaybe "" (T.replace "‸" "" <$> written Presentation marked)
          lineCount = length (T.splitOn "\n" completed)
          problems source = (checkText Presentation source).problems
          notClosed source = length [p | p <- problems source, "not closed" `T.isInfixOf` p.message]
          onLine = [p | p <- problems completed, p.from <= lineCount, p.to >= lineCount]
       in counterexample (toS completed) $
            (onLine, notClosed completed) === ([], notClosed text - 1)
  where
    text = T.intercalate "\n" (blockLines bs)
