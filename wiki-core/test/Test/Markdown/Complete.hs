-- | The editor's completion of the wiki's own Markdown, written with the
-- cursor marked in the text: @‸@ where the cursor is. What choosing an
-- entry writes is shown with its first place to fill marked too, @«…»@
-- around what the place holds, or @‸@ for an empty one, as the editor
-- shows it after the choice.
module Test.Markdown.Complete (spec) where

import Data.Text qualified as T
import Protolude
import Test.Hspec
import Wiki.Core.Markdown.Ast (openingMarker)
import Wiki.Core.Markdown.Box (BoxColour (..), BoxIcon (..), BoxKind (..))
import Wiki.Core.Markdown.Complete
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
    it "opens on a list's item's first line, where the parser reads a marker" $
      typedAt "- [[!no‸" `shouldBe` Just (StartMarker, "no", 2)
    it "opens nothing in the middle of a line" $ do
      typedAt "Text [[!no‸" `shouldBe` Nothing
      typedAt "> Text [!no‸" `shouldBe` Nothing
    it "closes once a character no name has is typed" $ do
      typedAt "[[!no ‸" `shouldBe` Nothing
      typedAt "[[!notes]‸" `shouldBe` Nothing
      typedAt "[[!No‸" `shouldBe` Nothing
    it "opens nothing where the syntax tree says the text is its own" $
      for_ ["FencedCode", "CodeBlock", "InlineCode", "HTMLBlock", "CommentBlock", "TeXMath", "URL", "LinkReference"] $ \node ->
        (node, triggerAt (readLine []) (atIn [node, "Document"] "[[!no‸")) `shouldBe` (node, Nothing)

  describe "triggerAt, for a link to a page" $ do
    it "opens after [[ anywhere a wiki link is read, with what was typed after it" $ do
      typedAt "[[‸" `shouldBe` Just (LinkToPage, "", 0)
      typedAt "[[On‸" `shouldBe` Just (LinkToPage, "On", 0)
      typedAt "See [[on c‸" `shouldBe` Just (LinkToPage, "on c", 4)
      typedAt "> - [[Gu‸" `shouldBe` Just (LinkToPage, "Gu", 4)
    it "takes the last [[ of the line" $
      typedAt "[[a]] and [[b‸" `shouldBe` Just (LinkToPage, "b", 10)
    it "leaves [[! and [[/ to the markers: at a line's start a marker, elsewhere nothing" $ do
      typedAt "[[!no‸" `shouldBe` Just (StartMarker, "no", 0)
      typedAt "[[/sp‸" `shouldBe` Just (EndMarker, "sp", 0)
      typedAt "See [[!no‸" `shouldBe` Nothing
      -- An address from the top of the wiki: no end marker is read there.
      completeIn Document "See [[/docs‸" `shouldBe` Nothing
    it "opens nothing after a |, a closing bracket, or an escaped [[" $ do
      typedAt "[[guide|Gu‸" `shouldBe` Nothing
      typedAt "[[a]‸" `shouldBe` Nothing
      typedAt "\\[[a‸" `shouldBe` Nothing
      typedAt "\\\\[[a‸" `shouldBe` Just (LinkToPage, "a", 2)
    it "opens after an opening bracket, which the parser reads in an address" $
      typedAt "[[a[b‸" `shouldBe` Just (LinkToPage, "a[b", 0)
    it "opens nothing in code, math, HTML or a link's address" $
      for_ ["InlineCode", "FencedCode", "TeXMath", "HTMLTag", "URL"] $ \node ->
        (node, triggerAt (readLine []) (atIn [node, "Paragraph"] "[[gu‸")) `shouldBe` (node, Nothing)
    it "replaces the end of a link typed already, and keeps a label typed already" $ do
      endOf "[[On‸call]] now" `shouldBe` (6, "")
      endOf "[[On‸]]" `shouldBe` (2, "")
      endOf "[[On‸call|Who is on call]] now" `shouldBe` (T.length "call|Who is on call]]", "|Who is on call]]")
      endOf "[[‸|Label]]" `shouldBe` (T.length "|Label]]", "|Label]]")
    it "keeps a label the parser reads with a bracket or a character reference in it (#294)" $
      endOf "[[q‸-a|[draft&#93;]]" `shouldBe` (T.length "-a|[draft&#93;]]", "|[draft&#93;]]")
    it "reads the link at the cursor inside emphasis, and in a table's cell (#294, third round)" $ do
      endOf "**[[Gu‸ide]]**" `shouldBe` (T.length "ide]]", "")
      endOf "Text _a [[Gu‸ide|Mine]] b_" `shouldBe` (T.length "ide|Mine]]", "|Mine]]")
      endOf "| h | i |\n| --- | --- |\n| [[Gu‸ide]] | x |" `shouldBe` (T.length "ide]]", "")
    it "reads a link where what follows the cursor makes the line what it is: a spoiler's description, a box's title, a link's text (#294, fourth round)" $ do
      endOf "[[!spoiler]]{s}(See [[Gu‸ide]])" `shouldBe` (T.length "ide]]", "")
      endOf "[[!spoiler]]{s}(See [[Gu‸)" `shouldBe` (0, "")
      endOf "> [!NOTE] See [[Gu‸ide|it]] here" `shouldBe` (T.length "ide|it]]", "|it]]")
      endOf "[See [[Gu‸ide]] there](https://example.com)" `shouldBe` (T.length "ide]]", "")
    it "opens before a long text, where the block ends before the bound, and not in a block still open at it (#294, fifth round)" $ do
      let long = T.intercalate "\n\n" (replicate 40 (T.replicate 12 "Some words. "))
      endOf ("See [[Gu‸\n\n" <> long) `shouldBe` (0, "")
      endOf ("See [[Gu‸ide]] now\n\n" <> long) `shouldBe` (T.length "ide]]", "")
      endOf ("See [[Gu‸ " <> T.replicate 400 "words ") `shouldBe` (-1, "no list")
    it "writes a label after an escaped bar in a table's cell, and after a bar elsewhere (#294, third round)" $ do
      labelsOf "| h | i |\n| --- | --- |\n| [[Gu‸]] | x |" `shouldBe` Just EscapedLabel
      labelsOf "| h | i |\n| --- | --- |\n| [[Gu‸ | x |" `shouldBe` Just EscapedLabel
      labelsOf "See [[Gu‸]] now" `shouldBe` Just PlainLabel
      labelsOf "See **[[Gu‸** now" `shouldBe` Just PlainLabel
    it "replaces a link to its end on a line below, and keeps its label's lines (#294)" $ do
      endOf "[[Gu‸ide|read\nmore]] now" `shouldBe` (T.length "ide|read\nmore]]", "|read\nmore]]")
      endOf "[[Gu‸ide\nto it]] now" `shouldBe` (T.length "ide\nto it]]", "")
    it "replaces nothing after the cursor when the link has no end there" $ do
      endOf "[[On‸ and [[b]]" `shouldBe` (0, "")
      endOf "[[On‸call" `shouldBe` (0, "")
      endOf "[[On‸|a]b]]" `shouldBe` (0, "")

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
    it "offers the end of the innermost block open at the cursor, which alone an end marker there ends" $
      labels Presentation "[[!notes]]\n\n[[!div]]{.a}\n\nText\n\n[[/‸" `shouldBe` ["[[/div]]"]
    it "offers no end of an outer block, which the innermost keeps open" $
      labels Presentation "[[!notes]]\n[[!div]]\n[[/n‸" `shouldBe` []
    it "offers no end of a block its quotation closed" $
      labels Presentation "> [[!div]]\n> Text\n\n[[/‸" `shouldBe` []
    it "offers no block that is closed, nor a marker in code" $
      labels Document "[[!div]]\n[[/div]]\n[[!spoiler]]{a}(A)\n```\n[[!div]]\n```\n[[/‸" `shouldBe` ["[[/spoiler]]"]
    it "opens no list when no block is open" $
      completeIn Document "Text\n[[/‸" `shouldBe` Nothing
    it "writes the end marker" $
      written Document "[[!div]]\n[[/‸" `shouldBe` Just "[[!div]]\n[[/div]]‸"

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
        calendar wanted = [Candidate {label = "[[!day]]", skeleton = [Literal "[[!day]]"], place = Nothing, writing = AsSnippet, origin = Just (FromPlugin "calendar")} | wanted.typed.trigger == StartMarker]
    it "shows a plugin's origin as from the plugin" $
      originText (FromPlugin "calendar") `shouldBe` "from plugin calendar"
    it "lists every source's candidates, in the sources' order, each with its origin" $ do
      let found = complete [wikiSource Document [], calendar] (readLine []) (atIn [] "[[!‸")
      (map (\c -> (c.label, c.origin)) . (.candidates) <$> found)
        `shouldBe` Just [("[[!spoiler]]", Nothing), ("[[!div]]", Nothing), ("[[!toc]]", Nothing), ("[[!day]]", Just (FromPlugin "calendar"))]
    it "opens a list with one source's candidates when the others have none" $
      (map (.label) . (.candidates) <$> complete [wikiSource Document [], calendar] (readLine []) (atIn [] "[[!d‸"))
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
-- the lines before the cursor's, at the cursor's line, before the mark
-- and after it.
atIn :: [Text] -> Text -> At
atIn nodes marked =
  At
    { place = placeOf nodes
    , above = initDef [] ls
    , before = fromMaybe "" (lastMay ls)
    , after = T.takeWhile (/= '\n') (T.drop 1 rest)
    , below = drop 1 (T.splitOn "\n" (T.drop 1 rest))
    }
  where
    (upTo, rest) = T.breakOn "‸" marked
    ls = T.splitOn "\n" upTo

typedAt :: Text -> Maybe (Trigger, Text, Int)
typedAt marked = (\t -> (t.trigger, t.letters, t.from)) <$> triggerAt (readLine at.above) at
  where
    at = atIn [] marked

-- | How a link's entry writes a label of its own there.
labelsOf :: Text -> Maybe LabelForm
labelsOf marked = (.labels) <$> triggerAt (readLine at.above) at
  where
    at = atIn [] marked

-- | What a link's entry replaces after the cursor, and writes back of it.
endOf :: Text -> (Int, Text)
endOf marked = maybe (-1, "no list") (\t -> (t.through, t.kept)) (triggerAt (readLine at.above) at)
  where
    at = atIn [] marked

completeIn :: PageKind -> Text -> Maybe Completion
completeIn kind = completeWith kind []

completeWith :: PageKind -> [BoxKind] -> Text -> Maybe Completion
completeWith kind defined marked = complete [wikiSource kind defined] (readLine at.above) at
  where
    at = atIn [] marked

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
