-- | A page's sections as places in its text ("Wiki.Core.Markdown.Sections"):
-- the headings of its table of contents, with the ids the rendered page
-- gives them, and the slices of the text they head. These run on both
-- backends.
module Test.Markdown.Sections (spec) where

import Data.Text qualified as T
import Protolude
import Test.Cases (coverage)
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (Gen, Property, conjoin, counterexample, cover, elements, forAll, listOf, vectorOf, (===))
import Test.Slides (aDeckWithBreaks)
import Wiki.Core.Markdown.Ast (Block (..), SpoilerHead (..), SpoilerKind (..), parseDeckSpans, parseMarkdown, plainText)
import Wiki.Core.Markdown.Sections
import Wiki.Core.Markdown.Toc (Entry (..), entryCount, tableOfContents)

-- | The outline of a text that has one.
outlined :: Text -> Outline
outlined source = fromMaybe (panic ("no outline of " <> show source)) (outline source)

-- | A part of a text by its anchor, whole or its own part only.
part :: Text -> Text -> Bool -> Maybe Text
part source anchor own = sectionText source <$> sectionRange (outlined source) anchor own

-- | Each section as its anchor, level and text, at every depth, in order.
headings :: [Section] -> [(Text, Int, Text)]
headings = concatMap (\s -> (s.id, s.level, s.text) : headings s.below)

-- | The sections at every depth, in order.
everyOne :: [Section] -> [Section]
everyOne = concatMap (\s -> s : everyOne s.below)

spec :: Spec
spec = describe "Markdown.Sections" $ do
  describe "outline" $ do
    it "cuts a page at its headings, the top before the first" $ do
      let source = "Intro.\n\n# Title\n\nLead.\n\n## One\n\nA.\n\n### Deeper\n\nB.\n\n## Two\n\nC.\n"
      part source topAnchor False `shouldBe` Just "Intro.\n\n"
      part source "title" True `shouldBe` Just "# Title\n\nLead.\n\n"
      part source "title" False `shouldBe` Just (T.drop 8 source)
      part source "one" False `shouldBe` Just "## One\n\nA.\n\n### Deeper\n\nB.\n\n"
      part source "one" True `shouldBe` Just "## One\n\nA.\n\n"
      part source "deeper" False `shouldBe` Just "### Deeper\n\nB.\n\n"
      part source "two" False `shouldBe` Just "## Two\n\nC.\n"
      part source "three" False `shouldBe` Nothing
      outlineAnchors (outlined source) `shouldBe` [topAnchor, "title", "one", "deeper", "two"]

    it "gives a page that starts with its heading an empty top" $
      part "# Title\n\nText." topAnchor False `shouldBe` Just ""

    it "gives a page with no heading its whole text as the top, and no sections" $ do
      outlined "Just text.\n\n- a list\n" `shouldBe` Outline {top = 21, sections = []}
      part "Just text.\n\n- a list\n" topAnchor False `shouldBe` Just "Just text.\n\n- a list\n"
      outlined "" `shouldBe` Outline {top = 0, sections = []}

    it "takes no # in a code block for a heading" $ do
      let source = "# A\n\n```\n# not a heading\n```\n\n    # nor indented\n\n## B\n"
      headings (outlined source).sections `shouldBe` [("a", 1, "A"), ("b", 2, "B")]
      part source "a" True `shouldBe` Just "# A\n\n```\n# not a heading\n```\n\n    # nor indented\n\n"

    it "starts a setext heading's section at its title's first line" $ do
      let source = "Top.\n\nThe title\n=========\n\nText.\n\nA part\nof it\n------\n\nMore."
      headings (outlined source).sections `shouldBe` [("the-title", 1, "The title"), ("a-part-of-it", 2, "A part of it")]
      part source "the-title" True `shouldBe` Just "The title\n=========\n\nText.\n\n"
      part source "a-part-of-it" False `shouldBe` Just "A part\nof it\n------\n\nMore."

    it "gives duplicate headings the distinct anchors the page gives them" $ do
      let source = "# Notes\n\nA.\n\n# Notes\n\nB.\n\n## Notes {#own}\n\nC.\n\n# Notes\n"
      map (\(i, _, _) -> i) (headings (outlined source).sections) `shouldBe` ["notes", "notes-2", "own", "notes-3"]
      part source "notes-2" False `shouldBe` Just "# Notes\n\nB.\n\n## Notes {#own}\n\nC.\n\n"
      part source "notes-3" False `shouldBe` Just "# Notes\n"

    it "keeps a heading in a quotation, a list or a box in the section it is in, as the page's contents list does" $ do
      let source = "# A\n\n> ## Quoted\n\n- ## Listed\n\n[[!div]]{.box}\n## Boxed\n[[/div]]\n\n> [!NOTE]\n> ## Noted\n\n## B\n"
      headings (outlined source).sections `shouldBe` [("a", 1, "A"), ("b", 2, "B")]
      map (\e -> e.id) (tableOfContents (either (panic . show) identity (parseMarkdown source))) `shouldBe` ["a"]
      part source "a" True `shouldBe` Just (T.dropEnd 5 source)

    it "lists a heading inside a spoiler, as the page's contents list does" $ do
      let source = "# A\n\n## [[!spoiler]] Question\n\nAnswer.\n\n### Why\n\nBecause.\n\n[[!spoiler]]{s}(More)\n## Hidden\n\nInside.\n[[/spoiler]]{s}\n\nAfter.\n"
      headings (outlined source).sections `shouldBe` [("a", 1, "A"), ("question", 2, "Question"), ("why", 3, "Why"), ("hidden", 2, "Hidden")]
      part source "question" False `shouldBe` Just "## [[!spoiler]] Question\n\nAnswer.\n\n### Why\n\nBecause.\n\n[[!spoiler]]{s}(More)\n"
      part source "hidden" False `shouldBe` Just "## Hidden\n\nInside.\n[[/spoiler]]{s}\n\nAfter.\n"

    it "cuts a text with carriage returns at the lines the parser reads" $ do
      let source = "Top.\r\n\r\n# A\r\n\r\nText.\r\rB\r\n---\r\nMore.\r\n"
      headings (outlined source).sections `shouldBe` [("a", 1, "A"), ("b", 2, "B")]
      part source topAnchor False `shouldBe` Just "Top.\r\n\r\n"
      -- Two carriage returns are two line breaks: a blank line.
      part source "a" True `shouldBe` Just "# A\r\n\r\nText.\r\r"
      part source "b" False `shouldBe` Just "B\r\n---\r\nMore.\r\n"

    it "names the first of two headings that have the same id written out" $
      part "# A {#x}\n\nOne.\n\n# B {#x}\n\nTwo." "x" False `shouldBe` Just "# A {#x}\n\nOne.\n\n"

  describe "on generated pages" $ do
    prop "lists the headings of the page's table of contents, with their ids" $
      forAll aPage $ \source -> withOutline source $ \o ->
        let entries = tableOfContents (either (panic . show) identity (parseMarkdown source))
         in headings o.sections === flatten entries
    prop "gives back the page from the top and the sections' own parts, in order" $
      coverage . forAll aPage $ \source ->
        cover 20 (T.any (== '\r') source) "a carriage return"
          . cover 20 ("[[!spoiler]]{s}(More)" `T.isInfixOf` source) "a block spoiler with a heading"
          . cover 40 (maybe False ((>= 3) . length . everyOne . (.sections)) (outline source)) "three sections or more"
          $ pageFromParts source
    prop "gives back a deck from the top and the sections' own parts, in order" $
      forAll aDeckWithBreaks pageFromParts
    prop "makes each whole section its own part and its subsections, in order" $
      forAll aPage $ \source -> withOutline source $ \o ->
        conjoin
          [ counterexample (show s.id) $
              sectionText source (s.start, s.end) === sectionText source (s.start, s.ownEnd) <> T.concat [sectionText source (c.start, c.end) | c <- s.below]
          | s <- everyOne o.sections
          ]
    prop "starts each section with its heading" $
      forAll aPage $ \source -> withOutline source $ \o ->
        conjoin
          [ counterexample (show (s.id, sectionText source (s.start, s.ownEnd))) $
              firstHeading (sectionText source (s.start, s.ownEnd)) === Just (s.level, s.text)
          | s <- everyOne o.sections
          ]
  where
    flatten = concatMap (\e -> (e.id, e.level, e.text) : flatten e.below)
    withOutline :: Text -> (Outline -> Property) -> Property
    withOutline source continue = case outline source of
      Nothing -> counterexample "no outline" False
      Just o -> continue o
    pageFromParts source = withOutline source $ \o ->
      counterexample (show o) $
        (length (everyOne o.sections), sectionText source (0, o.top) <> T.concat [sectionText source (s.start, s.ownEnd) | s <- everyOne o.sections])
          === (entryCount (tableOfContents (either (panic . show) identity (parseMarkdown source))), source)
    -- The level and the text of the heading a text starts with, read as
    -- the page reads its top level.
    firstHeading t = case parseDeckSpans t of
      Right ((_, b) : _) -> case b of
        Heading level _ xs -> Just (level, plainText xs)
        Spoiler h (Heading level _ xs : _) | h.kind == OnHeading -> Just (level, plainText xs)
        _ -> Nothing
      _ -> Nothing

-- | A page to cut into sections: headings of every level, written both
-- ways, some the same; blocks that hold a heading that is no section (a
-- quotation, a list, a box), and code with a @#@ in it; spoilers of both
-- kinds; with or without text before the first heading, its pieces apart
-- by one to three line breaks, and its line breaks line feeds, carriage
-- returns or both.
aPage :: Gen Text
aPage = do
  top <- elements ["", "Notes first.\n\n", "\n"]
  pieces <- listOf ((,) <$> piece <*> elements ["\n", "\n\n", "\n\n\n"])
  end <- elements ["", "\n"]
  let source = top <> T.concat [p <> sep | (p, sep) <- pieces] <> end
      ls = T.splitOn "\n" source
  endings <- vectorOf (length ls - 1) (elements ["\n", "\n", "\n", "\r\n", "\r"])
  pure (T.concat (zipWith (<>) ls (endings <> [""])))
  where
    piece :: Gen Text
    piece = do
      word <- elements ["Alpha", "Beta", "Alpha beta"]
      hashes <- elements ["#", "##", "###", "####"]
      elements
        [ hashes <> " " <> word
        , word <> "\n" <> "==="
        , word <> "\n" <> "---"
        , "Text of " <> word <> "."
        , "```\n" <> hashes <> " " <> word <> "\n```"
        , "    " <> hashes <> " indented"
        , "> " <> hashes <> " " <> word
        , "- " <> hashes <> " " <> word
        , "[[!div]]{.box}\n" <> hashes <> " " <> word <> "\n[[/div]]"
        , hashes <> " [[!spoiler]] " <> word <> "\n\nAnswer."
        , "[[!spoiler]]{s}(More)\n" <> hashes <> " " <> word <> "\n\nInside.\n[[/spoiler]]{s}"
        , "[[!spoiler]]{t}(Open)\n" <> hashes <> " " <> word
        , "---"
        , "[[!toc]]"
        ]
