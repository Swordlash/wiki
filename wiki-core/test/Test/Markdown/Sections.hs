-- | A page's sections as places in its text ("Wiki.Core.Markdown.Sections"):
-- the headings of its table of contents, with the ids the rendered page
-- gives them, and the slices of the text they head, a block spoiler's
-- headings within the spoiler's body. These run on both backends.
module Test.Markdown.Sections (spec) where

import Data.Text qualified as T
import Protolude
import Test.Cases (coverage)
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (Gen, Property, choose, conjoin, counterexample, cover, elements, forAll, listOf, oneof, vectorOf, (.&&.), (===))
import Test.Slides (aDeckWithBreaks)
import Wiki.Core.Markdown.Ast (Block (..), SpoilerHead (..), SpoilerKind (..), SpoilerProblem (..), attribute, parseDeckSpans, plainText)
import Wiki.Core.Markdown.Sections

-- | A part of a text by its anchor, whole or its own part only.
part :: Text -> Text -> Bool -> Maybe Text
part source anchor own = sectionText source <$> sectionRange (outline source) anchor own

-- | The text before a text's first section.
topOf :: Text -> Text
topOf source = sectionText source (topRange (outline source))

-- | Every section of a text, in spoilers too, as its anchor, level and
-- text, in the order of the text.
headings :: Text -> [(Text, Int, Text)]
headings = map (\s -> (s.id, s.level, s.text)) . outlineSections . outline

-- | The headings of a text that are sections, read from the parsed tree
-- rather than from lines: those of the table of contents ("Wiki.Core.Markdown.Toc"),
-- but the ones in a block spoiler never closed, in the order of the text.
expected :: Text -> [(Text, Int, Text)]
expected = either (panic . show) (concatMap (walk . snd)) . parseDeckSpans
  where
    walk = \case
      Heading level attrs xs | Just i <- attribute "id" attrs -> [(i, level, plainText xs)]
      Spoiler h bs
        | h.kind == OnHeading || h.problem /= Just NotClosed -> concatMap walk bs
      _ -> []

-- | The outline of a text and those of its spoilers, at every depth.
everyOutline :: Outline -> [Outline]
everyOutline o = o : concatMap (\sb -> everyOutline sb.outline) (o.spoilers <> concatMap (\s -> s.spoilers) (everyOne o.sections))

-- | The sections of an outline, without those in its spoilers, in order.
everyOne :: [Section] -> [Section]
everyOne = concatMap (\s -> s : everyOne s.below)

spec :: Spec
spec = describe "Markdown.Sections" $ do
  describe "outline" $ do
    it "cuts a page at its headings, the top before the first" $ do
      let source = "Intro.\n\n# Title\n\nLead.\n\n## One\n\nA.\n\n### Deeper\n\nB.\n\n## Two\n\nC.\n"
      topOf source `shouldBe` "Intro.\n\n"
      part source "title" True `shouldBe` Just "# Title\n\nLead.\n\n"
      part source "title" False `shouldBe` Just (T.drop 8 source)
      part source "one" False `shouldBe` Just "## One\n\nA.\n\n### Deeper\n\nB.\n\n"
      part source "one" True `shouldBe` Just "## One\n\nA.\n\n"
      part source "deeper" False `shouldBe` Just "### Deeper\n\nB.\n\n"
      part source "two" False `shouldBe` Just "## Two\n\nC.\n"
      part source "three" False `shouldBe` Nothing
      outlineAnchors (outline source) `shouldBe` ["title", "one", "deeper", "two"]

    it "gives a page that starts with its heading an empty top" $
      topOf "# Title\n\nText." `shouldBe` ""

    it "gives a page with no heading its whole text as the top, and no sections" $ do
      outline "Just text.\n\n- a list\n" `shouldBe` Outline {from = 0, top = 21, to = 21, spoilers = [], sections = []}
      topOf "Just text.\n\n- a list\n" `shouldBe` "Just text.\n\n- a list\n"
      outline "" `shouldBe` Outline {from = 0, top = 0, to = 0, spoilers = [], sections = []}

    it "takes no # in a code block for a heading" $ do
      let source = "# A\n\n```\n# not a heading\n```\n\n    # nor indented\n\n## B\n"
      headings source `shouldBe` [("a", 1, "A"), ("b", 2, "B")]
      part source "a" True `shouldBe` Just "# A\n\n```\n# not a heading\n```\n\n    # nor indented\n\n"

    it "starts a setext heading's section at its title's first line" $ do
      let source = "Top.\n\nThe title\n=========\n\nText.\n\nA part\nof it\n------\n\nMore."
      headings source `shouldBe` [("the-title", 1, "The title"), ("a-part-of-it", 2, "A part of it")]
      part source "the-title" True `shouldBe` Just "The title\n=========\n\nText.\n\n"
      part source "a-part-of-it" False `shouldBe` Just "A part\nof it\n------\n\nMore."

    it "gives duplicate headings the distinct anchors the page gives them" $ do
      let source = "# Notes\n\nA.\n\n# Notes\n\nB.\n\n## Notes {#own}\n\nC.\n\n# Notes\n"
      map (\(i, _, _) -> i) (headings source) `shouldBe` ["notes", "notes-2", "own", "notes-3"]
      part source "notes-2" False `shouldBe` Just "# Notes\n\nB.\n\n## Notes {#own}\n\nC.\n\n"
      part source "notes-3" False `shouldBe` Just "# Notes\n"

    it "keeps a heading in a quotation, a list or a box in the section it is in, as the page's contents list does" $ do
      let source = "# A\n\n> ## Quoted\n\n- ## Listed\n\n[[!div]]{.box}\n## Boxed\n[[/div]]\n\n> [!NOTE]\n> ## Noted\n\n## B\n"
      headings source `shouldBe` [("a", 1, "A"), ("b", 2, "B")]
      expected source `shouldBe` [("a", 1, "A"), ("b", 2, "B")]
      part source "a" True `shouldBe` Just (T.dropEnd 5 source)

    it "takes a heading spoiler for a section like any other" $ do
      let source = "# A\n\n## [[!spoiler]] Question\n\nAnswer.\n\n### Why\n\nBecause.\n\n## Next\n"
      headings source `shouldBe` [("a", 1, "A"), ("question", 2, "Question"), ("why", 3, "Why"), ("next", 2, "Next")]
      part source "question" False `shouldBe` Just "## [[!spoiler]] Question\n\nAnswer.\n\n### Why\n\nBecause.\n\n"

    it "keeps a block spoiler whole in the section it is in, its headings an outline of its body" $ do
      let source = "# A\n\nLead.\n\n[[!spoiler]]{s}(More)\nFirst.\n\n## Hidden\n\nInside.\n\n### Deeper\n\nDeep.\n[[/spoiler]]{s}\n\nAfter.\n\n## Next\n\nText.\n"
      headings source `shouldBe` [("a", 1, "A"), ("hidden", 2, "Hidden"), ("deeper", 3, "Deeper"), ("next", 2, "Next")]
      -- The outer section's own part holds the whole spoiler, its markers
      -- too, and goes on after it to the next heading outside.
      part source "a" True `shouldBe` Just "# A\n\nLead.\n\n[[!spoiler]]{s}(More)\nFirst.\n\n## Hidden\n\nInside.\n\n### Deeper\n\nDeep.\n[[/spoiler]]{s}\n\nAfter.\n\n"
      -- A section inside ends at the end marker's line, or before.
      part source "hidden" False `shouldBe` Just "## Hidden\n\nInside.\n\n### Deeper\n\nDeep.\n"
      part source "hidden" True `shouldBe` Just "## Hidden\n\nInside.\n\n"
      part source "deeper" False `shouldBe` Just "### Deeper\n\nDeep.\n"
      part source "next" False `shouldBe` Just "## Next\n\nText.\n"
      case (outline source).sections of
        [a] -> do
          map (\sb -> sectionText source (topRange sb.outline)) a.spoilers `shouldBe` ["First.\n\n"]
          map (\s -> s.id) a.below `shouldBe` ["next"]
        other -> expectationFailure ("not one section: " <> show other)

    it "places the headings in a block spoiler by the one parse of the page, where a piece read alone would read otherwise" $ do
      -- In the page, the second end marker names no open spoiler: it is a
      -- marker error, and the --- after it a rule. Read alone, without the
      -- spoiler around it, the same lines are a setext heading.
      let source = "# A\n\n[[!spoiler]]{s}(More)\n## B\n\n[[/spoiler]]{t}\n---\n\n[[/spoiler]]{s}\n\n## C\n"
      headings source `shouldBe` expected source
      headings source `shouldBe` [("a", 1, "A"), ("b", 2, "B"), ("c", 2, "C")]
      part source "b" False `shouldBe` Just "## B\n\n[[/spoiler]]{t}\n---\n\n"
      part source "a" True `shouldBe` Just (T.dropEnd 5 source)

    it "places the headings of a spoiler in a spoiler, each within its own spoiler" $ do
      let source = "[[!spoiler]]{a}(A)\n# X\n[[!spoiler]]{b}(B)\n## Y\n[[/spoiler]]{b}\n### Z\n[[/spoiler]]{a}\n"
      headings source `shouldBe` [("x", 1, "X"), ("y", 2, "Y"), ("z", 3, "Z")]
      part source "y" False `shouldBe` Just "## Y\n"
      part source "x" True `shouldBe` Just "# X\n[[!spoiler]]{b}(B)\n## Y\n[[/spoiler]]{b}\n"
      part source "x" False `shouldBe` Just "# X\n[[!spoiler]]{b}(B)\n## Y\n[[/spoiler]]{b}\n### Z\n"
      topOf source `shouldBe` source

    it "makes no sections of the headings of a spoiler never closed" $ do
      let source = "# Top\n\n[[!spoiler]]{a}(A)\n## X\n\nStill inside."
      expected source `shouldBe` [("top", 1, "Top")]
      headings source `shouldBe` [("top", 1, "Top")]
      part source "top" True `shouldBe` Just source

    it "cuts a text with carriage returns at the lines the parser reads" $ do
      let source = "Top.\r\n\r\n# A\r\n\r\nText.\r\rB\r\n---\r\nMore.\r\n"
      headings source `shouldBe` [("a", 1, "A"), ("b", 2, "B")]
      topOf source `shouldBe` "Top.\r\n\r\n"
      -- Two carriage returns are two line breaks: a blank line.
      part source "a" True `shouldBe` Just "# A\r\n\r\nText.\r\r"
      part source "b" False `shouldBe` Just "B\r\n---\r\nMore.\r\n"

    it "names the first of two headings that have the same id written out" $
      part "# A {#x}\n\nOne.\n\n# B {#x}\n\nTwo." "x" False `shouldBe` Just "# A {#x}\n\nOne.\n\n"

    it "takes any id written out for a heading's anchor, ~top and top too" $ do
      let source = "Before.\n\n# One {id=\"~top\"}\n\nA.\n\n# Two {#top}\n\nB.\n"
      outlineAnchors (outline source) `shouldBe` ["~top", "top"]
      part source "~top" False `shouldBe` Just "# One {id=\"~top\"}\n\nA.\n\n"
      part source "top" False `shouldBe` Just "# Two {#top}\n\nB.\n"
      topOf source `shouldBe` "Before.\n\n"

  describe "on generated pages" $ do
    prop "makes sections of the headings of the table of contents, but those in a spoiler never closed" $
      forAll aPage $
        \source -> headings source === expected source
    prop "gives back the page from the top and the sections' own parts, in order" $
      coverage . forAll aPage $ \source ->
        cover 20 (T.any (== '\r') source) "a carriage return"
          . cover 20 ("[[!spoiler]]{s}(More)" `T.isInfixOf` source) "a block spoiler with a heading"
          . cover 30 ((>= 3) . length . outlineSections $ outline source) "three sections or more"
          $ partsGiveBack source
    prop "gives back a deck from the top and the sections' own parts, in order" $
      forAll aDeckWithBreaks $
        \source -> headings source === expected source .&&. partsGiveBack source
    prop "places every heading of spoilers among awkward neighbours, each within its spoiler's body" $
      coverage . forAll aSpoiledPage $ \source ->
        cover 15 (any (\(i, _, _) -> "in-" `T.isPrefixOf` i) (headings source)) "a section inside a spoiler"
          . cover 10 ("[[/spoiler]]{x" `T.isInfixOf` source) "an end marker of no open spoiler"
          . cover 10 (T.count "[[!spoiler]]" source > T.count "[[/spoiler]]{s" source) "a spoiler never closed"
          . counterexample (show (outline source))
          $ headings source === expected source
            .&&. partsGiveBack source
            .&&. wholeOfParts source
            .&&. spoilersWhole source
    prop "makes each whole section its own part and its subsections, in order" $
      forAll aPage wholeOfParts
    prop "starts each section with its heading" $
      forAll aPage $ \source ->
        conjoin
          [ counterexample (show (s.id, sectionText source (s.start, s.ownEnd))) $
              firstHeading (sectionText source (s.start, s.ownEnd)) === Just (s.level, s.text)
          | s <- outlineSections (outline source)
          ]
  where
    -- For the page and for each spoiler's body: its top and its sections'
    -- own parts, in order, are the whole text, the page or the body.
    partsGiveBack :: Text -> Property
    partsGiveBack source =
      conjoin
        [ counterexample (show o) $
            sectionText source (topRange o) <> T.concat [sectionText source (s.start, s.ownEnd) | s <- everyOne o.sections] === sectionText source (o.from, o.to)
        | o <- everyOutline (outline source)
        ]
        .&&. (let o = outline source in (o.from, o.to) === (0, T.length source))
    wholeOfParts :: Text -> Property
    wholeOfParts source =
      conjoin
        [ counterexample (show s.id) $
            sectionText source (s.start, s.end) === sectionText source (s.start, s.ownEnd) <> T.concat [sectionText source (c.start, c.end) | c <- s.below]
        | s <- outlineSections (outline source)
        ]
    -- Each spoiler with an outline is whole in the part around it: its
    -- start marker's line, then its body, then its end marker, all within
    -- the top or the own part of the section it is in.
    spoilersWhole :: Text -> Property
    spoilersWhole source =
      conjoin
        ( [ counterexample (show sb) $
              ("[[!spoiler]]" `T.isPrefixOf` T.stripStart (sectionText source (sb.start, sb.outline.from)))
                .&&. ("[[/spoiler]]" `T.isPrefixOf` T.stripStart (T.drop sb.outline.to source))
                .&&. (a <= sb.start && T.length (T.takeWhile (\c -> c /= '\n' && c /= '\r') (T.drop sb.outline.to source)) + sb.outline.to <= b)
          | o <- everyOutline (outline source)
          , (a, b, sbs) <- (o.from, o.top, o.spoilers) : [(s.start, s.ownEnd, s.spoilers) | s <- everyOne o.sections]
          , sb <- sbs
          ]
        )
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

-- | A page whose headings sit in block spoilers among the neighbours that
-- read differently out of the page: an end marker of no open spoiler,
-- with a @---@ or @===@ line right after it, a @---@ or @===@ right after
-- a start marker, a fence left open, a heading spoiler, a spoiler in a
-- spoiler, and a spoiler never closed. The headings inside spoilers have
-- ids that start with @in-@.
aSpoiledPage :: Gen Text
aSpoiledPage = do
  top <- elements ["", "Notes first.\n\n"]
  count <- choose (0, 6 :: Int)
  parts <- vectorOf count (oneof [outside, spoiler (2 :: Int) (0 :: Int)])
  pure (top <> T.intercalate "\n" parts)
  where
    outside = elements ["# Out", "## Out", "Text.", "Out\n---", "---", "[[/spoiler]]{x}\n---", "[[/spoiler]]{x}\n==="]
    inside depth k =
      oneof
        ( elements
            [ "## In {#in-a}"
            , "### In {#in-b}"
            , "In {#in-c}\n---"
            , "## [[!spoiler]] In {#in-e}\n\nAnswer."
            , "Text."
            , "---"
            , "==="
            , "[[/spoiler]]{x}"
            , "[[/spoiler]]{x}\n---"
            , "[[/spoiler]]{x}\n==="
            , "```\n## not a heading"
            ]
            : [spoiler (depth - 1) (k + 1) | depth > 0]
        )
    spoiler depth k = do
      let name = "s" <> show k
      next <- elements ["", "\n---", "\n===", "\n## In {#in-d}"]
      size <- choose (0, 4 :: Int)
      body <- vectorOf size (inside depth k)
      close <- elements ["\n[[/spoiler]]{" <> name <> "}", "\n[[/spoiler]]{" <> name <> "}", "\n[[/spoiler]]{" <> name <> "}\n---", "\n[[/spoiler]]{" <> name <> "}\n===", "\n[[/spoiler]]{" <> name <> "}", ""]
      pure ("[[!spoiler]]{" <> name <> "}(Show)" <> next <> "\n" <> T.intercalate "\n" body <> close)
