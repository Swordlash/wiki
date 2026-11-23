-- | A page's sections as places in its text ("Wiki.Core.Markdown.Sections"):
-- the headings of its table of contents, with the ids the rendered page
-- gives them, and the slices of the text they head, a block spoiler's
-- headings within the spoiler's body. These run on both backends.
module Test.Markdown.Sections (spec) where

import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Protolude
import Test.Cases (coverage)
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (Gen, Property, choose, conjoin, counterexample, cover, elements, forAll, frequency, listOf, oneof, property, tabulate, vectorOf, (.&&.), (===))
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

  describe "replaceSection" $ do
    let page = "# A\n\nLead.\n\n## Install\n\nSteps.\n\n## Use\n\nRun it.\n"
    it "ends a body with a line break, and adds no empty line where none is needed" $ do
      replaceSection (WholeSection "use") page "## Use\n\nRun it twice."
        `shouldBe` Right Replaced {text = "# A\n\nLead.\n\n## Install\n\nSteps.\n\n## Use\n\nRun it twice.\n", anchor = Just "use"}
      replaceSection (WholeSection "install") page "## Install\n\nOther steps."
        `shouldBe` Right Replaced {text = "# A\n\nLead.\n\n## Install\n\nOther steps.\n## Use\n\nRun it.\n", anchor = Just "install"}

    it "adds an empty line where the body's last paragraph would run into a title underlined after it" $ do
      let source = "## One\n\nA.\n\nTitle\n---\n\nB.\n"
      -- Without the empty line, the paragraph's line would be the title's
      -- first line: one heading, "New text Title".
      headings "## One\n\nNew text\nTitle\n---\n\nB.\n" `shouldBe` [("one", 2, "One"), ("new-text-title", 2, "New text Title")]
      replaceSection (WholeSection "one") source "## One\n\nNew text"
        `shouldBe` Right Replaced {text = "## One\n\nNew text\n\nTitle\n---\n\nB.\n", anchor = Just "one"}

    it "takes a new text for the section's heading, and answers its new anchor" $ do
      let written = replaceSection (WholeSection "install") page "## Setup\n\nSteps.\n"
      (\done -> done.anchor) <$> written `shouldBe` Right (Just "setup")
      (\done -> part done.text "setup" False) <$> written `shouldBe` Right (Just "## Setup\n\nSteps.\n")
      (\done -> outlineAnchors (outline done.text)) <$> written `shouldBe` Right ["a", "setup", "use"]

    it "refuses a heading's new text that changes the anchor of another heading" $ do
      let source = "## Notes\n\nA.\n\n## Use\n\nB.\n\n## Notes\n\nC.\n"
      outlineAnchors (outline source) `shouldBe` ["notes", "use", "notes-2"]
      replaceSection (WholeSection "use") source "## Notes\n\nB.\n" `shouldBe` Left (AnchorChanges "notes-2" "notes-3")

    it "refuses an id written out that another section has, but lets the second of two take its own back" $ do
      let onFirst = "# A {#x}\n\nOne.\n\n# B\n\nTwo.\n"
          onSecond = "# A\n\nOne.\n\n# B {#x}\n\nTwo.\n"
          both = "# A {#x}\n\nOne.\n\n# B {#x}\n\nTwo.\n"
      replaceSection (WholeSection "b") onFirst "# B {#x}\n\nTwo.\n" `shouldBe` Left (AnchorTaken "x")
      replaceSection (WholeSection "a") onSecond "# A {#x}\n\nOne.\n" `shouldBe` Left (AnchorMoves "x")
      replaceSection (WholeSection "x") both "# A\n\nOne.\n\n" `shouldBe` Right Replaced {text = onSecond, anchor = Just "a"}

    it "refuses a body that takes an id an element outside has, through a heading or a block that is no section" $ do
      let source = "# A\n\n# B {#x}\n"
      -- A quotation's, a list item's or a box's heading is no section, and
      -- the outline leaves it out; the page renders its id all the same,
      -- and a link to #x would land on it, before B.
      replaceSection (WholeSection "a") source "# A\n\n> # C {#x}\n" `shouldBe` Left (AnchorMoves "x")
      replaceSection (WholeSection "a") source "# A\n\n- # C {#x}\n" `shouldBe` Left (AnchorMoves "x")
      replaceSection (WholeSection "a") source "# A\n\n[[!div]]{.box}\n# C {#x}\n[[/div]]\n" `shouldBe` Left (AnchorMoves "x")
      -- A box with an id of its own, and a link with one.
      replaceSection (WholeSection "a") source "# A\n\n[[!div]]{#x}\nText.\n[[/div]]\n" `shouldBe` Left (AnchorMoves "x")
      replaceSection (WholeSection "a") source "# A\n\nSee [this](/y){#x}.\n" `shouldBe` Left (AnchorMoves "x")
      -- An id the body does not repeat is taken.
      (\done -> done.text) <$> replaceSection (WholeSection "a") source "# A\n\n> # C {#y}\n" `shouldBe` Right "# A\n\n> # C {#y}\n# B {#x}\n"

    it "refuses a body that changes the id of a heading outside that is no section" $ do
      let source = "# A\n\n# B\n\n> ## Notes\n"
      replaceSection (WholeSection "a") source "# A\n\n> ## Notes\n" `shouldBe` Left (AnchorChanges "notes" "notes-2")

    it "refuses a code fence left open, which takes in the sections after it" $
      replaceSection (WholeSection "install") page "## Install\n\n```\nmake\n" `shouldBe` Left (SectionChanges "use" True)

    it "refuses a heading of another level" $ do
      replaceSection (WholeSection "install") page "### Install\n\nSteps.\n" `shouldBe` Left (NotAHeading 2)
      replaceSection (WholeSection "install") page "Steps first.\n\n## Install\n" `shouldBe` Left (NotAHeading 2)

    it "refuses a second heading of the section's level" $
      replaceSection (WholeSection "install") page "## Install\n\nSteps.\n\n## Extra\n\nMore.\n" `shouldBe` Left (AnotherSection "Extra" 2)

    it "refuses a subsection in a section's own part" $ do
      replaceSection (OwnPart "a") page "# A\n\nLead.\n\n### Sub\n\nMore.\n" `shouldBe` Left (Subsection "Sub")
      replaceSection (OwnPart "a") page "# A\n\nOther lead.\n"
        `shouldBe` Right Replaced {text = "# A\n\nOther lead.\n## Install\n\nSteps.\n\n## Use\n\nRun it.\n", anchor = Just "a"}

    let spoiled = "# A\n\n[[!spoiler]]{s}(More)\n## Hidden\n\nInside.\n[[/spoiler]]{s}\n\n## Next\n\nText.\n"
    it "writes a section inside a spoiler, within the spoiler" $
      replaceSection (WholeSection "hidden") spoiled "## Hidden\n\nChanged."
        `shouldBe` Right Replaced {text = "# A\n\n[[!spoiler]]{s}(More)\n## Hidden\n\nChanged.\n[[/spoiler]]{s}\n\n## Next\n\nText.\n", anchor = Just "hidden"}

    it "refuses a body that closes the spoiler the section is in" $
      replaceSection (WholeSection "hidden") spoiled "## Hidden\n\nInside.\n[[/spoiler]]{s}\n\nOutside.\n" `shouldBe` Left (SpoilerChanges False)

    it "refuses an empty body for a section, and takes one for the top" $ do
      replaceSection (WholeSection "install") page "" `shouldBe` Left EmptyBody
      replaceSection Top ("Intro.\n\n" <> page) "" `shouldBe` Right Replaced {text = page, anchor = Nothing}
      replaceSection Top page "Intro." `shouldBe` Right Replaced {text = "Intro.\n" <> page, anchor = Nothing}
      replaceSection Top page "Intro.\n\n# B\n" `shouldBe` Left (SectionInTop "B")

    it "knows no section by an anchor the page does not have" $
      replaceSection (WholeSection "nowhere") page "## Nowhere\n" `shouldBe` Left NoSuchAnchor

  describe "replaceSection on generated pages, of both kinds" $ do
    let pages = oneof [aPage, aSpoiledPage]
    prop "takes back every part as it was read" $
      coverage . forAll pages $ \source ->
        let parts = targets source
            unchanged = [t | t <- parts, Just (a, b) <- [rangeOf source t], normalised source (a, b) == sectionText source (a, b)]
         in cover 5 (length unchanged == length parts) "every part the same page"
              . cover 2 (length unchanged < length parts) "a last part given a final line break"
              . cover 5 (any inSpoiler parts) "a section inside a spoiler"
              $ conjoin (map (writesBack source) parts)
    prop "changes nothing outside the part written" $
      coverage . forAll (pages >>= \source -> (source,) <$> elements (targets source)) $ \(source, target) ->
        forAll (aBody source target) $ \body ->
          let result = replaceSection target source body
           in counterexample (show result)
                . tabulate "answers" [either (T.unpack . T.takeWhile (/= ' ') . show) (const "accepted") result]
                . cover 25 (isRight result) "accepted"
                . cover 1 (isRight result && inSpoiler target) "a section inside a spoiler written"
                . cover 0.5 (result == Left EmptyBody) "an empty body"
                . cover 1 (refusedFor (\case NotAHeading _ -> True; _ -> False) result) "not a heading"
                . cover 2 (refusedFor (\case AnotherSection _ _ -> True; Subsection _ -> True; SectionInTop _ -> True; _ -> False) result) "a heading too many"
                . cover 0.5 (refusedFor (\case SectionChanges _ _ -> True; _ -> False) result) "a section outside changes"
                . cover 0.5 (refusedFor (\case AnchorChanges _ _ -> True; _ -> False) result) "an anchor outside changes"
                . cover 0.3 (refusedFor (\case AnchorMoves _ -> True; _ -> False) result) "an id outside taken by the body"
                $ either (const (property True)) (outsideKept source target) result
  where
    refusedFor f = either f (const False)
    -- Whether a part is a section inside a spoiler: in 'aSpoiledPage',
    -- those headings have ids that start with @in-@.
    inSpoiler = \case
      WholeSection i -> "in-" `T.isPrefixOf` i
      OwnPart i -> "in-" `T.isPrefixOf` i
      Top -> False
    -- Every part of a page that can be written: the top, and each section
    -- by its anchor, whole and its own part.
    targets :: Text -> [Target]
    targets source = Top : concat [[WholeSection i, OwnPart i] | i <- ordNub (outlineAnchors (outline source))]
    rangeOf source = \case
      Top -> Just (topRange (outline source))
      WholeSection i -> sectionRange (outline source) i False
      OwnPart i -> sectionRange (outline source) i True
    -- A part's text as a write ends it: with a line break. Only the last
    -- part of a page without a final line break does not end so.
    normalised :: Text -> (Int, Int) -> Text
    normalised source (a, b) =
      let t = sectionText source (a, b)
       in if T.null t || T.last t `elem` ['\n', '\r'] then t else t <> "\n"
    -- A part written back as it was read is taken, and the page is the
    -- same, but for a final line break at the page's end ('normalised');
    -- the part read again and written back gives the same page.
    writesBack :: Text -> Target -> Property
    writesBack source target = case rangeOf source target of
      Nothing -> counterexample (show target <> " names nothing") False
      Just (a, b) ->
        let expectedText = T.take a source <> normalised source (a, b) <> T.drop b source
            anchorOf = case target of
              Top -> Nothing
              WholeSection i -> Just i
              OwnPart i -> Just i
         in counterexample (show target) $ case replaceSection target source (sectionText source (a, b)) of
              Left why -> counterexample ("refused: " <> show why) False
              Right done ->
                (done === Replaced {text = expectedText, anchor = anchorOf})
                  .&&. ( fmap (\again -> replaceSection target done.text (sectionText done.text again)) (rangeOf done.text target)
                           === Just (Right done)
                       )
    -- What a write took: outside the part, the text is the same character
    -- for character, the sections are the same headings in the same order,
    -- each anchor names a part whose text is the same (or, for a part
    -- around the one written, the same with the new body in its place), and
    -- the anchor answered names the body.
    outsideKept :: Text -> Target -> Replaced -> Property
    outsideKept source target done = case rangeOf source target of
      Nothing -> counterexample "no part" False
      Just (a, b) ->
        let new = done.text
            prefix = T.take a source
            suffix = T.drop b source
            middle = T.drop a (T.dropEnd (T.length suffix) new)
            b' = a + T.length middle
            old = outline source
            o' = outline new
            outside lo hi o = [(s.id, s.level, s.text) | s <- outlineSections o, s.start < lo || s.start >= hi]
            -- The text a part of the old page has in the new one.
            moved (s, e)
              | (s, e) == (a, b) = middle
              | e <= a = sectionText source (s, e)
              | s >= b = sectionText source (s, e)
              | s <= a && e >= b = sectionText source (s, a) <> middle <> sectionText source (b, e)
              | otherwise = "a part across the edge of the part written"
            ranges o i = [sectionRange o i own | own <- [False, True]]
            -- Each element with an id ('elementIds'): 'Nothing' in the
            -- part, or its number among those outside it.
            numbered lo hi t = snd (mapAccumL (\k (i, at) -> if maybe False (\p -> p >= lo && p < hi) at then (k, (i, Nothing)) else (k + 1, (i, Just k))) (0 :: Int) (elementIds t))
            idsOutside xs = [i | (i, Just _) <- xs]
            -- The outside element each id lands on: the first element with
            -- the id, for the ids whose first element is outside the part.
            landings xs = Map.mapMaybe identity (Map.fromListWith (\_ first' -> first') xs)
            before' = numbered a b source
            after' = numbered a b' new
         in counterexample (show (old, o')) $
              (T.isPrefixOf prefix new .&&. T.isSuffixOf suffix new .&&. T.length new >= T.length prefix + T.length suffix)
                .&&. outside a b old === outside a b' o'
                .&&. conjoin
                  [ counterexample (show i) $ map (fmap (sectionText new)) (ranges o' i) === map (fmap moved) (ranges old i)
                  | i <- ordNub [s.id | s <- outlineSections old, s.start < a || s.start >= b]
                  , all (maybe True (\(s, _) -> s < a || s >= b)) (ranges old i)
                  ]
                .&&. (sectionText new (topRange o') === moved (topRange old))
                .&&. counterexample "the elements with ids outside the part" (idsOutside before' === idsOutside after')
                -- An id that landed on an element in the part may land on
                -- one outside it now (the body gave its element another
                -- id), which mends a link; one that landed outside lands
                -- there still.
                .&&. counterexample ("where the ids of the elements outside the part land: " <> show (landings before', landings after')) (landings before' `Map.isSubmapOf` landings after')
                .&&. ( case (target, done.anchor) of
                         (Top, Nothing) -> topRange o' === (0, b')
                         (WholeSection _, Just i) -> sectionRange o' i False === Just (a, b')
                         (OwnPart _, Just i) -> sectionRange o' i True === Just (a, b')
                         other -> counterexample (show other) False
                     )
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

-- | A body to write in place of a part of a page: now and then nothing at
-- all, else a start, then a few pieces, then an ending of no, one or two
-- line breaks. For a section the
-- start is mostly its heading as the page has it, its whole text as read,
-- or a heading of its level with a new text; sometimes a heading of another
-- level, a title underlined, a heading with another section's text, a
-- paragraph, or nothing. For the top, mostly nothing, a paragraph or its
-- text as read. The pieces are mostly plain (a paragraph, a closed code
-- block, a deeper heading, a spoiler with a heading inside) and sometimes
-- awkward: a code fence or a spoiler left open, an end marker of a
-- spoiler the page has or of none, a heading of the same or a higher
-- level, a title underlined, a lone @---@ or @===@, a heading in a
-- quotation or a list with an id written out: @alpha@, or, more often,
-- one an element after the part has written out, which it would take.
aBody :: Text -> Target -> Gen Text
aBody source target = do
  start <- case section of
    Nothing -> frequency [(3, pure ""), (3, pure "Intro."), (2, pure original), (1, elements others)]
    Just s ->
      frequency
        [ (4, pure heading)
        , (2, pure original)
        , (2, pure (marks s.level <> " Renamed"))
        , (1, elements [marks (s.level + 1) <> " " <> s.text, marks (max 1 (s.level - 1)) <> " " <> s.text, s.text <> "\n===", s.text <> "\n---", "Text.", ""])
        , (1, elements others)
        ]
  count <- choose (0, 3 :: Int)
  pieces <- vectorOf count ((<>) <$> elements ["\n", "\n\n"] <*> frequency ([(6, elements plain), (4, elements awkward)] <> [(3, elements taking) | not (null later)]))
  end <- elements ["", "\n", "\n\n"]
  frequency [(1, pure ""), (19, pure (start <> T.concat pieces <> end))]
  where
    o = outline source
    section = case target of
      Top -> Nothing
      WholeSection i -> find (\s -> s.id == i) (outlineSections o)
      OwnPart i -> find (\s -> s.id == i) (outlineSections o)
    level = maybe 2 (\s -> s.level) section
    marks n = T.replicate n "#"
    original = maybe (sectionText source (topRange o)) (\s -> sectionText source (s.start, if target == OwnPart s.id then s.ownEnd else s.end)) section
    -- The heading's own lines in the page: one for a heading written with
    -- #, two for a title underlined.
    heading =
      let ls = T.lines original
       in T.intercalate "\n" (take (if maybe False ("#" `T.isPrefixOf`) (head ls) then 1 else 2) ls)
    others = marks level <> " Alpha" : [marks level <> " " <> s.text | s <- outlineSections o, Just s.id /= ((\x -> x.id) <$> section)]
    plain = ["Para.", "More text\nover two lines.", "```\ncode\n```", "[[!spoiler]]{q}(Q)\n" <> marks (level + 1) <> " Inner {#in-q}\n[[/spoiler]]{q}", marks (level + 1) <> " Deeper", "- a list"]
    awkward = ["```\nopen", "[[!spoiler]]{z}(Z)\nnever closed", "[[/spoiler]]{s0}", "[[/spoiler]]{s}", "[[/spoiler]]{x}", marks level <> " Sibling", marks (max 1 (level - 1)) <> " Higher", "Alpha\n===", "Beta\n---", "---", "===", "> # Quoted {#alpha}", "- ## Listed {#beta}"]
    -- A heading in a quotation or a list, which is no section, written
    -- with an id written out after the part, which the body's heading
    -- would take.
    taking = concat [["> # Quoted {#" <> i <> "}", "- ## Listed {#" <> i <> "}"] | i <- take 3 (ordNub later)]
    later = [i | (i, Just p) <- elementIds source, p >= partEnd, ("{#" <> i <> "}") `T.isInfixOf` source]
    partEnd = maybe (snd (topRange o)) (\s -> if target == OwnPart s.id then s.ownEnd else s.end) section
