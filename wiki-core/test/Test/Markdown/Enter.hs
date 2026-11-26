-- | Enter in the editor's text ("Wiki.Core.Markdown.Enter"): unit cases
-- written with the cursor at @‸@, each the text before Enter and after
-- it; and properties over generated documents, with the wiki's own parser
-- as the oracle (decision 83), and over generated lists ('Listed'), with a
-- model of what a split and a renumbering write.
--
-- The documents are made of chunks, each a few lines in containers: the
-- containers it keeps of the chunk before (a quotation's marks again, an
-- item's indentation) and new ones (a quotation's mark, an item's mark,
-- a task's box), written with the spacing CommonMark allows and some it
-- reads otherwise (spaces before a mark, one to five after it, a tab),
-- and a body: a paragraph, one with a lazy line after it, a heading, a
-- blank line, nothing (an empty item or quoted line), indented code, a
-- fence, a fence left open, HTML, a table, a box's marker (a box where
-- the chunk starts a quotation). Lists are nested at odd
-- indentations, numbered with @.@ and @)@ up to nine digits, and loose
-- where a blank chunk falls between items.
module Test.Markdown.Enter (spec) where

import Data.List (findIndex)
import Data.Text qualified as T
import GHC.Conc (getAllocationCounter)
import Protolude
import Test.Cases (coverage, longTexts)
import Test.Hspec hiding (after, before)
import Test.Hspec.QuickCheck (modifyMaxSuccess, prop)
import Test.QuickCheck (Arbitrary (..), Gen, Property, choose, conjoin, counterexample, cover, elements, frequency, listOf1, oneof, property, vectorOf, (.&&.), (===))
import Wiki.Core.Markdown.Ast (Block, Shape (..), ShapeKind (..), parseMarkdown, parseShapes, parseWithSpans)
import Wiki.Core.Markdown.Ast qualified as A
import Wiki.Core.Markdown.Enter hiding (Change (..))
import Wiki.Core.Markdown.Enter qualified as E
import Wiki.Core.Markdown.Problems qualified as Problems
import Wiki.Core.Markdown.Starts (Kept (..), blockStarts, longerEnd, textRange)
import Wiki.Core.PageKind (PageKind (..))

spec :: Spec
spec = describe "Wiki.Core.Markdown.Enter" $ do
  describe "a list's item" $ do
    "- one‸" `becomes` "- one\n- ‸"
    "* a‸" `becomes` "* a\n* ‸"
    "+ a‸" `becomes` "+ a\n+ ‸"
    "1. a‸" `becomes` "1. a\n2. ‸"
    "1) a‸" `becomes` "1) a\n2) ‸"
    "- [x] done‸" `becomes` "- [x] done\n- [ ] ‸"
    "- [ ] todo‸" `becomes` "- [ ] todo\n- [ ] ‸"
    "- one\n  - two‸" `becomes` "- one\n  - two\n  - ‸"
    "  -   spaced‸" `becomes` "  -   spaced\n  -   ‸"
    "> - a‸" `becomes` "> - a\n> - ‸"
    "- > - a‸" `becomes` "- > - a\n  > - ‸"
    it "goes on after an item's later line" $ press "- one\n  more‸" `shouldBe` "- one\n  more\n- ‸"
    it "goes on after a loose list's item" $ press "- a\n\n- b‸" `shouldBe` "- a\n\n- b\n- ‸"
    it "goes on after a loose list's first item, the blank line after the new one" $ press "- a‸\n\n- b" `shouldBe` "- a\n- ‸\n\n- b"
    it "renumbers the items below" $ press "1. a‸\n2. b" `shouldBe` "1. a\n2. ‸\n3. b"
    it "goes on after an item's second paragraph" $ press "- one\n\n  more‸" `shouldBe` "- one\n\n  more\n- ‸"
    it "goes on after a lazy line of an item" $ press "- one\nlazy‸" `shouldBe` "- one\nlazy\n- ‸"
    -- Codex's round 8 on #292: a tab in the item's text is no tab among
    -- its marks.
    it "goes on after an item with a tab in its text" $ press "- a\tb‸" `shouldBe` "- a\tb\n- ‸"
    it "breaks the line alone with a tab between a bullet and its text" $ press "- \ta‸" `shouldBe` "- \ta\n‸"
    it "goes on after a task with a tab in its text" $ press "- [ ] a\tb‸" `shouldBe` "- [ ] a\tb\n- [ ] ‸"

  describe "an empty item" $ do
    "- one\n- ‸" `becomes` "- one\n‸"
    "- one\n-‸" `becomes` "- one\n‸"
    "- one\n  - two\n  - ‸" `becomes` "- one\n  - two\n- ‸"
    "1. a\n   - b\n   - ‸" `becomes` "1. a\n   - b\n2. ‸"
    "- [x] a\n  - [x] b\n  - [ ] ‸" `becomes` "- [x] a\n  - [x] b\n- [ ] ‸"
    "> - a\n> - ‸" `becomes` "> - a\n> ‸"
    "- [ ] ‸" `becomes` "‸"
    "- [  ] ‸" `becomes` "‸"
    "- [   ] ‸" `becomes` "‸"
    it "breaks the line alone with an item below that going up would take in" $
      press "- a\n  - ‸\n  - c" `shouldBe` "- a\n  - \n‸\n  - c"

  describe "a quotation or a box" $ do
    "> a‸" `becomes` "> a\n> ‸"
    "> [!NOTE]‸" `becomes` "> [!NOTE]\n> ‸"
    "> [!NOTE]\n> text‸" `becomes` "> [!NOTE]\n> text\n> ‸"
    "> > q‸" `becomes` "> > q\n> > ‸"
    ">  > q‸" `becomes` ">  > q\n>  > ‸"
    "- a\n  > q‸" `becomes` "- a\n  > q\n  > ‸"
    it "goes on after a lazy line" $ press "> a\nlazy‸" `shouldBe` "> a\nlazy\n> ‸"
    it "goes on in the middle of a quotation" $ press "> a‸\n> b" `shouldBe` "> a\n> ‸\n> b"

  describe "an empty quoted line" $ do
    "> a\n> ‸" `becomes` "> a\n‸"
    "> > q\n> > ‸" `becomes` "> > q\n> ‸"
    ">  > q\n>  > ‸" `becomes` ">  > q\n> ‸"
    "> - a\n> ‸" `becomes` "> - a\n‸"
    "- a\n  > q\n  > ‸" `becomes` "- a\n  > q\n- ‸"

  describe "a plain line break" $ do
    "text‸" `becomes` "text\n‸"
    "    code‸" `becomes` "    code\n‸"
    ">     code‸" `becomes` ">     code\n‸"
    "- a\n\n      code‸" `becomes` "- a\n\n      code\n‸"
    "-     a‸" `becomes` "-     a\n‸"
    "```\n- a‸\n```" `becomes` "```\n- a\n‸\n```"
    "> ```\n> ‸" `becomes` "> ```\n> \n‸"
    "<div>\n- a‸\n</div>" `becomes` "<div>\n- a\n‸\n</div>"
    "1.\tone‸" `becomes` "1.\tone\n‸"
    "- [ ]\tone‸" `becomes` "- [ ]\tone\n‸"
    "999999999. a‸" `becomes` "999999999. a\n‸"
    "8. a\n9. b‸\n   - child" `becomes` "8. a\n9. b\n‸\n   - child"
    "- a\n  ‸" `becomes` "- a\n  \n‸"
    "[^a]: - note‸" `becomes` "[^a]: - note\n‸"
    "| a | b |\n| - | - |‸" `becomes` "| a | b |\n| - | - |\n‸"

  -- #301: Enter in the text of an item's first line, with text after the
  -- cursor, splits the item. The text before the cursor stays as it is,
  -- blanks included; the blanks after it go; the item's later lines go
  -- with the new item.
  describe "an item split at the cursor" $ do
    "- on‸e" `becomes` "- on\n- ‸e"
    "- one ‸two" `becomes` "- one \n- ‸two"
    "- one‸ two" `becomes` "- one\n- ‸two"
    "- one‸   two" `becomes` "- one\n- ‸two"
    "* a‸b" `becomes` "* a\n* ‸b"
    "+ a‸b" `becomes` "+ a\n+ ‸b"
    "1. a‸b" `becomes` "1. a\n2. ‸b"
    "1) a‸b" `becomes` "1) a\n2) ‸b"
    "- [x] do‸ne" `becomes` "- [x] do\n- [ ] ‸ne"
    "- [ ] to‸do" `becomes` "- [ ] to\n- [ ] ‸do"
    "1. [x] a‸b" `becomes` "1. [x] a\n2. ‸b"
    "  -   spaced‸ out" `becomes` "  -   spaced\n  -   ‸out"
    "- a\n  - b‸c" `becomes` "- a\n  - b\n  - ‸c"
    "> - a‸b" `becomes` "> - a\n> - ‸b"
    "- > - a‸b" `becomes` "- > - a\n  > - ‸b"
    "- a‸b\n  more" `becomes` "- a\n- ‸b\n  more"
    "- a‸b\nlazy" `becomes` "- a\n- ‸b\nlazy"
    "- a‸b\n  - child" `becomes` "- a\n- ‸b\n  - child"
    "- a‸b\n\n  second" `becomes` "- a\n- ‸b\n\n  second"
    "- a‸b\n- c" `becomes` "- a\n- ‸b\n- c"
    "- a `x` ‸b" `becomes` "- a `x` \n- ‸b"
    "- [a](u)‸ b" `becomes` "- [a](u)\n- ‸b"
    "- 🎉 a‸b 🎉" `becomes` "- 🎉 a\n- ‸b 🎉"
    "- a\tb‸c" `becomes` "- a\tb\n- ‸c"

  -- #301: Enter in the middle of a later line of an item's first
  -- paragraph, indented or lazy, splits the item there too. The lines
  -- above the cursor's stay in the item; the rest of the cursor's line and
  -- every line of the item below it go with the new item.
  describe "an item split on a later line of its first paragraph" $ do
    "- a\n  b‸c" `becomes` "- a\n  b\n- ‸c"
    "- a\n  b‸ c" `becomes` "- a\n  b\n- ‸c"
    "- a\n  b ‸c" `becomes` "- a\n  b \n- ‸c"
    "- a\n  b\n  c‸d" `becomes` "- a\n  b\n  c\n- ‸d"
    "- a\n    b‸c" `becomes` "- a\n    b\n- ‸c"
    "- a\nb‸c" `becomes` "- a\nb\n- ‸c"
    "* a\n  b‸c" `becomes` "* a\n  b\n* ‸c"
    "1) a\n   b‸c" `becomes` "1) a\n   b\n2) ‸c"
    "- [x] a\n  b‸c" `becomes` "- [x] a\n  b\n- [ ] ‸c"
    "- [ ] a\nb‸c" `becomes` "- [ ] a\nb\n- [ ] ‸c"
    "- a\n  - b\n    c‸d" `becomes` "- a\n  - b\n    c\n  - ‸d"
    "- a\n  - b\nc‸d" `becomes` "- a\n  - b\nc\n  - ‸d"
    "> - a\n>   b‸c" `becomes` "> - a\n>   b\n> - ‸c"
    "> - a\n> b‸c" `becomes` "> - a\n> b\n> - ‸c"
    "> [!NOTE]\n> - a\n>   b‸c" `becomes` "> [!NOTE]\n> - a\n>   b\n> - ‸c"
    "- a\n  b‸c\n  more" `becomes` "- a\n  b\n- ‸c\n  more"
    "- a\n  b‸c\n  - child" `becomes` "- a\n  b\n- ‸c\n  - child"
    "- a\n  b‸c\n\n  second" `becomes` "- a\n  b\n- ‸c\n\n  second"
    "- a\n  b‸c\n- d" `becomes` "- a\n  b\n- ‸c\n- d"
    "- [a](u)\n  b‸c" `becomes` "- [a](u)\n  b\n- ‸c"
    -- An item whose text starts on the line after its mark.
    "-\n  a‸b" `becomes` "-\n  a\n- ‸b"
    -- Nothing below the cursor's line is in the item, so a wider number
    -- moves nothing.
    "9. a\n   b‸c" `becomes` "9. a\n   b\n10. ‸c"
    "1. a\n   b‸c\n2. d\n3. e" `becomes` "1. a\n   b\n2. ‸c\n3. d\n4. e"
    "8. a\n   b‸c\n9. d" `becomes` "8. a\n   b\n9. ‸c\n10. d"

  -- With the cursor elsewhere on a line than the contract says, Enter is
  -- a line break and nothing more: no edit at all.
  describe "a plain line break in the middle of a line" $ do
    let plainCases =
          [ ("at the start of the item's text, as before", "- ‸one")
          , ("among the marks", "-‸ one")
          , ("at the start of a task's text", "- [ ] ‸one")
          , ("in a task's box", "- [‸ ] one")
          , ("before blanks alone", "- a‸  ")
          , ("in an item's second paragraph", "- a\n\n  b‸c")
          , ("on a later line of an item's second paragraph", "- a\n\n  b\n  c‸d")
          , ("at the start of a later line's text", "- a\n  ‸bc")
          , ("at the start of a lazy line", "- a\n‸bc")
          , ("among a later line's indentation", "- a\n ‸ bc")
          , ("at the end of a later line with more of the item below", "- a\n  b‸\n  c")
          , ("on a later line of a quotation", "> a\n> b‸c")
          , ("on a later line of a box", "> [!NOTE]\n> a\n> b‸c")
          , ("on a later line in a footnote's list", "[^a]: text\n\n    - a\n      b‸c")
          , ("on a later line of a table in an item", "- | a | b |\n  | - | - |\n  | c‸d | e |")
          , ("on a later line of a fence in an item", "- a\n\n  ```\n  b‸c\n  ```")
          , ("with a tab after a later line's indentation", "- a\n  \tb‸c")
          , ("with a later line indented by a tab", "- a\n\tb‸c")
          , ("in brackets opened on an earlier line", "- [la\n  be‸l]")
          , ("in a link's text over two lines", "- [li\n  n‸k](u)")
          , ("in inline code over two lines", "- `a\n  b‸c`")
          , ("in emphasis over two lines", "- *a\n  b‸c*")
          , ("between a tag of HTML on an earlier line and its end", "- <span class=\"x\">a\n  b‸c</span>")
          , ("where the first half would end in a heading's underline", "- a\n  -‸b")
          , ("where the first half would end in a heading's other underline", "- a\n  =‸b")
          , ("on a later line of an item longer than the limit", "- a\n  b‸c" <> T.replicate textLimit "c")
          , ("in a paragraph", "text‸more")
          , ("in a quotation", "> a‸b")
          , ("in a box", "> [!NOTE]\n> a‸b")
          , ("in a heading", "# He‸ading")
          , ("in a heading in an item", "- # He‸ading")
          , ("in a footnote's definition", "[^a]: - no‸te")
          , ("in a footnote's list", "[^a]: text\n\n    - no‸te")
          , ("in a fence", "```\n- a‸b\n```")
          , ("in indented code", "    - a‸b")
          , ("with a tab after the bullet", "-\ta‸b")
          , ("with a tab after a number", "1.\ta‸b")
          , ("with a line indented by a tab", "- a\n\t- b‸c")
          , ("with a tab right after the cursor", "- a ‸\tb")
          , ("in inline code", "- `co‸de`")
          , ("in math", "- $x‸y$")
          , ("in a link's text", "- [li‸nk](u)")
          , ("in a link's address", "- [link](u‸v)")
          , ("in brackets left open", "- [la‸bel]")
          , ("between a link's text and its label", "- [a]‸[b]")
          , ("in brackets left open, a bracket in code before", "- [a `]` ‸b][ref]")
          , ("in emphasis", "- *em‸ph*")
          , ("in strong emphasis", "- **str‸ong**")
          , ("in an emoji's name", "- :ta‸da:")
          , ("in a mention", "- @lo‸gin")
          , ("in an inline spoiler", "- ||hid‸den||")
          , ("between a tag of HTML and its end", "- <span class=\"x\">a‸b</span>")
          , ("in a colour the toolbar writes", "- <span class=\"c-red\">red ‸text</span> after")
          , ("before a heading's mark", "- a ‸# b")
          , ("before a bullet", "- a ‸- b")
          , ("before a number", "- a ‸1. b")
          , ("before a quotation's mark", "- a ‸> b")
          , ("before a task's box", "- a ‸[ ] b")
          , ("with a carriage return in the text", "- a‸b\r\n- c")
          , ("in an item longer than the limit", "- a‸b" <> T.replicate textLimit "c")
          , -- The item's second paragraph, indented to "9. "'s text, would
            -- not be in "10. "'s.
            ("where the new number's width takes a paragraph out", "9. a‸b\n\n   para")
          , -- The line below, indented to "9. "'s text, would be a lazy
            -- line of "10. "'s paragraph.
            ("where the new number's width makes the line below lazy", "9. a\n   b‸c\n   d")
          ]
    for_ plainCases $ \(name, marked') -> it name $ do
      let (before, rest) = T.breakOn "‸" marked'
      (enter before (T.drop 1 rest), edit True before (T.drop 1 rest)) `shouldBe` (NewLine, Nothing)

  -- #301: after a new item of a numbered list, the items after it in that
  -- list are renumbered, where they were numbered one after another.
  describe "the numbers after a new item" $ do
    "1. a‸b\n2. c\n3. d" `becomes` "1. a\n2. ‸b\n3. c\n4. d"
    "3) a‸\n4) b" `becomes` "3) a\n4) ‸\n5) b"
    "> 1. a‸\n> 2. b" `becomes` "> 1. a\n> 2. ‸\n> 3. b"
    "1. a\n   1. x‸\n   2. y\n2. b" `becomes` "1. a\n   1. x\n   2. ‸\n   3. y\n2. b"
    -- Codex's round 1 on #383: a list that starts on its outer item's
    -- first line starts on the same line as the outer list. The new item
    -- and the numbers are the inner list's, the outer list's left alone.
    "1. 1. a‸\n   2. b\n2. c" `becomes` "1. 1. a\n   2. ‸\n   3. b\n2. c"
    "1. 1. a‸b\n   2. c\n2. d" `becomes` "1. 1. a\n   2. ‸b\n   3. c\n2. d"
    "1. 1. 1. a‸\n      2. b\n   2. c\n2. d" `becomes` "1. 1. 1. a\n      2. ‸\n      3. b\n   2. c\n2. d"
    "- 1. a‸\n  2. b\n- c" `becomes` "- 1. a\n  2. ‸\n  3. b\n- c"
    "1. - a‸\n   - b\n2. c" `becomes` "1. - a\n   - ‸\n   - b\n2. c"
    "1. - a‸b\n2. c" `becomes` "1. - a\n   - ‸b\n2. c"
    "1. - [ ] a‸\n2. c" `becomes` "1. - [ ] a\n   - [ ] ‸\n2. c"
    "1. - [x] a‸b\n2. c" `becomes` "1. - [x] a\n   - [ ] ‸b\n2. c"
    "- 1) a‸b\n  2) c\n- d" `becomes` "- 1) a\n  2) ‸b\n  3) c\n- d"
    "1. a‸\n2. b\n\n- c\n\n1. d" `becomes` "1. a\n2. ‸\n3. b\n\n- c\n\n1. d"
    "1. a‸\n2. b\n\n\n3. c" `becomes` "1. a\n2. ‸\n3. b\n\n\n4. c"
    it "leaves a list numbered 1. 1. 1. as it is" $ press "1. a‸\n1. b\n1. c" `shouldBe` "1. a\n2. ‸\n1. b\n1. c"
    it "leaves a list with a gap in its numbers as it is" $ press "1. a‸\n2. b\n5. c" `shouldBe` "1. a\n2. ‸\n2. b\n5. c"
    it "renumbers past nine, a later line kept as it is in the paragraph" $ press "8. a‸\n9. b\n   more" `shouldBe` "8. a\n9. ‸\n10. b\n   more"
    it "leaves the numbers where a wider one would take a paragraph out of its item" $ press "8. a‸\n9. b\n\n   para" `shouldBe` "8. a\n9. ‸\n9. b\n\n   para"
    it "leaves a list going up from an empty nested item as it is" $ press "1. a\n   - b\n   - ‸\n2. c" `shouldBe` "1. a\n   - b\n2. ‸\n2. c"
    -- The numbers' places in UTF-16 units from the cursor: an emoji is
    -- two of them.
    it "gives the numbers' places in UTF-16 units" $
      fmap (.renumbered) (edit True "1. a" "🎉b\n2. c")
        `shouldBe` Just [E.Change 4 5 "2" "3"]
    it "renumbers after an emoji" $ press "1. 🎉 a‸b 🎉\n2. 🎉 c\n3. d" `shouldBe` "1. 🎉 a\n2. ‸b 🎉\n3. 🎉 c\n4. d"
    -- A page longer than the text Enter reads: the editor hands over the
    -- items around the cursor's, then a longer text for the numbers.
    it "renumbers a list on a long page from the longer text the editor hands over" $ do
      let items = T.intercalate "\n" [show i <> ". Item " <> show i <> "." | i <- [1 .. 30 :: Int]]
          (before, after) = T.breakOn "\n3. " (items <> "\n\n" <> T.intercalate "\n" (replicate 200 "Some more text."))
      fmap (\e -> (e.change, length e.renumbered, take 1 e.renumbered, e.longer)) (editorEdit before after)
        `shouldBe` Just (E.Change 0 0 "" "\n3. ", 28, [E.Change 1 2 "3" "4"], False)
    it "renumbers nothing where the list goes on past the longer text" $ do
      let items = T.intercalate "\n" [show i <> ". Item " <> show i <> "." | i <- [1 .. 400 :: Int]]
          (before, after) = T.breakOn "\n3. " items
      fmap (\e -> (e.change, e.renumbered)) (editorEdit before after) `shouldBe` Just (E.Change 0 0 "" "\n3. ", [])
    it "says where the list may go on past the text, and renumbers in the text it then gets" $ do
      let (before, after) = ("1. a", "\n2. b")
      (fmap (.longer) (edit False before after), fmap (.longer) (edit False before (after <> "\n\nEnd.")), fmap (.longer) (edit True before after))
        `shouldBe` (Just True, Just False, Just False)

  -- Each case the reviews of #292 found in the JavaScript built on the
  -- library, made of the generator's own chunks, so that the properties
  -- below can meet it: the answer the parser's reading gives now.
  describe "the cases of #292's reviews" $ do
    let review name doc cursor expected = it name $ do
          let (before, after) = cursorAfter doc cursor
          enter before after `shouldBe` expected
    review "two spaces between a quotation's marks" (Doc [Chunk [] [Quote 0 (Spaces 2), Quote 0 (Spaces 1)] Text]) 1 (NewLineWith ">  > ")
    review "indented code in a quotation" (Doc [Chunk [] [Quote 0 (Spaces 1)] Indented]) 1 NewLine
    review "a tab after a number" (Doc [Chunk [] [Item 0 "1." Tab Nothing] Text]) 1 NewLine
    review "a numbered item that looks like a task, which the parser reads as text" (Doc [Chunk [] [Item 0 "1." (Spaces 1) (Just "x")] Text]) 1 (NewLineWith "2. ")
    review "an indented number of nine digits" (Doc [Chunk [] [Item 1 "123456789." (Spaces 1) Nothing] Text]) 1 (NewLineWith " 123456790. ")
    review "a numbered item with a child, which a new item would take" (Doc [Chunk [] [Item 0 "8." (Spaces 1) Nothing] Text, Chunk [] [Item 0 "9." (Spaces 1) Nothing] Text, Chunk [Item 0 "9." (Spaces 1) Nothing] [Item 0 "-" (Spaces 1) Nothing] Text]) 2 NewLine
    review "a loose list" (Doc [Chunk [] [Item 0 "-" (Spaces 1) Nothing] Text, Chunk [] [] Blank, Chunk [] [Item 0 "-" (Spaces 1) Nothing] Text]) 3 (NewLineWith "- ")
    review "a lazy line of a quotation" (Doc [Chunk [] [Quote 0 (Spaces 1)] TextLazy]) 2 (NewLineWith "> ")
    -- Codex's round 7 on #292: emptying the task away handed the line of
    -- attributes to the list below (a task's list and a list of plain
    -- items are two lists).
    let task = Item 0 "-" (Spaces 1) (Just " ")
        plainItem = Item 0 "-" (Spaces 1) Nothing
    review "an empty task under a line of attributes, a list below" (Doc [Chunk [] [] Attrs, Chunk [] [task] Empty, Chunk [] [plainItem] Text]) 2 NewLine
    review "the same in a quotation" (Doc [Chunk [] [Quote 0 (Spaces 1)] Attrs, Chunk [Quote 0 (Spaces 1)] [task] Empty, Chunk [Quote 0 (Spaces 1)] [plainItem] Text]) 2 NewLine
    -- CI's seed 2030959543 on main: a footnote's line of attributes with
    -- a quotation indented under it failed the page's parse, so Enter
    -- broke the line alone over the whole text, and went on over the text
    -- handed over, which ends above the footnote.
    review "an item above a line of attributes with an indented line under it" (Doc [Chunk [] [Item 0 "1." (Spaces 1) Nothing] Text, Chunk [] [Item 0 "2." (Spaces 1) Nothing] Text, Chunk [] [Note 1] Attrs, Chunk [Note 1] [Quote 3 (Spaces 1)] Text]) 1 (NewLineWith "2. ")

  -- Texts CodeMirror's tree cut where the wiki's parser reads on (Codex's
  -- review of #292's 0dd1018b): from the start the check keeps, the text
  -- handed over answers as the whole text does. CodeMirror gave
  -- "# Heading\n- " (the HTML above read on, for the wiki's parser) and
  -- "- a\n[[/notes]]" (the start marker in the paragraph above), which
  -- answered "- " and LineBecomes "".
  describe "the text handed over from the start the last check kept" $ do
    let kept name marked' expected = it name $ do
          let (before, rest) = T.breakOn "‸" marked'
              after = T.drop 1 rest
          (editorAnswer before after unchanged, enter before after) `shouldBe` (expected, expected)
    kept "HTML that reads on over a heading and a list" "- a\n<a>\n# Heading\n- ‸" NewLine
    kept "an end marker after a start marker in a paragraph" "plain\n[[!notes]]\n\n- a\n[[/notes]]‸" NewLine
    kept "an item after a footnote's definition" "[^1]: text\n\n    more\n\n- a‸" (NewLineWith "- ")
    -- A property's seed 634744112 at 4,000 cases, shrunk by hand: the
    -- line of attributes in the item above gives them to the cursor's
    -- item, and the parser puts them on its fence, which a split moves
    -- into the new item. The page refuses the split; a text cut at the
    -- cursor's item, which took the attributes, would not see why.
    kept "an item's later line, under an item holding a line of attributes" "+ {#a .b}\n+ a\n  b‸c\n  ```\n  code\n  ```" NewLine
    it "counts no start at a block or an item that took attributes" $ do
      let text = "+ {#a .b}\n+ a\n+ b\n\n{#c}\n\nPara.\n\nEnd."
      fmap (blockStarts text) (parseShapes text) `shouldBe` Right [1, 3, 5, 9]
    -- Codex's round 6 on #292: the check read "- a\n\nx\n\n  b", and the
    -- paragraph between was deleted since. Its start and the item's below
    -- map onto one place, which counted twice cut "  b" off.
    it "keeps two starts a deletion put in one place from cutting the item's line below off" $ do
      let (before, after) = ("- a", "\n  b")
      (maybe NewLine (uncurry enter) (handedOverAt before after 3 1 "\n\nx\n\n"), enter before after) `shouldBe` (NewLine, NewLine)
    -- CI's seed 1649390415 on main, shrunk: the check read
    -- "1. a\n\n\nx\n\n\n   more", and "\n\nx\n\n" was deleted since. The
    -- paragraph's start lands on the blank line, and the indented line's
    -- start, a top-level block then, is the item's second paragraph now:
    -- the two cut the text before it, and Enter went on in the list.
    it "ends the text below the first change only at a flush line" $ do
      let (before, after) = ("1. a", "\n\n   more")
      (maybe NewLine (uncurry enter) (handedOverAt before after 5 0 "\n\nx\n\n"), enter before after) `shouldBe` (NewLine, NewLine)
    -- The same property at 100,000 cases, seed 1649390415: a deleted
    -- block's start lands on an empty line at the change, which ends
    -- where the change begins, and counted as a start the check read
    -- before the change. Below the cursor it cut the item under it off;
    -- above the cursor it began the text inside the item above.
    it "counts no start on an empty line where a change begins" $ do
      let (before, after) = ("999999999. [x] 10.     Some text.\nlazy text\nSome text.\n > ", "\n\n999999999. [ ] * ")
      (maybe NewLine (uncurry enter) (handedOverAt before after 59 0 "\n\nx\n\n"), enter before after) `shouldBe` (NewLine, NewLine)
      (maybe NewLine (uncurry enter) (handedOverAt "- a\n\n  b" "" 4 0 "\nx\n"), enter "- a\n\n  b" "") `shouldBe` (NewLineWith "- ", NewLineWith "- ")
    -- Codex's round 6: CommonMark counts a carriage return alone as a line
    -- break, and the editor does not, so the lines differ.
    it "keeps no starts, and breaks the line alone, in a text holding a carriage return" $ do
      (Problems.checkText Document "p\rp\rp\n\n```\n\n- a\n- b\n```\n\nend").starts `shouldBe` []
      enter "p\rp\n\n- a" "" `shouldBe` NewLine

  -- What Enter reads, and so what it costs, is bounded: by the text the
  -- editor hands over, the cursor's block and the one below it, and past
  -- 'textLimit' by nothing at all.
  describe "what Enter costs" $ do
    -- The check finds the starts after every pause in the typing, once
    -- over the lines (Codex's round 8 on #292; "Wiki.Core.Markdown.Starts"
    -- says how). Its cost is asserted by no count: allocation does not
    -- tell a walk down the list of lines for each block from one lookup
    -- (47 MB and 54 MB for these paragraphs, natively, 2026-11-17), and a
    -- time is not asserted. The large text is read here all the same.
    -- A new item in the middle of a numbered list of a thousand items:
    -- the items around the cursor's, then the longer text, past whose end
    -- the list goes on, so no number is changed.
    it "reads at most the longer text for the numbers in the middle of a numbered list of a thousand items" $ do
      let ls = "# Items" : "" : [show i <> ". item " <> show i | i <- [1 .. 1000 :: Int]]
          before = T.intercalate "\n" (take 502 ls)
          after = mconcat ["\n" <> l | l <- drop 502 ls]
          ((given, taken, ends), (given', taken', ends')) = fromMaybe (("", "", True), ("", "", True)) (editorTexts before after)
      _ <- evaluate (T.length given + T.length taken + T.length given' + T.length taken' + fromEnum ends + fromEnum ends')
      (answer, allocated) <- allocations (askedOf (given, taken, ends) (given', taken', ends'))
      -- 1,987 characters in the longer text and 50 MB, measured natively
      -- on 2026-11-25, unoptimised, both asks together.
      (fmap (.renumbered) answer, T.length given' + T.length taken' <= textLimit, allocated < 100_000_000) `shouldBe` (Just [], True, True)
    it "finds the starts of sixteen thousand paragraphs" $ do
      let text = T.intercalate "\n\n" ["Paragraph " <> show i <> "." | i <- [1 .. 16000 :: Int]]
      blockStarts text (fromRight [] (parseShapes text)) `shouldBe` [1, 3 .. 31999]
    it "breaks the line alone past its limit, reading nothing" $ do
      let items = T.intercalate "\n" ["- item " <> show i | i <- [1 .. 1000 :: Int]]
      _ <- evaluate (T.length items)
      (answer, allocated) <- allocations (enter items "")
      -- 1,488 bytes measured natively on 2026-11-16, unoptimised.
      (answer, allocated < 100_000) `shouldBe` (NewLine, True)
    -- A page-level list's items are starts: the editor hands over the
    -- items around the cursor's, not the whole list.
    it "goes on at the end and in the middle of a list of a thousand items, reading a few of them" $ do
      let ls = "# Items" : "" : ["- item " <> show i | i <- [1 .. 1000 :: Int]]
          at n = fromMaybe ("", "") (handedOver (T.intercalate "\n" (take n ls)) (mconcat ["\n" <> l | l <- drop n ls]) unchanged)
      for_ [length ls, 502] $ \n -> do
        let (given, taken) = at n
        _ <- evaluate (T.length given + T.length taken)
        (answer, allocated) <- allocations (enter given taken)
        -- At the end: 22 characters and 0.77 MB measured natively on
        -- 2026-11-17, unoptimised; the middle reads three items.
        (answer, T.length given + T.length taken < 100, allocated < 5_000_000) `shouldBe` (NewLineWith "- ", True, True)
    it "reads only the cursor's block and the one below it in a far slide of a long deck" $ do
      let ls = T.splitOn "\n" longDeck
          -- The end of slide 50's last item.
          n = 2 + 100 * 49 + 36
          before = T.intercalate "\n" (take n ls)
          after = mconcat ["\n" <> l | l <- drop n ls]
          (given, taken) = fromMaybe ("", "") (handedOver before after unchanged)
      _ <- evaluate (T.length given + T.length taken)
      (answer, allocated) <- allocations (enter given taken)
      -- 407 characters and 6.6 MB measured natively on 2026-11-16,
      -- unoptimised; the deck is 300 KB.
      (answer, T.length given + T.length taken < 1000, allocated < 20_000_000) `shouldBe` (NewLineWith "- ", True, True)

  -- Ten times the suite's count: 1,000 documents natively, a quarter of
  -- that on wasm (Test.Cases), each pressed at every line's end.
  modifyMaxSuccess (* 10) $ do
    -- The answer is forced at every line's end, and its text holds no line
    -- break: what it writes stays on the cursor's line and one new line.
    prop "writes only on the cursor's line and one new line, and never throws" $ \doc ->
      atEveryLineEnd doc $ \_ _ answer -> case answer of
        NewLine -> property True
        NewLineWith t -> counterexample "a new line's start with a line break in it" (not ("\n" `T.isInfixOf` t))
        LineBecomes t -> counterexample "a line with a line break in it" (not ("\n" `T.isInfixOf` t))
        SplitWith _ _ -> counterexample "a split at a line's end" False
    -- The parser reads the cursor's line as code or HTML (alone, or in a
    -- quotation or an item): Enter is a plain line break.
    prop "breaks the line alone in code and HTML" $ \doc ->
      atEveryLineEnd doc $ \before after answer ->
        let n = T.count "\n" before + 1
         in if inCode (before <> after) n then answer === NewLine else property True
    -- A new line the parser reads as a new item: the tree is the old tree
    -- with one empty item after one of a list's items (a task after a
    -- task), and nothing else changed, the lines below included.
    prop "adds exactly one empty item, after an item, where it starts a new item" $ \doc ->
      atEveryLineEnd doc $ \before after answer -> case answer of
        NewLineWith _ | newItemOn before after answer -> itemAdded before after answer
        _ -> property True
    -- Any other new line is in the same quotations and items, in order,
    -- as the cursor's line.
    prop "keeps a new quoted line in the quotations and items of the cursor's line" $ \doc ->
      atEveryLineEnd doc $ \before after answer -> case answer of
        NewLineWith _ | not (newItemOn before after answer) -> quotedLineKept before after answer
        _ -> property True
    -- Whatever Enter writes, the top-level blocks before the cursor's and
    -- after it are read as before.
    prop "leaves the top-level blocks around the cursor's as they were" $ \doc ->
      atEveryLineEnd doc $ \before after answer -> case answer of
        NewLine -> property True
        _ -> blocksAroundKept before after answer
    -- A line replaced is one container less deep (an item of the list
    -- around counts as deep as its sibling), with nothing after its marks.
    -- Each block that took attributes, from its own braces or from a line
    -- of attributes alone above it, takes the same ones: a list's are
    -- shown, a paragraph's and a quotation's are dropped.
    prop "leaves every line of attributes to the block it went to" $ \doc ->
      atEveryLineEnd doc $ \before after answer -> case answer of
        NewLine -> property True
        _ -> attributesKept before after answer
    prop "takes an empty item or quoted line up one level" $ \doc ->
      atEveryLineEnd doc $ \before after answer -> case answer of
        LineBecomes _ -> wentUp before after answer
        _ -> property True
    -- What the editor hands over gives the answer the whole page gives:
    -- the text from the start the last check kept at or above the
    -- cursor's line and before the first change since, mapped through the
    -- change ('handedOver'), for a change anywhere (letters typed in the
    -- cursor's block, a block added below, one edited above).
    -- A tenth of the cases on wasm, where each document's long text is
    -- checked at every line's end ("Test.Cases").
    longTexts . prop "answers as for the whole text, for the text from the start the last check kept" $ \doc change ->
      atEveryLineEnd doc $ \before after answer -> case handedOver before after change of
        Just (given, taken) -> counterexample (show (given, taken)) (enter given taken === answer)
        Nothing -> property True
    -- The same for every block deleted below the cursor since the check,
    -- at each blank line below it: the deleted block's start lands there,
    -- and the starts below it follow, where the deletion may have put
    -- them in a block above (seed 1649390415 met one in a random change).
    prop "answers as for the whole text, after a block below the cursor deleted since the check" $ \doc ->
      atEveryLineEnd doc $ \before after answer ->
        conjoin
          [ counterexample (show (at, given, taken)) (enter given taken === answer)
          | at <- blankLinesBelow before after
          , Just (given, taken) <- [handedOverAt before after at 0 "\n\nx\n\n"]
          ]
    -- Not only safe but useful: in containers written as CommonMark reads
    -- them without doubt, Enter at the end of an item's or a quotation's
    -- text goes on in it, as the containers say.
    prop "goes on in a list or a quotation written plainly" $ \(Plain doc) ->
      let rendered = render doc
          expected = plainExpected doc
       in counterexample (toS (rendered <> "‸"))
            $ coverage
            $ cover 30 (isNewItem expected) "an item"
            $ cover 20 (not (isNewItem expected)) "a quoted line"
            $ enter rendered "" === expected
    -- Every document generated is one the parser reads: on one it cannot
    -- read, Enter over the whole text is a line break alone, while the
    -- text handed over, which may end above where the parse fails, reads.
    prop "reads every document" $ \doc ->
      counterexample (toS (render doc)) (isRight (parseShapes (render doc)))
    -- The documents meet every kind of answer often enough for the
    -- properties above to mean something.
    prop "covers every answer" $ \doc ->
      let answers = [enter b a | (b, a) <- lineEnds (render doc)]
       in coverage
            $ cover 30 (any isNewItem answers) "a new item"
            $ cover 15 (any isQuoted answers) "a new quoted line"
            $ cover 10 (any isUp answers) "a line gone up"
            $ cover 50 (NewLine `elem` answers) "a plain line break" True

  -- #301 over generated lists ('Listed'), the cursor on an item's first
  -- line or on the later line of its first paragraph: at its end, in its
  -- text, among its marks or its indentation.
  describe "in a generated list" $ do
    -- What Enter writes is what the model says ('modelPress'): a split, a
    -- new item, the numbers after either, or a plain line break where the
    -- model says the parser reads the cut otherwise.
    prop "writes what the model of a split and of the numbers says" $ \l ->
      let (before, after) = cursorOf l
       in counterexample (toS (before <> "‸" <> after))
            $ coverage
            $ cover 25 (splits l) "a split"
            $ cover 1 (splits l && onLater l && maybe False ((== Continued) . (.more)) (atMay l.entries l.item)) "a split on an item's indented later line"
            $ cover 1 (splits l && onLater l && maybe False ((== LazyLine) . (.more)) (atMay l.entries l.item)) "a split on an item's lazy line"
            $ cover 3 (cutInside l) "a cut inside code, math, a link or emphasis"
            $ cover 10 (renumbers l) "numbers rewritten"
            $ cover 1 (renumbers l && widens l) "a number one digit wider"
            $ cover 20 (any onLine l.holders) "a list starting on its outer item's first line"
            $ cover 3 (renumbers l && any onLine l.holders) "numbers rewritten in a list starting on its outer item's first line"
            -- At the end of a line, where a new item starts is the rule
            -- of #292, which the properties above check; the model is of
            -- what comes after a new item there.
            $ if not (inText l) && enter before after == NewLine
              then property True
              else press (before <> "‸" <> after) === modelPress l
    -- The parser's reading of a split, apart from the model: one list has
    -- one more item, the two items' texts joined are the old item's, the
    -- rest of the page is the same, and the cursor is at the start of the
    -- new item's text.
    prop "splits one item in two, as the parser reads it, the cursor at the new item's text" $ \l ->
      let (before, after) = cursorOf l
       in counterexample (toS (before <> "‸" <> after)) $ case enter before after of
            SplitWith blanks start -> splitReadBack before after blanks start
            _ -> property True
    prop "takes the edit back with its inverse" $ \l ->
      let (before, after) = cursorOf l
       in counterexample (toS (before <> "‸" <> after)) $ case edit True before after of
            Just e -> undone before after e === Just (before <> after)
            Nothing -> property True
    -- From the texts the editor hands over (from the start the last check
    -- kept, then a longer one where the answer asks for it), the edit is
    -- the whole text's.
    prop "edits from the editor's texts as from the whole text" $ \l ->
      let (before, after) = cursorOf l
       in counterexample (toS (before <> "‸" <> after)) (editorEdit before after === edit True before after)

  -- Enter in the middle of any line of the generated documents: a split
  -- the parser reads as one, which the inverse edit takes back, or a plain
  -- line break; never anything else. The documents' items have later
  -- lines, indented and lazy, so some splits are on those.
  prop "splits an item read back as one, or breaks the line alone, in the middle of any line" $ \doc ->
    let text = render doc
     in counterexample (toS text)
          $ coverage
          $ cover 5 (any (uncurry splitsOnLaterLine) (midLines text)) "a split on an item's later line"
          $ conjoin
            [ counterexample (toS (before <> "‸" <> after)) $ case enter before after of
                NewLine -> property True
                SplitWith blanks start ->
                  splitReadBack before after blanks start
                    .&&. (undone before after =<< edit True before after) === Just text
                other -> counterexample ("not a split: " <> show other) False
            | (before, after) <- midLines text
            ]
  -- The same as "answers as for the whole text, for the text from the
  -- start the last check kept", in the middle of lines: a split from the
  -- text the editor hands over, for a change anywhere since the check, is
  -- the whole text's.
  longTexts . prop "splits as for the whole text, for the text from the start the last check kept" $ \doc change ->
    let text = render doc
     in counterexample (toS text) $
          conjoin
            [ counterexample (toS (before <> "‸" <> after) <> show (given, taken)) (enter given taken === enter before after)
            | (before, after) <- midLines text
            , Just (given, taken) <- [handedOver before after change]
            ]
  where
    isNewItem = \case
      NewLineWith t -> not (T.null (T.strip (T.filter (/= '>') t)))
      _ -> False
    isQuoted = \case
      NewLineWith t -> T.null (T.strip (T.filter (/= '>') t))
      _ -> False
    isUp = \case
      LineBecomes _ -> True
      _ -> False

----------------------------------------------------------------------
-- Unit cases

-- | The text after Enter at the @‸@, with the cursor at @‸@: the edit
-- applied ('edit', with the text ending where the page does), or a line
-- break.
press :: Text -> Text
press marked' =
  let (before, rest) = T.breakOn "‸" marked'
      after = T.drop 1 rest
   in case applied before after =<< edit True before after of
        Just (new, at) -> T.take at new <> "‸" <> T.drop at new
        Nothing -> before <> "\n‸" <> after

-- | The text after an edit, and the cursor's place in it, in characters:
-- each change's range read in UTF-16 units from the cursor, as the editor
-- reads it. Nothing where a range does not hold the text the change says
-- it holds.
applied :: Text -> Text -> Edit -> Maybe (Text, Int)
applied before after e = do
  let text = before <> after
      at u
        | u >= 0 = T.length before + unitsIn after u
        | otherwise = T.length before - unitsIn (T.reverse before) (negate u)
      ranges = sortOn (\(a, _, _, _) -> a) [(at a, at b, old, new) | E.Change a b old new <- e.change : e.renumbered]
  guard (and [T.take (b - a) (T.drop a text) == old | (a, b, old, _) <- ranges])
  let new = replacing [(a, b, t) | (a, b, _, t) <- ranges] text
  pure (new, unitsIn new (utf16Length before + e.cursor))

-- | The changes of an edit taken back: each the range its new text holds
-- in the text after the edit, its old text put back.
undone :: Text -> Text -> Edit -> Maybe Text
undone before after e = do
  (new, _) <- applied before after e
  let at u
        | u >= 0 = T.length before + unitsIn after u
        | otherwise = T.length before - unitsIn (T.reverse before) (negate u)
      ranges = sortOn (\(a, _, _, _) -> a) [(at a, at b, old, t) | E.Change a b old t <- e.change : e.renumbered]
      -- Where each change's new text starts after the edit: moved by
      -- what the changes before it added.
      shifts = scanl (\s (a, b, _, t) -> s + T.length t - (b - a)) 0 ranges
      back = [(a + s, a + s + T.length t, old) | ((a, _, old, t), s) <- zip ranges shifts]
  pure (replacing back new)

-- | A text with ranges replaced, in characters, in order.
replacing :: [(Int, Int, Text)] -> Text -> Text
replacing changes t = go 0 changes
  where
    go at = \case
      [] -> T.drop at t
      (a, b, new) : rest -> T.take (a - at) (T.drop at t) <> new <> go b rest

-- | How many characters of a text make so many UTF-16 units.
unitsIn :: Text -> Int -> Int
unitsIn t u = length (takeWhile (<= u) (scanl (+) 0 (map (\c -> if ord c > 0xFFFF then 2 else 1) (T.unpack t)))) - 1

becomes :: Text -> Text -> Spec
becomes old new = it (show old <> " becomes " <> show new) (press old `shouldBe` new)

----------------------------------------------------------------------
-- Generated documents

-- | A container a chunk's lines are in.
data Layer
  = -- | A quotation: the spaces before its mark, and what follows it.
    Quote Int Gap
  | -- | A list's item: the spaces before its mark, the mark, what follows
    -- it, and a task's box (what is between its brackets: one to three
    -- spaces, or @x@).
    Item Int Text Gap (Maybe Text)
  | -- | A footnote's definition, @[^7]: @, its later lines indented by
    -- four spaces: a container CodeMirror's Markdown does not know. Its
    -- label is drawn from a million, so that no two are the same: the
    -- parser drops a second definition of a label, blocks and all, which
    -- makes the reading of a line depend on a definition anywhere above
    -- it, not only on the blocks around it.
    Note Int
  deriving stock (Eq, Show)

-- | What follows a mark: spaces, or a tab.
data Gap = Spaces Int | Tab
  deriving stock (Eq, Show)

-- | What a chunk's lines hold after their containers' marks.
data Body = Attrs | AttrsIndented | Text | TabText | TextLazy | Heading | Box | Blank | Empty | Indented | Fenced | OpenFence | Html | Table | Opener | Closer | TextOpener | RawHtml Int
  deriving stock (Eq, Show)

-- | Some lines: the containers kept from the chunk before, written as on
-- a later line, the new ones, written on the first line with their marks,
-- and the body.
data Chunk = Chunk [Layer] [Layer] Body
  deriving stock (Eq, Show)

-- | A document: its chunks.
newtype Doc = Doc [Chunk]
  deriving stock (Eq, Show)

instance Arbitrary Doc where
  arbitrary = do
    k <- choose (1, 8)
    Doc . reverse . fst <$> foldM (\(done, stack) () -> chunk stack >>= \c@(Chunk kept fresh _) -> pure (c : done, kept <> fresh)) ([], []) (replicate k ())

-- | A chunk after a chunk in these containers: some of them kept, and up
-- to two new ones; an item at a depth where one was is often its list's
-- next item.
chunk :: [Layer] -> Gen Chunk
chunk stack = do
  k <- choose (0, length stack)
  let kept = take k stack
  fresh <- do
    n <- frequency [(2, pure 0), (3, pure 1), (2, pure 2)]
    first' <- case drop k stack of
      Item b m g t : _ | n > 0 -> frequency [(2, pure [Item b (nextMark m) g t]), (1, (: []) <$> layer)]
      _ -> replicateM (min n 1) layer
    rest <- replicateM (max 0 (n - 1)) layer
    -- A footnote's definition at the top level only, where a page writes
    -- one (the parser reads one in an item too, and lists it at the end).
    footnote <- if k == 0 then frequency [(5, pure []), (1, (: []) . Note <$> choose (1, 1000000))] else pure []
    pure (footnote <> first' <> rest)
  body <- frequency [(2, pure Attrs), (1, pure AttrsIndented), (6, pure Text), (1, pure TabText), (2, pure TextLazy), (1, pure Heading), (1, pure Box), (2, pure Blank), (2, pure Empty), (1, pure Indented), (1, pure Fenced), (1, pure OpenFence), (1, pure Html), (1, pure Table), (1, pure Opener), (1, pure Closer), (1, pure TextOpener), (2, RawHtml <$> choose (1, 7))]
  pure (Chunk kept fresh body)

-- | A new container.
layer :: Gen Layer
layer =
  frequency
    [ (2, Quote <$> frequency [(4, pure 0), (1, choose (1, 3))] <*> gap)
    , (3, Item <$> frequency [(4, pure 0), (1, choose (1, 3))] <*> elements ["-", "-", "*", "+", "1.", "2)", "10.", "123456789.", "999999999."] <*> gap <*> frequency [(3, pure Nothing), (1, Just <$> elements [" ", "x", "  ", "   "])])
    ]
  where
    gap = frequency [(6, pure (Spaces 1)), (2, Spaces <$> choose (0, 5)), (1, pure Tab)]

-- | The mark of a list's next item.
nextMark :: Text -> Text
nextMark m = case T.unsnoc m of
  Just (digits, d) | not (T.null digits), T.all (`elem` ['0' .. '9']) digits -> maybe m (\n -> show (n + 1 :: Integer) <> T.singleton d) (readMaybe (T.unpack digits))
  _ -> m

-- | A document's text.
render :: Doc -> Text
render (Doc chunks) = T.intercalate "\n" (concatMap renderChunk chunks)

-- | A chunk's lines.
renderChunk :: Chunk -> [Text]
renderChunk (Chunk kept fresh body) =
  [ if lazy then line else foldMap laterForm kept <> foldMap (if i == 0 then firstForm else laterForm) fresh <> line
  | (i, (lazy, line)) <- zip [0 :: Int ..] (bodyLines body)
  ]

-- | A body's lines, each with whether it is lazy (written with no marks).
bodyLines :: Body -> [(Bool, Text)]
bodyLines = \case
  -- A line of attributes alone, which the block after it takes.
  Attrs -> [(False, "{#a .b}")]
  -- The same, and a line indented by less than code's four spaces right
  -- after it, which is no line of attributes.
  AttrsIndented -> [(False, "{#a .b}"), (False, "   Some text.")]
  Text -> [(False, "Some text.")]
  -- A tab in the text, after its first character.
  TabText -> [(False, "a\tb")]
  TextLazy -> [(False, "Some text."), (True, "lazy text")]
  Heading -> [(False, "# A heading")]
  -- A box's marker, where its line is a quotation's first.
  Box -> [(False, "[!NOTE] A box")]
  Blank -> [(False, "")]
  Empty -> [(False, "")]
  Indented -> [(False, "    code")]
  Fenced -> [(False, "```"), (False, "- code"), (False, "```")]
  OpenFence -> [(False, "```"), (False, "> code")]
  Html -> [(False, "<div>"), (False, "- html"), (False, "</div>")]
  Table -> [(False, "| a | b |"), (False, "| - | - |")]
  -- The marker lines of one of the wiki's own blocks, which CodeMirror's
  -- Markdown does not know either.
  Opener -> [(False, "[[!notes]]")]
  Closer -> [(False, "[[/notes]]")]
  -- A start marker right after a paragraph's line.
  TextOpener -> [(False, "Some text."), (False, "[[!notes]]")]
  -- HTML of each of CommonMark's seven kinds, its end not written: the
  -- first five read on to their end marker, the last two to a blank line.
  RawHtml kind -> [(False, fromMaybe "<a>" (atMay ["<script>", "<!-- a comment", "<?php", "<!DOCTYPE html>", "<![CDATA[", "<div>", "<a>"] (kind - 1))), (False, "- html")]

-- | A container's marks on its first line.
firstForm :: Layer -> Text
firstForm = \case
  Quote b g -> T.replicate b " " <> ">" <> gapText g
  Item b m g t -> T.replicate b " " <> m <> gapText g <> maybe "" (\c -> "[" <> c <> "] ") t
  Note label -> "[^" <> show label <> "]: "

-- | A container's marks on a later line: a quotation's again, an item's
-- indentation.
laterForm :: Layer -> Text
laterForm = \case
  q@(Quote _ _) -> firstForm q
  Item b m g _ -> T.replicate (b + T.length m + width g) " "
  Note _ -> "    "
  where
    width = \case
      Spaces k | k >= 1 && k <= 4 -> k
      Spaces _ -> 1
      Tab -> 4

gapText :: Gap -> Text
gapText = \case
  Spaces k -> T.replicate k " "
  Tab -> "\t"

-- | The document split at the end of a line (from 1), as the text before
-- the cursor and after it.
cursorAfter :: Doc -> Int -> (Text, Text)
cursorAfter doc n =
  let ls = T.splitOn "\n" (render doc)
   in (T.intercalate "\n" (take n ls), mconcat ["\n" <> l | l <- drop n ls])

-- | Every line end of a text, as the text before the cursor and after it.
lineEnds :: Text -> [(Text, Text)]
lineEnds text =
  let ls = T.splitOn "\n" text
   in [(T.intercalate "\n" (take n ls), mconcat ["\n" <> l | l <- drop n ls]) | n <- [1 .. length ls]]

-- | A property at every line end of a document, with Enter's answer there.
atEveryLineEnd :: Doc -> (Text -> Text -> Enter -> Property) -> Property
atEveryLineEnd doc holds =
  counterexample (toS (render doc)) $
    conjoin
      [ counterexample (toS (before <> "‸" <> after)) (holds before after (enter before after))
      | (before, after) <- lineEnds (render doc)
      ]

----------------------------------------------------------------------
-- The parser's reading, as the properties check it

-- | The shapes covering a line, the outermost first: those whose lines,
-- or whose inner shapes' lines, run over it.
shapesAt :: [Shape] -> Int -> [Shape]
shapesAt shapes n = case lastMay (filter covers shapes) of
  Nothing -> []
  Just s -> s : shapesAt s.inner n
  where
    covers s = let (a, b) = linesOf s in a <= n && n <= b

-- | A shape's first and last line.
linesOf :: Shape -> (Int, Int)
linesOf s =
  let own = [(l, if c == 1 then l' - 1 else l') | ((l, _), (l', c)) <- s.parts]
      lows = map fst own <> map (fst . linesOf) s.inner
      highs = map snd own <> map (snd . linesOf) s.inner
   in (fromMaybe 0 (minimumMay lows), fromMaybe 0 (maximumMay highs))

-- | Whether the parser reads a line as code or HTML.
inCode :: Text -> Int -> Bool
inCode text n = case parseShapes text of
  Right shapes -> any (\s -> s.kind == CodeShape) (shapesAt shapes n)
  Left _ -> False

-- | The text after an answer.
textAfter :: Text -> Text -> Enter -> Text
textAfter before after answer = let (b, a) = written before after answer in b <> a

-- | Whether an answer's new line starts an item, for the parser.
newItemOn :: Text -> Text -> Enter -> Bool
newItemOn before after answer = case parseShapes (textAfter before after answer) of
  Right shapes -> any (startsItem (T.count "\n" before + 2)) (shapesAt shapes (T.count "\n" before + 2))
  Left _ -> False
  where
    startsItem n s = case s.kind of
      ItemShape _ _ -> fmap (fst . fst) (head s.parts) == Just n
      _ -> False

-- | Every tree made of these blocks with one empty item put in a list,
-- right after one of its items (a task's after a task).
withItemAdded :: [Block] -> [[Block]]
withItemAdded = \case
  [] -> []
  b : bs -> map (: bs) (inBlock b) <> map (b :) (withItemAdded bs)
  where
    inBlock = \case
      A.List kind items ->
        [ A.List kind (take (i + 1) items <> [A.ListItem (False <$ task) []] <> drop (i + 1) items)
        | (i, A.ListItem task _) <- zip [0 ..] items
        ]
          <> [ A.List kind (take i items <> [A.ListItem task inner'] <> drop (i + 1) items)
             | (i, A.ListItem task inner) <- zip [0 ..] items
             , inner' <- withItemAdded inner
             ]
      A.BlockQuote bs -> A.BlockQuote <$> withItemAdded bs
      A.Alert k h bs -> A.Alert k h <$> withItemAdded bs
      A.Div a bs -> A.Div a <$> withItemAdded bs
      A.Spoiler h bs -> A.Spoiler h <$> withItemAdded bs
      A.Footnotes notes -> [A.Footnotes (take i notes <> [A.Footnote n l bs'] <> drop (i + 1) notes) | (i, A.Footnote n l bs) <- zip [0 ..] notes, bs' <- withItemAdded bs]
      _ -> []

-- | The parser reads the text after the answer as the text before it with
-- one empty item added after an item. A paragraph and an item's plain text
-- count as the same: an empty item followed by a blank line does not make
-- its list loose, as the item's text, typed, will (commonmark 0.3.1), so
-- a loose list may read as tight until then.
itemAdded :: Text -> Text -> Enter -> Property
itemAdded before after answer = case (parseMarkdown (before <> after), parseMarkdown (textAfter before after answer)) of
  (Right old, Right new) -> counterexample (show new) (map plain new `elem` map (map plain) (withItemAdded old))
  _ -> counterexample "no parse" False

-- | A block with every paragraph in it as an item's plain text.
plain :: Block -> Block
plain = \case
  A.Para xs -> A.Plain xs
  A.List kind items -> A.List kind [A.ListItem task (map plain bs) | A.ListItem task bs <- items]
  A.BlockQuote bs -> A.BlockQuote (map plain bs)
  A.Alert k h bs -> A.Alert k h (map plain bs)
  A.Div a bs -> A.Div a (map plain bs)
  A.Spoiler h bs -> A.Spoiler h (map plain bs)
  A.Footnotes notes -> A.Footnotes [A.Footnote n l (map plain bs) | A.Footnote n l bs <- notes]
  other -> other

-- | The quotations and items a line is in, for the parser, each with the
-- line it starts on. On a blank line, or one in no block of text or code,
-- only marks,
-- the items at the end that do not start on it are left out: such a line
-- is a blank line of those items, no item's own.
containersAt :: Text -> Int -> Maybe [(Bool, Int)]
containersAt text n = do
  shapes <- rightToMaybe (parseShapes text)
  let found = shapesAt shapes n
      held = [(isQuote s, fst (linesOf s)) | s <- found, isQuote s || isItem s]
      blank = maybe True (T.null . T.strip) (atMay (T.splitOn "\n" text) (n - 1))
      marksOnly = blank || maybe True (\s -> isQuote s || isItem s || s.kind == ListShape) (lastMay found)
  pure (if marksOnly then reverse (dropWhile (\(q, l) -> not q && l /= n) (reverse held)) else held)
  where
    isQuote s = s.kind == QuoteShape
    isItem s = case s.kind of
      ItemShape _ _ -> True
      _ -> False

-- | The new quoted line is in the same quotations and items as the
-- cursor's line, and the blocks of the text before the cursor's top-level
-- block are read as before.
quotedLineKept :: Text -> Text -> Enter -> Property
quotedLineKept before after answer =
  let n = T.count "\n" before + 1
      text = textAfter before after answer
   in counterexample (show (containersAt (before <> after) n, containersAt text (n + 1))) $
        (map fst <$> containersAt text (n + 1)) === (map fst <$> containersAt (before <> after) n)

-- | The cursor's line, after the answer, is one container less deep, or
-- an item of the list around (which counts as deep as the item it is
-- next to); and it holds nothing after its marks.
wentUp :: Text -> Text -> Enter -> Property
wentUp before after answer =
  let n = T.count "\n" before + 1
      text = textAfter before after answer
      was = fromMaybe [] (containersAt (before <> after) n)
      now = fromMaybe [] (containersAt text n)
   in counterexample (show (was, now)) $
        conjoin
          [ counterexample "not one level up" (length now == length was - 1)
          , counterexample "text on the line" (not (inTextAt text n))
          ]

-- | The top-level blocks before the cursor's one, and after it, are the
-- same after the answer as before it. Going up may split the cursor's
-- block in two (a quotation left in its middle), so the blocks after it
-- are compared from the end. The footnotes, which the parser gathers at
-- the end of the page, are left out: the cursor may be in one.
blocksAroundKept :: Text -> Text -> Enter -> Property
blocksAroundKept before after answer = case (placed (before <> after), placed (textAfter before after answer)) of
  (Just old, Just new) ->
    let n = T.count "\n" before + 1
        own = fromMaybe n (lastMay [a | (a, b, _) <- old, a <= n, n <= b])
        above = [blk | (a, _, blk) <- old, a < own]
        below = [blk | (a, _, blk) <- old, a > n]
        blocks = [blk | (_, _, blk) <- new]
     in counterexample (show blocks) $
          take (length above) blocks === above
            .&&. reverse (take (length below) (reverse blocks)) === below
  _ -> counterexample "no parse" False
  where
    placed t = either (const Nothing) (\bs -> Just [(a, b, blk) | (Just (a, b), blk) <- bs, not (isFootnotes blk)]) (parseWithSpans t)
    isFootnotes = \case
      A.Footnotes _ -> True
      _ -> False

-- | The blocks that took attributes are the same after the answer, each
-- by the line it starts on (a new line counted out).
attributesKept :: Text -> Text -> Enter -> Property
attributesKept before after answer = case (parseShapes (before <> after), parseShapes (textAfter before after answer)) of
  (Right old, Right new) ->
    let n = T.count "\n" before + 2
        back l = case answer of
          NewLineWith _ | l == n -> 0
          NewLineWith _ | l > n -> l - 1
          _ -> l
     in sort [(back l, a) | (l, a) <- took new] === took old
  _ -> counterexample "no parse" False
  where
    took = sort . concatMap go
    go s = [(fst (linesOf s), s.attributes) | not (null s.attributes)] <> concatMap go s.inner

-- | Whether a line is in a paragraph, an item's text or a heading.
inTextAt :: Text -> Int -> Bool
inTextAt text n = either (const False) (\shapes -> any (\s -> s.kind == TextShape) (shapesAt shapes n)) (parseShapes text)

-- | A change since the editor's last check: at this per mille of the
-- text now (moved to the start of its line, or not), so many characters
-- of the text now stood in place of this text then. A deletion of up to
-- forty characters spans several starts; one at a line's start puts text
-- in at a start; one across lines replaces lines.
data Change = Change Int Bool Int Text
  deriving stock (Show)

instance Arbitrary Change where
  arbitrary =
    Change
      <$> choose (0, 1000)
      <*> elements [False, True]
      <*> frequency [(2, pure 0), (3, choose (1, 6)), (2, choose (7, 40))]
      <*> elements ["", "x", "\n", "\n\n", "- ", "> ", "    ", "```\n", "<div>\n", "<!--\n", "[[!notes]]\n", "[[/notes]]\n", "[^9]: x\n", "\n\nx\n\n"]

-- | No change since the last check.
unchanged :: Change
unchanged = Change 0 False 0 ""

-- | The text the editor hands to 'enter' for a cursor at the end of a line
-- of the text now, when the last check was of the text before a change:
-- the starts that check kept ('blockStarts', as offsets), mapped through
-- the change as CodeMirror maps a position after what is put in (several
-- onto one place, into a line), each with its line now, cut by
-- 'textRange'. Nothing past the limit.
handedOver :: Text -> Text -> Change -> Maybe (Text, Text)
handedOver before after (Change per toLine removed was) = handedOverAt before after at removed was
  where
    now = before <> after
    near = T.length now * per `div` 1000
    at = if toLine then T.length (T.dropWhileEnd (/= '\n') (T.take near now)) else near

-- | 'handedOver' for a change at an offset of the text now.
handedOverAt :: Text -> Text -> Int -> Int -> Text -> Maybe (Text, Text)
handedOverAt before after at removed was = do
  (begins, ends) <- textRange kept firstChange lineStart lineEnd (T.length now)
  pure (T.take (cursor - begins) (T.drop begins now), T.take (ends - cursor) (T.drop cursor now))
  where
    now = before <> after
    taken = min removed (T.length now - at)
    old = T.take at now <> was <> T.drop (at + taken) now
    changed = taken > 0 || not (T.null was)
    oldLines = T.splitOn "\n" old
    oldStarts = [sum (map ((+ 1) . T.length) (take (l - 1) oldLines)) | l <- either (const [1]) (blockStarts old) (parseShapes old)]
    mapped s
      | not changed || s < at = s
      | s > at + T.length was = s + taken - T.length was
      | otherwise = at + taken
    kept = [Kept {at = m, lineFrom = lineOf m, lineTo = m + T.length (lineAt m), flush = flushAt m} | s <- oldStarts, let m = mapped s]
    lineOf m = T.length (T.dropWhileEnd (/= '\n') (T.take m now))
    lineAt m = T.takeWhile (/= '\n') (T.drop (lineOf m) now)
    -- As the editor says it: text from the line's first column.
    flushAt m = maybe False (\(c, _) -> c /= ' ' && c /= '\t') (T.uncons (lineAt m))
    firstChange = if changed then at else T.length now
    cursor = T.length before
    lineStart = T.length (T.dropWhileEnd (/= '\n') before)
    lineEnd = cursor + T.length (T.takeWhile (/= '\n') after)

-- | The offsets of the blank lines below the cursor's line.
blankLinesBelow :: Text -> Text -> [Int]
blankLinesBelow before after =
  [ T.length before + 1 + sum (map ((+ 1) . T.length) (take i ls))
  | (i, l) <- zip [0 ..] ls
  , T.all (\c -> c == ' ' || c == '\t') l
  ]
  where
    ls = drop 1 (T.splitOn "\n" after)

-- | What the editor's Enter answers: 'enter' over what it hands over, or
-- a line break alone past the limit.
editorAnswer :: Text -> Text -> Change -> Enter
editorAnswer before after change = maybe NewLine (uncurry enter) (handedOver before after change)

-- | An answer, forced, and the bytes allocated to make it.
allocations :: (Show a) => a -> IO (a, Int64)
allocations answer = do
  start <- getAllocationCounter
  forced <- evaluate (answer `seq` T.length (show answer :: Text) `seq` answer)
  end <- getAllocationCounter
  pure (forced, start - end)

-- | A presentation of sixty slides of a hundred lines each, as the browser
-- suite's long deck: paragraphs, a list of ten items, a fence, a formula.
longDeck :: Text
longDeck = T.intercalate "\n" (["# Long", ""] <> concatMap slide [1 .. 60 :: Int])
  where
    slide n =
      take 100 $
        ["## Slide " <> show n, ""]
          <> para n 12
          <> [""]
          <> para n 11
          <> [""]
          <> ["- Item " <> show k <> " of slide " <> show n <> "." | k <- [1 .. 10 :: Int]]
          <> ["", "```haskell"]
          <> ["step" <> show i <> " x = x * " <> show n | i <- [0 .. 9 :: Int]]
          <> ["```", ""]
          <> concat [para n 9 <> [""] | _ <- [1 .. 6 :: Int]]
          <> repeat ""
    para :: Int -> Int -> [Text]
    para n k = ["A sentence of slide " <> show n <> ", line " <> show i <> "." | i <- [1 .. k]]

----------------------------------------------------------------------
-- Splits and numbers (#301)

-- | Whether Enter splits an item with the cursor on a line on which no
-- item starts: a later line of the item's first paragraph.
splitsOnLaterLine :: Text -> Text -> Bool
splitsOnLaterLine before after = case (enter before after, parseShapes (before <> after)) of
  (SplitWith _ _, Right shapes) -> not (any startsHere (shapesAt shapes n))
  _ -> False
  where
    n = T.count "\n" before + 1
    startsHere s = case s.kind of
      ItemShape _ _ -> fmap (fst . fst) (head s.parts) == Just n
      _ -> False

-- | Some places in the middle of each line of a text (after its first
-- character, in its middle, before its last), as the text before the
-- cursor and after it.
midLines :: Text -> [(Text, Text)]
midLines text =
  [ (T.take at text, T.drop at text)
  | (start, l) <- zip (scanl (\a l -> a + T.length l + 1) 0 ls) ls
  , T.length l >= 2
  , k <- ordNub [1, T.length l `div` 2, T.length l - 1]
  , let at = start + k
  ]
  where
    ls = T.splitOn "\n" text

-- | The parser reads a split as one: the top-level blocks are the old
-- ones with one list's item split in two, the two items' first
-- paragraphs' text joined by the blanks taken out being the old one's
-- (in plain text, 'A.plainText'), the rest of the old item in the new
-- one; and the cursor is where the new item's text starts.
splitReadBack :: Text -> Text -> Int -> Text -> Property
splitReadBack before after blanks start =
  let (before', after') = written before after (SplitWith blanks start)
      new = before' <> after'
      cut = T.takeWhileEnd (\c -> c == ' ' || c == '\t') (T.takeWhileEnd (/= '\n') before) <> T.take blanks after
      n = T.count "\n" before' + 1
      line = T.takeWhileEnd (/= '\n') before' <> T.takeWhile (/= '\n') after'
      column = T.length (T.takeWhileEnd (/= '\n') before') + 1
      -- Where the new item's text starts, as the parser reads it: its
      -- paragraph's start, or for a task, after its box and the blanks
      -- after it (the parser starts a task's paragraph after the box's
      -- @]@; in a numbered list a box is text).
      textAt c =
        let rest = T.drop (c - 1) line
            unboxed = if "[ ] " `T.isPrefixOf` rest then T.drop 4 rest else rest
         in [c, c + T.length rest - T.length (T.dropWhile (== ' ') unboxed)]
   in case (parseMarkdown (before <> after), parseMarkdown new, parseShapes new) of
        (Right old, Right blocks, Right shapes) ->
          counterexample (show (old, blocks)) (oneSplit cut old blocks)
            .&&. counterexample "the cursor is not where the new item's text starts" (any (\((l, c), _) -> l == n && column `elem` textAt c) (concatMap (.parts) (filter ((== TextShape) . (.kind)) (shapesAt shapes n))))
        _ -> counterexample "no parse" False

-- | Whether some blocks are others with one list's item split in two at a
-- cut ('splitReadBack'), everything else the same.
oneSplit :: Text -> [Block] -> [Block] -> Bool
oneSplit cut olds news =
  length olds == length news && case [(o, n) | (o, n) <- zip olds news, o /= n] of
    [(o, n)] -> inBlock o n
    _ -> False
  where
    inBlock o n = case (o, n) of
      (A.List kind items, A.List kind' items') | kind == kind' -> splitItems items items' || inItems items items'
      (A.BlockQuote bs, A.BlockQuote bs') -> oneSplit cut bs bs'
      (A.Alert k h bs, A.Alert k' h' bs') -> k == k' && h == h' && oneSplit cut bs bs'
      (A.Div a bs, A.Div a' bs') -> a == a' && oneSplit cut bs bs'
      (A.Spoiler h bs, A.Spoiler h' bs') -> h == h' && oneSplit cut bs bs'
      _ -> False
    inItems items items' =
      length items == length items' && case [(i, i') | (i, i') <- zip items items', i /= i'] of
        [(A.ListItem task bs, A.ListItem task' bs')] -> task == task' && oneSplit cut bs bs'
        _ -> False
    splitItems items items' =
      length items' == length items + 1
        && or
          [ take i items == take i items' && drop (i + 1) items == drop (i + 2) items' && pieces o a b
          | (i, o) <- zip [0 ..] items
          , Just a <- [atMay items' i]
          , Just b <- [atMay items' (i + 1)]
          ]
    pieces (A.ListItem task (p : rest)) (A.ListItem task1 [p1]) (A.ListItem task2 (p2 : rest')) =
      task1 == task && task2 == (False <$ task) && rest == rest' && joined p p1 p2
    pieces _ _ _ = False
    joined p p1 p2 = case (p, p1, p2) of
      (A.Para xs, A.Para ys, A.Para zs) -> A.plainText xs == A.plainText ys <> cut <> A.plainText zs
      (A.Plain xs, A.Plain ys, A.Plain zs) -> A.plainText xs == A.plainText ys <> cut <> A.plainText zs
      _ -> False

-- | What the editor's Enter does, over the texts it hands over, for a
-- text its last check read as it is: the edit for the text from the start
-- the check kept ('textRange'); where that edit says the list may go on
-- past the text, the edit for the longer text ('longerEnd') if its change
-- on the cursor's line is the same, else that change with no number
-- rewritten (web/src/editor.js's askEnter).
editorEdit :: Text -> Text -> Maybe Edit
editorEdit before after = uncurry askedOf =<< editorTexts before after

-- | The texts the editor hands over ('editorEdit'): from the start the
-- check kept to where 'textRange' ends it, and to where 'longerEnd' ends
-- the longer one, each as the text before the cursor, the text after it,
-- and whether it ends where the page does.
editorTexts :: Text -> Text -> Maybe ((Text, Text, Bool), (Text, Text, Bool))
editorTexts before after = do
  let now = before <> after
      len = T.length now
      ls = T.splitOn "\n" now
      offsets = scanl (\a l -> a + T.length l + 1) 0 ls
      kept =
        [ Kept {at, lineFrom = at, lineTo = at + T.length l, flush = maybe False (\(c, _) -> c /= ' ' && c /= '\t') (T.uncons l)}
        | s <- either (const [1]) (blockStarts now) (parseShapes now)
        , Just at <- [atMay offsets (s - 1)]
        , Just l <- [atMay ls (s - 1)]
        ]
      cursor = T.length before
      lineStart = T.length (T.dropWhileEnd (/= '\n') before)
      lineEnd = cursor + T.length (T.takeWhile (/= '\n') after)
      slice a b = T.take (b - a) (T.drop a now)
  (begins, ends) <- textRange kept len lineStart lineEnd len
  longer <- longerEnd kept len lineStart lineEnd len
  pure ((slice begins cursor, slice cursor ends, ends == len), (slice begins cursor, slice cursor longer, longer == len))

-- | What the editor's Enter does with the texts it hands over
-- ('editorTexts'), as web/src/editor.js's askEnter asks.
askedOf :: (Text, Text, Bool) -> (Text, Text, Bool) -> Maybe Edit
askedOf (given, taken, ends) (given', taken', ends') = do
  e <- edit ends given taken
  if not e.longer
    then pure e
    else case edit ends' given' taken' of
      Just again | not again.longer && again.change == e.change && again.cursor == e.cursor -> pure again
      _ -> pure (E.Edit e.change [] e.cursor False)

-- | A list for Enter on one of its items' first lines, or on the line
-- after it: what it is in (the outermost first), its items' bullet (none
-- for a numbered list) or its numbers' delimiter, its first number and
-- how the numbers go on, its items, whether it is loose, what is above it
-- and below it, and the cursor: the item, whether it is on the item's
-- later line (where the item has one, 'onLater'), and the column on its
-- line, from 0.
data Listed = Listed
  { holders :: [Around]
  , bullet :: Maybe Char
  , delimiter :: Char
  , start :: Int
  , numbering :: Numbering
  , entries :: [Entry]
  , loose :: Bool
  , above :: Bool
  , below :: Below
  , item :: Int
  , later :: Bool
  , column :: Int
  }
  deriving stock (Show)

-- | What a list is in: a quotation, an item of a list around (the list
-- after the item's first line and a blank line), or an item whose first
-- line the list starts on, with this mark, an item of the outer list after
-- it (@1. 1. a@ … @2. Outer next@): both lists start on that line.
data Around = Quoted | InItem | OnLine Text
  deriving stock (Eq, Show)

-- | How a numbered list's numbers go: one after another, all the same, or
-- with a gap after the second.
data Numbering = OneAfterAnother | AllTheSame | WithAGap
  deriving stock (Eq, Show)

-- | An item: a task's box (what is between its brackets), the words of
-- its first line, and what comes after that line.
data Entry = Entry
  { box :: Maybe Char
  , tokens :: [Token]
  , more :: More
  }
  deriving stock (Show)

-- | A word of an item's text: plain, or written over several characters
-- that a cut inside would break (code, math, a link, emphasis, an emoji's
-- name, an inline spoiler).
data Token = Word Text | Construct Text
  deriving stock (Show)

-- | What follows an item's first line in the item: nothing, a line
-- indented to its text, a lazy line, a second paragraph, a nested list.
data More = NoMore | Continued | LazyLine | Second | Nested
  deriving stock (Eq, Show)

-- | What is below the list: nothing, a paragraph, a list of another kind.
data Below = NothingBelow | ParagraphBelow | ListBelow
  deriving stock (Eq, Show)

instance Arbitrary Listed where
  arbitrary = do
    depth <- frequency [(3, pure 0), (3, pure 1), (1, pure 2)]
    holders <- vectorOf depth (frequency [(1, pure Quoted), (1, pure InItem), (3, OnLine <$> elements ["-", "*", "1.", "3)"])])
    bullet <- elements [Just '-', Just '*', Just '+', Nothing, Nothing, Nothing]
    delimiter <- elements ['.', ')']
    start <- elements [1, 1, 7, 8, 9, 98, 99]
    numbering <- frequency [(6, pure OneAfterAnother), (1, pure AllTheSame), (1, pure WithAGap)]
    k <- choose (1, 6)
    entries <- vectorOf k $ do
      box <- if isJust bullet then frequency [(3, pure Nothing), (1, Just <$> elements [' ', 'x'])] else pure Nothing
      tokens <- choose (1, 4) >>= \m -> vectorOf m (frequency [(4, Word <$> elements ["alpha", "beta", "gamma", "🎉", "naïve"]), (2, Construct <$> elements ["`code span`", "$x+y$", "[a link](u)", "*em text*", "**strong**", ":tada:", "||hidden||", "<span class=\"c-red\">red text</span>"])])
      more <- frequency [(4, pure NoMore), (1, pure Continued), (1, pure LazyLine), (1, pure Second), (1, pure Nested)]
      pure Entry {box, tokens, more}
    loose <- frequency [(4, pure False), (1, pure True)]
    above <- elements [False, True]
    below <- elements [NothingBelow, ParagraphBelow, ListBelow]
    item <- choose (0, k - 1)
    later <- frequency [(2, pure False), (1, pure True)]
    let listed column = Listed {holders, bullet, delimiter, start, numbering, entries, loose, above, below, item, later, column}
        line = cursorLine (listed 0)
        textStart = textStartOf (listed 0)
    listed <$> frequency [(6, choose (textStart + 1, max (textStart + 1) (T.length line - 1))), (2, pure (T.length line)), (1, choose (0, T.length line))]

-- | The number of a numbered list's item.
numberOf :: Listed -> Int -> Int
numberOf l i = case l.numbering of
  OneAfterAnother -> l.start + i
  AllTheSame -> l.start
  WithAGap -> l.start + i + (if i >= 2 then 3 else 0)

-- | An item's mark.
markOf :: Listed -> Int -> Text
markOf l i = maybe (show (numberOf l i) <> T.singleton l.delimiter) T.singleton l.bullet

-- | Whether a list starts on its outer item's first line.
onLine :: Around -> Bool
onLine = \case
  OnLine _ -> True
  _ -> False

-- | What a line of the list after its first starts with, for what the
-- list is in.
laterPrefix :: Listed -> Text
laterPrefix l = foldMap later l.holders
  where
    later = \case
      Quoted -> "> "
      InItem -> "  "
      OnLine m -> T.replicate (T.length m + 1) " "

-- | An item's first line, without what the list is in.
ownLine :: Listed -> Int -> Text
ownLine l i =
  let e = fromMaybe (Entry Nothing [] NoMore) (atMay l.entries i)
   in markOf l i <> " " <> boxOf e <> T.unwords (map tokenText e.tokens)

boxOf :: Entry -> Text
boxOf e = maybe "" (\c -> "[" <> T.singleton c <> "] ") e.box

tokenText :: Token -> Text
tokenText = \case
  Word t -> t
  Construct t -> t

-- | Whether the cursor is on its item's later line: the line after the
-- item's first, in the item's first paragraph, indented or lazy.
onLater :: Listed -> Bool
onLater l = l.later && maybe False (isJust . laterWords) (atMay l.entries l.item)

-- | The words of an item's later line in its first paragraph, where it
-- has one.
laterWords :: Entry -> Maybe Text
laterWords e = case e.more of
  Continued -> Just "more words"
  LazyLine -> Just "lazy words"
  _ -> Nothing

-- | Where the cursor's line is among the list's lines.
cursorIndex :: Listed -> Int
cursorIndex l = fromMaybe 0 (findIndex ((== Just l.item) . fst) (listedLines l)) + (if onLater l then 1 else 0)

-- | The cursor's line: its item's first line, or the line after it.
cursorLine :: Listed -> Text
cursorLine l = maybe "" snd (atMay (listedLines l) (cursorIndex l))

-- | Where the cursor's item's text starts on the cursor's line.
textStartOf :: Listed -> Int
textStartOf l =
  let e = fromMaybe (Entry Nothing [] NoMore) (atMay l.entries l.item)
   in case laterWords e of
        Just ws | onLater l -> T.length (cursorLine l) - T.length ws
        _ -> T.length (cursorLine l) - T.length (ownLine l l.item) + T.length (markOf l l.item) + 1 + T.length (boxOf e)

-- | The list's lines, each an item's first line (with the item) or not:
-- the list's own, in what it is in, the innermost first, with what is
-- above it and below it at the top level.
listedLines :: Listed -> [(Maybe Int, Text)]
listedLines l = top <> foldr wrap body l.holders <> bottom
  where
    top = map (Nothing,) (if l.above then ["Intro.", ""] else [])
    body = intercalate [(Nothing, "") | l.loose] [(Just i, ownLine l i) : map ((Nothing,) . fromMaybe "") (moreOf i e) | (i, e) <- zip [0 ..] l.entries]
    moreOf i e =
      let indent = T.replicate (T.length (markOf l i) + 1) " "
       in case e.more of
            NoMore -> []
            Continued -> [Just (indent <> "more words")]
            LazyLine -> [Just "lazy words"]
            Second -> [Nothing, Just (indent <> "second paragraph")]
            Nested -> [Just (indent <> "- child")]
    -- A line in a container: its prefix, and on a blank line the prefix
    -- without its spaces at the end.
    under p (i, t) = (i, if T.null t then T.stripEnd p else p <> t)
    wrap container ls = case container of
      Quoted -> map (under "> ") ls
      InItem -> [(Nothing, "- Outer"), (Nothing, "")] <> map (under "  ") ls
      OnLine m -> zipWith (\k x -> under (if k == (0 :: Int) then m <> " " else T.replicate (T.length m + 1) " ") x) [0 ..] ls <> [(Nothing, nextMark m <> " Outer next")]
    bottom = map (Nothing,) $ case l.below of
      NothingBelow -> []
      ParagraphBelow -> ["", "After."]
      ListBelow -> ["", if l.bullet == Just '-' then "+ other" else "- other"]

-- | The text before the cursor and after it.
cursorOf :: Listed -> (Text, Text)
cursorOf l =
  let ls = listedLines l
      n = cursorIndex l
      text = T.intercalate "\n" (map snd ls)
      at = sum (map ((+ 1) . T.length . snd) (take n ls)) + min l.column (T.length (cursorLine l))
   in (T.take at text, T.drop at text)

-- | Whether the cursor is in the text of its line, with text after it.
inText :: Listed -> Bool
inText l = l.column > textStartOf l && l.column < T.length (cursorLine l)

-- | Whether the cut is strictly inside a word written over several
-- characters (on an item's first line: its later line has plain words).
cutInside :: Listed -> Bool
cutInside l =
  inText l
    && not (onLater l)
    && or
      [ a < o && o < a + T.length t
      | (a, Construct t) <- zip (scanl (\p tok -> p + T.length (tokenText tok) + 1) 0 tokens) tokens
      ]
  where
    tokens = maybe [] (.tokens) (atMay l.entries l.item)
    o = l.column - textStartOf l

-- | Whether a numbered item's next number is one digit wider.
wider :: Listed -> Int -> Bool
wider l i = isNothing l.bullet && T.length (show (numberOf l i + 1) :: Text) > T.length (show (numberOf l i) :: Text)

-- | Whether an item has lines after its first that a wider number would
-- take out of it: a second paragraph or a nested list, indented to its
-- text.
indentedBelow :: Listed -> Int -> Bool
indentedBelow l i = maybe False (\e -> e.more `elem` [Second, Nested]) (atMay l.entries i)

-- | Whether the model splits the item: not where the new item's number is
-- one digit wider and the item has a line indented to its text below the
-- cursor's, which the new item would hold otherwise (a line of its
-- paragraph would become a lazy line, which Enter does not write). On the
-- item's later line, nothing of the item is below the cursor's line.
splits :: Listed -> Bool
splits l = inText l && not (cutInside l) && not (not (onLater l) && wider l l.item && maybe False (\e -> e.more `elem` [Continued, Second, Nested]) (atMay l.entries l.item))

-- | Whether the model adds an item: a split, or Enter at the end of an
-- item's last line, its only line or its later line.
adds :: Listed -> Bool
adds l = splits l || (l.column >= T.length (cursorLine l) && (onLater l || maybe False ((== NoMore) . (.more)) (atMay l.entries l.item)))

-- | The items after the cursor's.
laterItems :: Listed -> [Int]
laterItems l = [l.item + 1 .. length l.entries - 1]

-- | Whether the model renumbers the items after the new one: those of a
-- numbered list, numbered one after another from the cursor's item, none
-- of them with a wider number and lines indented below.
renumbers :: Listed -> Bool
renumbers l =
  adds l
    && isNothing l.bullet
    && not (null (laterItems l))
    && map (numberOf l) (laterItems l) == take (length (laterItems l)) [numberOf l l.item + 1 ..]
    && not (any (\i -> wider l i && indentedBelow l i) (laterItems l))

-- | Whether a number the model rewrites is one digit wider.
widens :: Listed -> Bool
widens l = any (wider l) (laterItems l)

-- | What the model writes: the text after Enter with the cursor at @‸@.
modelPress :: Listed -> Text
modelPress l
  | not (adds l) = before <> "\n‸" <> after
  | otherwise = T.intercalate "\n" (map (\(_, t) -> t) above' <> [kept, startLine] <> map renumbered below')
  where
    (before, after) = cursorOf l
    ls = listedLines l
    n = cursorIndex l
    above' = take n ls
    below' = drop (n + 1) ls
    line = cursorLine l
    e = fromMaybe (Entry Nothing [] NoMore) (atMay l.entries l.item)
    prefix = laterPrefix l
    next = maybe (show (numberOf l l.item + 1) <> T.singleton l.delimiter) T.singleton l.bullet
    start' = prefix <> next <> " " <> (if isJust e.box then "[ ] " else "")
    kept = T.take l.column line
    startLine = start' <> "‸" <> T.dropWhile (== ' ') (T.drop l.column line)
    renumbered (Just i, t)
      | renumbers l = prefix <> show (numberOf l i + 1) <> T.singleton l.delimiter <> T.drop (T.length prefix + T.length (markOf l i)) t
    renumbered (_, t) = t

----------------------------------------------------------------------
-- Lists and quotations written plainly

-- | A document ending in a line of text in containers written as
-- CommonMark reads them without doubt: no spaces before a mark, one after
-- it, no tab, numbers far from their limit; the cursor at its end.
newtype Plain = Plain Doc
  deriving stock (Show)

instance Arbitrary Plain where
  arbitrary = do
    stack <- plainLayers
    k <- choose (0, length stack - 1)
    let outer = take k stack
        inner = drop k stack
    above <- elements [[], [Chunk [] stack Text]]
    pure (Plain (Doc (above <> [Chunk (if null above then [] else outer) (if null above then stack else inner) Text])))
    where
      -- A box only on the innermost item; numbers that may start a list
      -- after a paragraph's line.
      plainLayers = do
        ls <- listOf1 plainLayer
        box <- elements [Nothing, Just " ", Just "x"]
        pure [if i == length ls - 1 then withBox box l else l | (i, l) <- zip [0 :: Int ..] ls]
      plainLayer = oneof [pure (Quote 0 (Spaces 1)), Item 0 <$> elements ["-", "*", "1.", "1)"] <*> pure (Spaces 1) <*> pure Nothing]
      withBox box = \case
        Item b m g _ -> Item b m g box
        q -> q

-- | What Enter at the end of such a document writes: the next item of its
-- innermost item, or the next line of its innermost quotation, after the
-- marks of the containers around.
plainExpected :: Doc -> Enter
plainExpected (Doc chunks) = case lastMay chunks of
  Just (Chunk kept fresh _) ->
    let stack = kept <> fresh
     in NewLineWith $ case lastMay stack of
          -- The parser reads no box after a number: a numbered item's
          -- next item has none.
          Just (Item b m g t) -> foldMap laterForm (initSafe stack) <> firstForm (Item b (nextMark m) g (if isBullet m then " " <$ t else Nothing))
          _ -> foldMap laterForm stack
  Nothing -> NewLine
  where
    isBullet m = m `elem` ["-", "*", "+"]
