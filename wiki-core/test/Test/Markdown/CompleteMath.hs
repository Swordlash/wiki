-- | The completion of commands after a backslash in math
-- ("Wiki.Core.Markdown.Complete", WP5.124 (e) of docs/PLAN.md): where it
-- opens, with the wiki's parser as the oracle; what it offers (KaTeX's
-- commands, read from its tables, and the macros of the page, its spaces
-- and the wiki, the nearer shadowing the farther); and what choosing one
-- writes.
module Test.Markdown.CompleteMath (spec) where

import Data.Char (isAsciiLower, isAsciiUpper)
import Data.Text qualified as T
import Protolude
import Test.Cases (longTexts)
import Test.Hspec
import Test.Hspec.QuickCheck (modifyMaxSuccess, prop)
import Test.Markdown.CompleteStarts (alikeEverywhere, caseAt)
import Test.QuickCheck (Arbitrary (..), Gen, Property, counterexample, elements, forAll, (===))
import Wiki.Core.Markdown.Ast (Block (..), Inline (..), ListItem (..), parseMarkdown)
import Wiki.Core.Markdown.Complete

spec :: Spec
spec = describe "Wiki.Core.Markdown.Complete, in math" $ do
  describe "where a list opens" $ do
    it "opens in a formula, closed or being typed, in braces, in display math, in a math fence, and in emphasis" $
      map opens ["$\\fr‸$", "$\\fr‸", "Text $a + \\fr‸", "Text $a + \\fr‸ b$ c", "$$\n\\fr‸", "$$\n\\fr‸\n$$", "```math\n\\fr‸", "> $\\fr‸", "- $\\fr‸$", "> [!NOTE]\n> $\\fr‸$", "*so $\\fr‸$ is*", "**so $\\fr‸", "$\\frac{\\fr‸}{}", "Text $\\frac{\\fr‸}{} and more", "$\\sqrt{\\fr‸", "$\\frac{a}{\\sqrt{\\fr‸", "a } b $\\sqrt{\\fr‸", "Text { $\\fr‸", "See `{` $\\sqrt{\\fr‸", "$$\n\\frac{\\fr‸", "$$\n\\frac{a}{\n\\fr‸", "$$\n\\frac{a}{\n\\sqrt{\\fr‸", "$\\sqrt{\\$ + \\fr‸", "$\\fr‸ + \\$5"]
        `shouldBe` replicate 24 True
    it "opens nowhere else: outside math, in code, a heading, a table, a link's or an image's text, a formula over lines, after a line break's two backslashes, after a formula closed already" $
      map opens ["\\fr‸", "Text \\fr‸", "`$\\fr‸`", "```\n\\fr‸", "# $\\fr‸$", "| a |\n| - |\n| $\\fr‸$ |", "[$\\fr‸$](https://example.com)", "![$\\fr‸$](a.png)", "[*$\\fr‸$*](https://example.com)", "$a\n\\fr‸$", "$a \\\\fr‸", "Costs $5$ and \\fr‸", "$\\fr‸$\r", "Costs $5 and \\fr‸\nTitle\n===", "$\\fr‸ac{1}{2}$"]
        `shouldBe` replicate 15 False
    it "opens nothing in math in a link's or an image's text, the syntax tree's reading, which a reference defined past what is read makes one" $ do
      let inLink nodes marked = let at = placeAt marked in isJust (complete [mathSource (mathCommands [])] (readLine at.above) at {place = placeOf nodes})
      inLink ["TeXMath", "Link", "Paragraph", "Document"] "[$\\fr‸$][far]\n\nMore." `shouldBe` False
      inLink ["TeXMath", "Image", "Paragraph", "Document"] "![$\\fr‸$][far]\n\nMore." `shouldBe` False
      inLink ["TeXMath", "Paragraph", "Document"] "Text $\\fr‸$" `shouldBe` True
      inLink ["CodeText", "FencedCode", "Document"] "```math\n\\fr‸" `shouldBe` True
    modifyMaxSuccess (* 10) . longTexts . prop "opens exactly where the parser reads the command in plain math, once the math being typed is closed, over long texts" $
      forAll genPlace inMathAsParsed
    -- WP5.124 (f): the same places, read from the starts of a check
    -- ("Test.Markdown.CompleteStarts"' modes): the answer is the whole
    -- text's.
    modifyMaxSuccess (`div` 2) . prop "opens the list the whole text opens, read from a start of a block" $
      forAll ((,) <$> genPlace <*> arbitrary) $ \((upTo, rest), change) ->
        alikeEverywhere [mathSource (mathCommands [])] (caseAt (upTo <> "‸" <> rest) (T.takeWhileEnd (/= '\\') upTo) change)

  describe "what it offers" $ do
    it "puts the nearer macro before the farther and before KaTeX's command of its name, which it shadows" $ do
      let commands = mathCommands [cmd "\\frac" "this page", cmd "\\R" "Team", cmd "\\R" "the wiki"]
      [c.origin | c <- commands, c.name == "\\frac"] `shouldBe` ["this page"]
      [c.origin | c <- commands, c.name == "\\R"] `shouldBe` ["Team"]
      [c.origin | c <- commands, c.name == "\\sqrt"] `shouldBe` ["KaTeX"]
    it "lists the commands that start with the letters, the shorter first, with where each comes from" $ do
      (take 3 . map (\c -> (c.label, c.note)) <$> listAt [cmd "\\fraction" "this page"] "$\\fr‸$")
        `shouldBe` Just [("\\frac", Just "KaTeX"), ("\\frak", Just "KaTeX"), ("\\frown", Just "KaTeX")]
      (map (.label) <$> listAt [cmd "\\fraction" "this page"] "$\\fract‸$") `shouldBe` Just ["\\fraction"]
    it "opens no list where the letters name a command already, so that Enter after it is a new line" $
      listAt [] "$$\n\\alpha‸" `shouldBe` Nothing
    it "offers \\begin and \\end as commands like any other, and no environment" $ do
      (map (.label) <$> listAt [] "$$\n\\beg‸") `shouldBe` Just ["\\begin"]
      (map (.label) <$> listAt [] "$$\n\\en‸") `shouldSatisfy` maybe False (elem "\\end")

  describe "what choosing writes" $ do
    it "writes the command's name alone, the cursor after it, as text" $ do
      written "$\\fr‸$" "\\frac" `shouldBe` Just ("\\frac‸", AsText)
      written "$\\alp‸$" "\\alpha" `shouldBe` Just ("\\alpha‸", AsText)
      written "> $$\n> \\beg‸" "\\begin" `shouldBe` Just ("\\begin‸", AsText)
      written "$\\left( x \\ri‸" "\\right" `shouldBe` Just ("\\right‸", AsText)
    it "chooses with Enter only once a letter follows the backslash" $
      (.enter) <$> completeAt [] "$\\fr‸$" `shouldBe` Just (EnterChoosesAfter 2)
  where
    cmd name origin = MathCommand {name, origin}

-- | The place a marked text gives: the lines above the cursor's, the
-- cursor's before the mark and after it, the lines below.
placeAt :: Text -> At
placeAt marked =
  At {place = InText, above = initDef [] ls, before = fromMaybe "" (lastMay ls), after = T.takeWhile (/= '\n') (T.drop 1 rest), below = drop 1 (T.splitOn "\n" (T.drop 1 rest)), cutBelow = False}
  where
    (upTo, rest) = T.breakOn "‸" marked
    ls = T.splitOn "\n" upTo

-- | The list at a marked text, with these macros.
completeAt :: [MathCommand] -> Text -> Maybe Completion
completeAt macros marked = let at = placeAt marked in complete [mathSource (mathCommands macros)] (readLine at.above) at

listAt :: [MathCommand] -> Text -> Maybe [Candidate]
listAt macros marked = (.candidates) <$> completeAt macros marked

opens :: Text -> Bool
opens = isJust . completeAt []

-- | What choosing an entry writes in place of the backslash and the
-- letters, where the cursor goes after it marked, and how.
written :: Text -> Text -> Maybe (Text, Writes)
written marked label = do
  c <- find ((== label) . (.label)) =<< listAt [] marked
  let (text, ranges) = skeletonText c.skeleton
  (s, _) <- snd <$> head ranges
  Just (T.take s text <> "‸" <> T.drop s text, c.writing)

-- | A place for a backslash and letters: what comes before it in its block
-- (math opened or not, in containers or not), and what closes it, if
-- anything; with a text longer than every bound before or after it.
genPlace :: Gen (Text, Text)
genPlace = do
  opening <- elements ["", "Text ", "$", "Text $a + ", "costs $5 and ", "$$\n", "```math\n", "```\n", "> $", "- $", "# $", "`$", "$a$ and ", "$a\n", "*so $", "[$", "![$", "$\\frac{", "Text $a \\sqrt{", "$\\frac{a}{\\sqrt{", "$\\{", "a } b $\\sqrt{", "$$\n\\frac{a}{\n", "$$ \\sqrt{\n", "$$\n\\left( \\frac{\n", "$\\sqrt{\\$ + "]
  closing <- elements ["", "$", " b$", "$ more", "\n$$", "\n```", "`", " and $b$", "$ is*", "$](x)", "}{}", "} b", "}{b}$", " + \\$5", "\\$ b$"]
  letters <- elements ["fr", "alp", "beg", "x"]
  intro <- elements ["", "Intro.\n\n", longText <> "\n\n"]
  outro <- elements ["", "\n\nAfter.", "\n\n" <> longText]
  pure (intro <> opening <> "\\" <> letters, closing <> outro)

-- | A text longer than every bound of the completion.
longText :: Text
longText = T.intercalate "\n\n" (replicate 40 (T.replicate 6 "Some more words. "))

-- | Whether the list opens at a place exactly where the wiki's parser reads
-- a command written there in plain math: a formula on one line among a
-- paragraph's own text (in emphasis too, not in a link's or an image's
-- text), or display math, at the top of the page or in a
-- list's item or a quotation; as the text is, or, where it reads none,
-- once the math is closed after the cursor (a @$@ at the line's end, after
-- up to three braces, where no @$@ follows on the line; a line of @$$@).
inMathAsParsed :: (Text, Text) -> Property
inMathAsParsed (upTo, rest) =
  counterexample (toS (upTo <> "‸" <> rest)) $
    opens (upTo <> "‸" <> rest) === (settled && readsMath rest || settled && (null (afterDelimiters "$" lineAfter) && any (\k -> readsMath (lineAfter <> T.replicate k "}" <> "$")) [0 .. 3]) || settled && any (\k -> readsMath (lineAfter <> T.replicate k "}" <> "\n$$")) [0 .. 3])
  where
    lineAfter = T.takeWhile (/= '\n') rest
    -- The completion reads 2,000 characters after the cursor: where the
    -- text goes on further, the block that holds the command must end
    -- within them, the parser reading a block after it there.
    settled =
      T.length rest <= 2000 || case parseMarkdown (commandAt <> T.take 2000 rest) of
        Right blocks -> case break holdsWord (concatMap inOrder blocks) of
          (_, _ : _ : _) -> True
          _ -> False
        Left _ -> True
    inOrder b =
      b : case b of
        BlockQuote bs -> concatMap inOrder bs
        List _ items -> concat [concatMap inOrder bs | ListItem _ bs <- items]
        _ -> []
    holdsWord = \case
      MathBlock tex -> word `T.isInfixOf` tex
      Para xs -> any holdsInline xs
      Plain xs -> any holdsInline xs
      CodeBlock _ _ code -> word `T.isInfixOf` code
      _ -> False
    holdsInline = \case
      Math _ tex -> word `T.isInfixOf` tex
      Str t -> word `T.isInfixOf` t
      Emph xs -> any holdsInline xs
      Strong xs -> any holdsInline xs
      _ -> False
    word = "Zqxjvoracle"
    commandAt = T.dropWhileEnd (\c -> isAsciiLower c || isAsciiUpper c) upTo <> word
    readsMath following = case parseMarkdown (commandAt <> following) of
      Right blocks -> any plainMath blocks
      Left _ -> False
    plainMath = \case
      MathBlock tex -> word `T.isInfixOf` tex
      Para xs -> any ownMath xs
      Plain xs -> any ownMath xs
      BlockQuote bs -> any plainMath bs
      List _ items -> or [any plainMath bs | ListItem _ bs <- items]
      _ -> False
    ownMath = \case
      Math _ tex -> word `T.isInfixOf` tex && not ("\n" `T.isInfixOf` tex)
      Emph xs -> any ownMath xs
      Strong xs -> any ownMath xs
      _ -> False
