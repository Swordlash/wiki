-- | The completion of commands after a backslash in math
-- ("Wiki.Core.Markdown.Complete", WP5.124 (e) of docs/PLAN.md): where it
-- opens, with the wiki's parser as the oracle; what it offers (KaTeX's
-- commands, read from its tables, and the macros of the page, its spaces
-- and the wiki, the nearer shadowing the farther); and what choosing one
-- writes.
module Test.Markdown.CompleteMath (spec) where

import Data.Char (isAsciiLower, isAsciiUpper)
import Data.List (lookup)
import Data.Text qualified as T
import Protolude
import Test.Cases (longTexts)
import Test.Hspec
import Test.Hspec.QuickCheck (modifyMaxSuccess, prop)
import Test.Markdown.CompleteStarts (alikeEverywhere, caseAt)
import Test.QuickCheck (Arbitrary (..), Gen, Property, classify, conjoin, counterexample, elements, forAll, property, (.&&.), (===))
import Wiki.Core.Markdown.Ast (Block (..), Inline (..), ListItem (..), parseMarkdown)
import Wiki.Core.Markdown.Complete
import Wiki.Core.Math.KatexCommands (katexArguments, katexCommands)

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
    it "writes a command that takes no argument by its name alone, the cursor after it, as text" $ do
      written "$\\alp‸$" "\\alpha" `shouldBe` Just ("\\alpha‸", AsText)
      written "> $$\n> \\beg‸" "\\begin" `shouldBe` Just ("\\begin‸", AsText)
      written "$\\left( x \\ri‸" "\\right" `shouldBe` Just ("\\right‸", AsText)
    it "writes a command that takes arguments with a pair of braces for each, in place of the cursor's line: the cursor in the first pair, Tab to the next, and on to after the last (#301)" $ do
      walkAt [] "$\\fr‸$" "\\frac" `shouldBe` Just (["$\\frac{‸}{}$", "$\\frac{}{‸}$", "$\\frac{}{}‸$"], LineAsText "$\\fr$" (1, 4))
      fst <$> walkAt [] "Text $a + \\sq‸$ b" "\\sqrt" `shouldBe` Just ["Text $a + \\sqrt{‸}$ b", "Text $a + \\sqrt{}‸$ b"]
      fst <$> walkAt [] "$$\n\\tex‸" "\\text" `shouldBe` Just ["\\text{‸}", "\\text{}‸"]
      fst <$> walkAt [] "- $\\frac{\\overl‸}{2}$" "\\overline" `shouldBe` Just ["- $\\frac{\\overline{‸}}{2}$", "- $\\frac{\\overline{}‸}{2}$"]
      -- Optional arguments, in brackets, are not written.
      fst <$> walkAt [] "$a \\xrightarr‸$" "\\xrightarrow" `shouldBe` Just ["$a \\xrightarrow{‸}$", "$a \\xrightarrow{}‸$"]
    it "writes the braces where nothing after the cursor can be an argument: the end of the math, of a group, of a cell or a row, a superscript, the end of a pair of delimiters"
      $ for_
        [ "$\\frac{\\fr‸ }{}$"
        , "$\\fr‸^2$"
        , "$a = \\fr‸_1$"
        , "$\\frac{\\fr‸}{}$"
        , "$\\sqrt[\\fr‸]{x}$"
        , "$\\begin{matrix} \\fr‸ & b \\end{matrix}$"
        , "$\\fr‸ \\\\ b$"
        , "$\\left( \\fr‸ \\right)$"
        , "$\\fr‸"
        , "$$\n\\fr‸"
        , "$$\n\\fr‸\n$$"
        , "$$\n\\fr‸\n\n$$"
        , "$$\n\\begin{aligned}\n\\fr‸\n\\end{aligned}\n$$"
        , "```math\n\\fr‸\n```"
        ]
      $ \marked -> (marked, snd <$> walkAt [] marked "\\frac") `shouldSatisfy` (lineAsText . snd)
    it "writes the name alone where what follows the cursor may be an argument the reader meant: braces, a bracket, a digit, a letter, a command, any other character, and on the next line in display math" $ do
      for_
        [ "$\\fr‸{a}{b}$"
        , "$\\fr‸ {a}{b}$"
        , "$\\fr‸12$"
        , "$\\fr‸ x$"
        , "$\\fr‸\\alpha\\beta$"
        , "$\\fr‸\\rightarrow$"
        , "$\\fr‸+1$"
        , "$\\fr‸ = 1$"
        , "$$\n\\fr‸\n{a}{b}\n$$"
        , "> $$\n> \\fr‸\n> $$"
        ]
        $ \marked -> (marked, walkAt [] marked "\\frac") `shouldBe` (marked, Just (["\\frac‸"], AsText))
      (snd <$> walkAt [] "$\\sq‸[3]{x}$" "\\sqrt") `shouldBe` Just AsText
    it "writes the name alone for a command whose arguments KaTeX's tables do not give for certain: a colour, a delimiter's size, a macro of KaTeX's that is a function" $
      for_ [("$\\colo‸$", "\\color"), ("$\\textcolo‸$", "\\textcolor"), ("$\\bi‸$", "\\big"), ("$\\lef‸$", "\\left"), ("$\\operatorn‸$", "\\operatorname"), ("$\\p‸$", "\\pu")] $ \(marked, label) ->
        (label, snd <$> walkAt [] marked label) `shouldBe` (label, Just AsText)
    it "writes a macro's braces as its definition gives them, and the name alone for a macro of a nearer definition that gives none for certain, KaTeX's command or a farther macro of its name shadowed" $ do
      fst <$> walkAt [cmdTaking "\\abs" "the wiki" 1] "$\\ab‸$" "\\abs" `shouldBe` Just ["$\\abs{‸}$", "$\\abs{}‸$"]
      walkAt [cmd "\\frac" "this page"] "$\\fr‸$" "\\frac" `shouldBe` Just (["\\frac‸"], AsText)
      walkAt [cmd "\\vect" "this page", cmdTaking "\\vect" "the wiki" 1] "$\\ve‸$" "\\vect" `shouldBe` Just (["\\vect‸"], AsText)
    it "writes the name alone where the parser would read the math otherwise with the braces written" $ do
      let at = placeAt "$\\fr‸$"
          -- A parser that reads no math in a probe that holds a brace,
          -- where the parser itself reads the same math.
          braceless probe = readLine at.above (if any ("{" `T.isInfixOf`) probe.written then Probe {from = probe.from, written = ["No math here."]} else probe)
          writing readOf = (.writing) <$> (find ((== "\\frac") . (.label)) . (.candidates) =<< complete [mathSource (mathCommands [])] readOf at)
      writing braceless `shouldBe` Just AsText
      writing (readLine at.above) `shouldBe` Just (LineAsText "$\\fr$" (1, 4))
    modifyMaxSuccess (`div` 2) . longTexts . prop "writes braces only where the whole text, written and its places filled, reads as it did with the command alone, the braces in the math, over long texts" $
      forAll genPlace bracesReadBack
    prop "writes as many pairs of braces as KaTeX's tables give a command, and its name alone for every other" $
      forAll (elements [c | c <- katexCommands, T.length c > 2]) $ \name ->
        case find ((== name) . (.label)) =<< listAt [] ("$" <> T.dropEnd 1 name <> "‸$") of
          Nothing -> classify True "not listed" True
          Just c -> counterexample (toS name) (pairsOf c === fromMaybe 0 (lookup name katexArguments))
    it "chooses with Enter only once a letter follows the backslash" $
      (.enter) <$> completeAt [] "$\\fr‸$" `shouldBe` Just (EnterChoosesAfter 2)
  where
    cmd name origin = MathCommand {name, origin, arguments = 0}
    cmdTaking name origin arguments = MathCommand {name, origin, arguments}
    lineAsText = \case
      Just (LineAsText _ _) -> True
      _ -> False

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

-- | What choosing an entry writes, with the cursor at each place in the
-- order Tab goes through them (the last where the writing goes on), and
-- how it is written: a command's name as text, or, with its braces, in
-- place of the cursor's line.
walkAt :: [MathCommand] -> Text -> Text -> Maybe ([Text], Writes)
walkAt macros marked label' = do
  c <- find ((== label') . (.label)) =<< listAt macros marked
  let (text, ranges) = skeletonText c.skeleton
  Just ([T.take s text <> "‸" <> T.drop s text | (_, (s, _)) <- ranges], c.writing)

-- | How many pairs of braces an entry writes: its places to fill but the
-- last, where the writing goes on.
pairsOf :: Candidate -> Int
pairsOf c = case c.writing of
  LineAsText _ _ -> length [() | Field n _ <- c.skeleton, n /= 0]
  _ -> 0

-- | Whether every entry with braces at a place writes what it should, and
-- the whole text, written with a word in each pair, reads as the text with
-- a word in place of the command did: the same blocks, the braces inside
-- the same math. Asked where the text read that word in math (a formula
-- closed, display math): a formula still being typed is not math yet, and
-- its check is the probe's ('Wiki.Core.Markdown.Complete.complete').
bracesReadBack :: (Text, Text) -> Property
bracesReadBack (upTo, rest) =
  counterexample (toS (upTo <> "‸" <> rest))
    $ classify inMath "in math as the text is"
    $ conjoin
      [ counterexample (toS c.label) $
          fst (skeletonText c.skeleton) === lineBefore <> c.label <> T.concat (replicate n "{}") <> T.takeWhile (/= '\n') rest
            .&&. (if inMath then shownReading (upToCommand <> filled c.label n <> rest) (filled c.label n) === shownReading baseline "" else property True)
      | c <- fromMaybe [] (listAt [] (upTo <> "‸" <> rest))
      , let n = pairsOf c
      , n > 0
      ]
  where
    upToCommand = T.dropWhileEnd (\c -> isAsciiLower c || isAsciiUpper c) upTo & T.dropEnd 1
    lineBefore = T.takeWhileEnd (/= '\n') upToCommand
    word = "Zqxjvword"
    fill = "Zqxjvfill"
    baseline = upToCommand <> "\\" <> word <> rest
    filled name n = name <> T.concat (replicate n ("{" <> fill <> "}"))
    -- The parser's reading of a text as shown, with what was written in
    -- place of the command shown as the word.
    shownReading text written' = case parseMarkdown text of
      Right blocks -> (if T.null written' then identity else T.replace (shown written') (shown ("\\" <> word))) (show blocks)
      Left _ -> "no reading"
    shown t = T.dropEnd 1 (T.drop 1 (show t))
    inMath = case parseMarkdown baseline of
      Right blocks -> any (holdsInMath word) blocks
      Left _ -> False

-- | Whether a block holds a word in math: a formula's or display math's.
holdsInMath :: Text -> Block -> Bool
holdsInMath w = \case
  MathBlock tex -> w `T.isInfixOf` tex
  Para xs -> any inline xs
  Plain xs -> any inline xs
  BlockQuote bs -> any (holdsInMath w) bs
  List _ items -> or [any (holdsInMath w) bs | ListItem _ bs <- items]
  _ -> False
  where
    inline = \case
      Math _ tex -> w `T.isInfixOf` tex
      Emph xs -> any inline xs
      Strong xs -> any inline xs
      _ -> False

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
