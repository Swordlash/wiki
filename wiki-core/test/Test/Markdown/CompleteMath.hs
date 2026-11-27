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
import Test.QuickCheck (Arbitrary (..), Gen, Property, classify, conjoin, counterexample, elements, forAll, once, property, (.&&.), (===))
import Wiki.Core.Markdown.Ast (Block (..), Inline (..), ListItem (..), parseMarkdown)
import Wiki.Core.Markdown.Complete
import Wiki.Core.Math.KatexCommands (katexArguments, katexCommands, katexEnvironments)

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
      let inLink nodes marked = let at = placeAt marked in isJust (complete [mathSource OfferEnvironments (mathCommands [])] (readLine at.above) at {place = placeOf nodes})
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
        alikeEverywhere [mathSource OfferEnvironments (mathCommands [])] (caseAt (upTo <> "‸" <> rest) (T.takeWhileEnd (/= '\\') upTo) change)

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
    it "offers \\begin and \\end as commands like any other, \\begin before the environments, and no \\end of an environment" $ do
      (take 1 . map (.label) <$> listAt [] "$$\n\\beg‸") `shouldBe` Just ["\\begin"]
      (map (.label) <$> listAt [] "$$\n\\en‸") `shouldSatisfy` maybe False (elem "\\end")
      (filter ("{" `T.isInfixOf`) . map (.label) <$> listAt [] "$$\n\\en‸") `shouldBe` Just []

  describe "environments (#301)" $ do
    let environmentsAt marked = filter ("\\begin{" `T.isPrefixOf`) . map (.label) <$> listAt [] marked
        labelOf name = "\\begin{" <> name <> "}"
        displayOnly = ["CD", "align", "align*", "equation", "equation*", "gather", "gather*", "split"]
    it "offers KaTeX's environments that take no argument, in the list's order, those KaTeX reads only in display math only there" $ do
      sort <$> environmentsAt "$$\n\\be‸" `shouldBe` Just (sort (map (labelOf . fst) katexEnvironments))
      sort <$> environmentsAt "$\\be‸$" `shouldBe` Just (sort [labelOf name | (name, False) <- katexEnvironments])
      sort [name | (name, True) <- katexEnvironments] `shouldBe` displayOnly
      for_ ["$\\be‸$", "Text $a = \\be‸", "> $\\be‸$"] $ \marked ->
        (marked, any (`elem` map labelOf displayOnly) <$> environmentsAt marked) `shouldBe` (marked, Just False)
      for_ ["$$\n\\be‸\n$$", "```math\n\\be‸", "> $$\n> \\be‸"] $ \marked ->
        (marked, elem (labelOf "align*") <$> environmentsAt marked) `shouldBe` (marked, Just True)
      -- Shorter labels first, then by label, among the commands.
      (map (.label) <$> listAt [] "$\\begi‸$")
        `shouldBe` Just ["\\begin", "\\begin{cases}", "\\begin{dcases}", "\\begin{matrix}", "\\begin{rcases}", "\\begin{Bmatrix}", "\\begin{Vmatrix}", "\\begin{aligned}", "\\begin{bmatrix}", "\\begin{drcases}", "\\begin{matrix*}", "\\begin{pmatrix}", "\\begin{vmatrix}", "\\begin{Bmatrix*}", "\\begin{Vmatrix*}", "\\begin{bmatrix*}", "\\begin{gathered}", "\\begin{pmatrix*}", "\\begin{vmatrix*}", "\\begin{smallmatrix}"]
    it "finds them from \\b to \\begi, not after \\begin typed whole nor after its brace, where no list opens" $ do
      for_ ["$\\b‸$", "$\\be‸$", "$\\beg‸$", "$\\begi‸$"] $ \marked ->
        (marked, elem (labelOf "pmatrix") <$> environmentsAt marked) `shouldBe` (marked, Just True)
      listAt [] "$\\begin‸$" `shouldBe` Nothing
      listAt [] "$\\begin{pm‸$" `shouldBe` Nothing
    it "writes the environment's begin and end in place of the cursor's line, the cursor between them, Tab to after the end, in a formula and in display math" $ do
      walkAt [] "$\\be‸$" "\\begin{pmatrix}" `shouldBe` Just (["$\\begin{pmatrix}‸\\end{pmatrix}$", "$\\begin{pmatrix}\\end{pmatrix}‸$"], LineAsText "$\\be$" (1, 4))
      fst <$> walkAt [] "Text $x = \\beg‸ + 1$ b" "\\begin{cases}" `shouldBe` Just ["Text $x = \\begin{cases}‸\\end{cases} + 1$ b", "Text $x = \\begin{cases}\\end{cases}‸ + 1$ b"]
      fst <$> walkAt [] "$$\n\\b‸\n$$" "\\begin{align*}" `shouldBe` Just ["\\begin{align*}‸\\end{align*}", "\\begin{align*}\\end{align*}‸"]
      fst <$> walkAt [] "> $$\n> \\be‸" "\\begin{CD}" `shouldBe` Just ["> \\begin{CD}‸\\end{CD}", "> \\begin{CD}\\end{CD}‸"]
    it "still writes \\begin alone, the cursor after it" $
      written "$$\n\\beg‸" "\\begin" `shouldBe` Just ("\\begin‸", AsText)
    it "offers them whatever follows the cursor in the math but a brace or an \\end, an environment taking no argument; what is outside the math is not read"
      $ for_
        [ "$\\be‸ x$"
        , "$\\be‸12$"
        , "$\\be‸\\alpha$"
        , "$\\be‸ = 1$"
        , "$\\begin{pmatrix} \\be‸$"
        , -- Another command that starts with the letters of \end.
          "$\\begingroup \\be‸ + 1 \\endgroup$"
        , -- A no-break space is no white space to KaTeX: the brace after it
          -- is no environment's name.
          "$\\be‸\xa0{pmatrix}$"
        , "$$\n\\be‸\n$$\n\n\\end{pmatrix}"
        , -- Display math closed on the cursor's line, and a paragraph
          -- after it that names an \end: not the math's.
          "$$\\be‸$$\nSee \\end{pmatrix} in the text."
        , "$$\\be‸$$\n\nSee \\end{pmatrix} in the text."
        , "Text $$\\be‸$$ and \\end{pmatrix} after it."
        , -- A quotation's closing line, its mark taken off by the parser.
          "> $$\n> \\be‸\n> $$\n> And \\end{pmatrix} after it."
        , -- A long paragraph after the math, an \end past the completion's
          -- read limit.
          "$$\\be‸$$\n" <> T.intercalate "\n" (replicate 200 "Some more words on a line.") <> "\n\\end{pmatrix}"
        , -- Display math the page does not close yet, with nothing on the
          -- line after the cursor's, or no line at all.
          "$$\n\\begin{aligned}\n\\be‸"
        , "$$\n\\be‸"
        , "$$\n\\be‸\n\nMore text."
        , "> $$\n> \\be‸"
        ]
      $ \marked -> (marked, elem (labelOf "pmatrix") <$> environmentsAt marked) `shouldBe` (marked, Just True)
    it "offers none where the rest of the math, past white space and line ends, starts with a brace, or holds an \\end anywhere, matched by a \\begin before the cursor or not, or in a comment"
      $ for_
        [ "$\\be‸{pmatrix}$"
        , "$\\be‸ {pmatrix} a \\end{pmatrix}$"
        , "$\\be‸ a & b \\end{pmatrix}$"
        , "$\\begingroup \\be‸ + 1 \\endgroup \\end{x}$"
        , "$\\be‸ \\end$"
        , "$$\n\\be‸\na & b \\\\\nc & d\n\\end{pmatrix}\n$$"
        , "```math\n\\be‸\n\\end{cases}"
        , "$\\begin{matrix} a \\end{matrix} \\be‸ \\end{matrix}$"
        , -- The name on the next line: KaTeX reads \begin and its name
          -- over a line end.
          "$$\n\\be‸\n{pmatrix}\n$$"
        , -- A \begin in a comment before the cursor opens nothing.
          "$$\n% \\begin{pmatrix}\n\\be‸ a & b \\end{pmatrix}\n$$"
        , -- An \end that is not of the environment open before the cursor.
          "$$\n\\begin{aligned}\n\\be‸ a & b \\end{pmatrix}\n$$"
        , -- An \end that is: an entry is missing, never a wrong write.
          "$\\begin{pmatrix} \\be‸ \\end{pmatrix}$"
        , "$$\n\\begin{aligned}\nx &= \\be‸ \\\\\ny &= 2\n\\end{aligned}\n$$"
        , "$$\n\\begin{aligned}\nx &= \\be‸ % \\end{aligned}\n$$"
        ]
      $ \marked -> (marked, environmentsAt marked) `shouldBe` (marked, Just [])
    it "offers none in math the page does not close yet, display math or a formula, where the line after the cursor's holds anything, a quotation's mark too: the lines below are not in the math read"
      $ for_
        [ "$$\n\\be‸\na & b\n\\end{pmatrix}"
        , "$$\n\\be‸\n "
        , "> $$\n> \\be‸\n> a & b\n> \\end{pmatrix}"
        , "> $$\n> \\be‸\n>"
        , "Text $a + \\be‸\nmore text"
        , "$\\be‸\n\\end{pmatrix} $"
        ]
      $ \marked -> (marked, environmentsAt marked) `shouldBe` (marked, Just [])
    it "reads a formula that runs over lines whole, as the page does, the list opening on its first line: an \\end on a later line refuses" $ do
      for_
        [ "$\\be‸\n\\end{pmatrix}$"
        , "$\\be‸\na & b\n\\end{pmatrix}$"
        , "> $\\be‸\n> \\end{pmatrix}$"
        , "- $\\be‸\n  \\end{pmatrix}$"
        ]
        $ \marked -> (marked, environmentsAt marked) `shouldBe` (marked, Just [])
      for_ ["$\\be‸\na & b$", "Text $a + \\be‸", "$\\be‸\n\nNext paragraph."] $ \marked ->
        (marked, elem (labelOf "pmatrix") <$> environmentsAt marked) `shouldBe` (marked, Just True)
    it "offers none where the editor says a macro in force defines \\begin or \\end, which it decides from the macros' names (Test.Complete)" $ do
      let environmentsWith offered marked =
            let at = placeAt marked
             in filter ("\\begin{" `T.isPrefixOf`) . map (.label) . (.candidates) <$> complete [mathSource offered (mathCommands [])] (readLine at.above) at
      environmentsWith NoEnvironments "$$\n\\be‸\n$$" `shouldBe` Just []
      length <$> environmentsWith OfferEnvironments "$$\n\\be‸\n$$" `shouldBe` Just (length katexEnvironments)
      -- Bare \begin is still offered, by its name.
      (map (.label) <$> (let at = placeAt "$$\n\\beg‸\n$$" in (.candidates) <$> complete [mathSource NoEnvironments (mathCommands [])] (readLine at.above) at)) `shouldBe` Just ["\\begin"]
    it "offers none where a comment follows the cursor: KaTeX passes over it, and the name may come after" $
      for_ ["```math\n\\be‸ % a comment\n{pmatrix}\n```", "$$\n\\be‸% a comment\n$$"] $ \marked ->
        (marked, environmentsAt marked) `shouldBe` (marked, Just [])
    it "tells math the page does not close yet from math the page reads, a formula over lines too, with the page's TeX after the cursor" $ do
      let pageMathAt marked =
            let at = placeAt marked
                probed wanted = [Candidate {label = maybe "none" (T.strip . T.replace "\\Zqxjvmath" "‸") wanted.pageMath, skeleton = [], writing = AsText, action = Nothing, note = Nothing, origin = Nothing}]
             in map (.label) . (.candidates) <$> complete [probed] (readLine at.above) at
      for_ ["$$\n\\be‸", "$$\n\\be‸\na & b", "> $$\n> \\be‸\n> a", "$$\n\\begin{aligned}\n\\be‸", "$\\be‸", "Text $a + \\be‸", "Text $a + \\be‸\nmore text"] $ \marked ->
        (marked, pageMathAt marked) `shouldBe` (marked, Just ["none"])
      for_
        [ ("$$\n\\be‸\n$$", "‸")
        , ("$$\\be‸$$", "‸")
        , ("$\\be‸$", "‸")
        , ("```math\n\\be‸", "‸")
        , ("```math\n\\be‸\n```", "‸")
        , ("> $$\n> \\be‸\n> $$", "‸")
        , ("$\\be‸\n\\end{pmatrix}$", "‸\n\\end{pmatrix}")
        , ("> $\\be‸\n> \\end{pmatrix}$", "‸\n\\end{pmatrix}")
        ]
        $ \(marked, tex) -> (marked, pageMathAt marked) `shouldBe` (marked, Just [tex])
    it "offers none where the parser would read the math otherwise with environments written, a name of letters or one with a star" $ do
      let at = placeAt "$$\n\\be‸\n$$"
          -- Parsers that read no math in a probe that holds a text, where
          -- the parser itself reads the same math.
          without text probe = readLine at.above (if any (text `T.isInfixOf`) probe.written then Probe {from = probe.from, written = ["No math here."]} else probe)
          offered readOf = filter ("\\begin{" `T.isPrefixOf`) . map (.label) . (.candidates) <$> complete [mathSource OfferEnvironments (mathCommands [])] readOf at
      offered (without "\\begin{") `shouldBe` Just []
      offered (without "*}") `shouldBe` Just []
      -- The first name of letters alone offered there.
      offered (without "{CD}Zqxjvmath") `shouldBe` Just []
      length <$> offered (readLine at.above) `shouldBe` Just (length katexEnvironments)

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
        , -- A formula over lines, read whole: the arguments on the next
          -- line (#392's review).
          "$\\fr‸\n{a}{b}$"
        , -- A no-break space is no white space to KaTeX, which takes it as
          -- the argument (#392's review).
          "$\\fr‸\xa0$"
        , "> $\\fr‸\n> {a}{b}$"
        , "$$\n\\fr‸ % a comment\n{a}{b}\n$$"
        ]
        $ \marked -> (marked, walkAt [] marked "\\frac") `shouldBe` (marked, Just (["\\frac‸"], AsText))
      -- The rest of the math, a quotation's marks taken off by the parser:
      -- its end follows, and the braces are written.
      (snd <$> walkAt [] "> $$\n> \\fr‸\n> $$" "\\frac") `shouldSatisfy` lineAsText
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
          writing readOf = (.writing) <$> (find ((== "\\frac") . (.label)) . (.candidates) =<< complete [mathSource OfferEnvironments (mathCommands [])] readOf at)
      writing braceless `shouldBe` Just AsText
      writing (readLine at.above) `shouldBe` Just (LineAsText "$\\fr$" (1, 4))
    modifyMaxSuccess (`div` 2) . longTexts . prop "writes braces only where the whole text, written and its places filled, reads as it did with the command alone, the braces in the math, over long texts" $
      forAll genPlace bracesReadBack
    -- The property meets every environment in math only now and then: here
    -- each one is checked on its own, in a formula and in display math.
    it "writes each environment so that the whole text reads as it did, its begin and end in the math, in a formula, display math, a math fence and a quotation (#301)"
      $ once
      $ conjoin
        [ (marked, length . filter ("\\begin{" `T.isPrefixOf`) . map (.label) <$> listAt [] marked) === (marked, Just count) .&&. bracesReadBack place'
        | (place', count) <- [(("$\\beg", "$"), 19), (("Text $a = \\beg", " + 1$ b"), 19), (("$$\n\\beg", "\n$$"), 27), (("```math\n\\beg", "\n```"), 27), (("> $$\n> \\beg", "\n> $$"), 27)]
        , let marked = fst place' <> "‸" <> snd place'
        ]
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
completeAt macros marked = let at = placeAt marked in complete [mathSource OfferEnvironments (mathCommands macros)] (readLine at.above) at

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
--
-- An environment's entry (#301) the same: its begin and its end written,
-- the word between them, every name offered checked on its own, those
-- with a star too ('Wiki.Core.Markdown.Complete.mathSource' asks the
-- parser once for them all, with one name of letters and one with a
-- star written).
bracesReadBack :: (Text, Text) -> Property
bracesReadBack (upTo, rest) =
  counterexample (toS (upTo <> "‸" <> rest))
    $ classify inMath "in math as the text is"
    $ classify (any (isJust . environmentOf) offered) "an environment offered"
    $ conjoin
      [ counterexample (toS c.label) $
          fst (skeletonText c.skeleton) === lineBefore <> written' <> T.takeWhile (/= '\n') rest
            .&&. (if inMath then shownReading (upToCommand <> withFill <> rest) withFill === shownReading baseline "" else property True)
      | c <- offered
      , let n = pairsOf c
      , n > 0
      , let (written', withFill) = case environmentOf c of
              Just name -> (c.label <> "\\end{" <> name <> "}", c.label <> fill <> "\\end{" <> name <> "}")
              Nothing -> (c.label <> T.concat (replicate n "{}"), filled c.label n)
      ]
  where
    offered = fromMaybe [] (listAt [] (upTo <> "‸" <> rest))
    environmentOf c = T.stripPrefix "\\begin{" c.label >>= T.stripSuffix "}"
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
