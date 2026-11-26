-- | The editor's answer to CodeMirror's question about the list of the
-- wiki's own Markdown ("Wiki.App.Editor.Complete"): against the text the
-- editor holds, from the sources it holds, in the shape
-- @web/src/editor.js@ reads; and the links to pages it writes
-- ("Wiki.App.Route"'s 'pageLinkText'), read back by the wiki's parser and
-- 'resolvePageLink' over generated trees of pages.
module Test.Complete (spec) where

import Data.Aeson (Value (..), object, (.=))
import Data.Aeson.KeyMap qualified as KeyMap
import Data.ByteString qualified as BS
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet qualified as IntSet
import Data.List (lookup, nub)
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.Time (UTCTime (..), fromGregorian)
import Protolude
import System.Mem (getAllocationCounter, setAllocationCounter)
import Test.Hspec
import Test.Hspec.QuickCheck (modifyMaxSuccess, prop)
import Test.QuickCheck (Gen, Property, checkCoverage, choose, conjoin, counterexample, cover, elements, forAll, property, (===))
import Wiki.App.Backend.Mem.State (MemState (..))
import Wiki.App.Demo (demoNow, demoState)
import Wiki.App.Editor.Complete
import Wiki.App.Editor.Tools (Control (..), controlName, htmlTags, notOffered, slashEntries, toolbarGroups)
import Wiki.App.Route (PageLink (..), pageLinkText, pageRoute, resolvePageLink)
import Wiki.Core.Assistant.Reference (markdownReference)
import Wiki.Core.Id (PageId (..), SpaceId)
import Wiki.Core.Markdown (Destination (..), classifyDestination)
import Wiki.Core.Markdown.Ast (Block (..), BoxHead (..), Footnote (..), Inline (..), ListItem (..), allInlines, parseMarkdown, plainText)
import Wiki.Core.Markdown.Ast qualified as Ast
import Wiki.Core.Markdown.Box (BoxColour (..), BoxIcon (..), BoxKind (..), boxChoices)
import Wiki.Core.Markdown.Complete
import Wiki.Core.Markdown.Edit (htmlTagLabel)
import Wiki.Core.Markdown.Problems qualified as Problems
import Wiki.Core.Markdown.Starts (linesFromStart)
import Wiki.Core.Math (definedMacroArities, redefinesEnvironments)
import Wiki.Core.Math.KatexCommands (builtinMacroArguments)
import Wiki.Core.Page (PageSummary (..))
import Wiki.Core.PageKind (PageKind (..))
import Wiki.Core.PageMatch (TitleHit (..))
import Wiki.Core.Slug (Slug, mkSlug, slugText, slugify)
import Wiki.Core.Space (Space (homePage, id, name, slug))
import Wiki.Core.Space qualified as S

-- | The answer, from no memo of the parser's readings.
answered :: [Source] -> Text -> Request -> Maybe Completion
answered found text request = fst (answer found noEarlier noReadings text request)

spec :: Spec
spec = describe "Wiki.App.Editor.Complete" $ do
  let text = "# Title\n\n[[!div]]\n\n[[/"
      atEnd = Request {line = 5, before = "[[/", after = "", nodes = ["Paragraph", "Document"]}
      document = sources (held Document [])
  describe "answer" $ do
    it "answers from the text the editor holds, at the request's line" $
      (map (.label) . (.candidates) <$> answered document text atEnd) `shouldBe` Just ["[[/div]]"]
    it "answers nothing when the line is no longer as CodeMirror saw it" $ do
      -- The block is open still, but the line was typed over since.
      answered document "# Title\n\n[[!div]]\n\nText" atEnd `shouldBe` Nothing
      answered document "# Title\n\n[[!div]]\n\n[[" atEnd `shouldBe` Nothing
      answered document (text <> "x") atEnd `shouldBe` Nothing
    it "answers nothing for a line the text does not have" $ do
      answered document text Request {line = 9, before = "[[/", after = "", nodes = []} `shouldBe` Nothing
      answered document text Request {line = 0, before = "[[/", after = "", nodes = []} `shouldBe` Nothing
    it "answers nothing where the syntax tree says the text is its own" $
      answered document text Request {line = 5, before = "[[/", after = "", nodes = ["CodeText", "FencedCode", "Document"]} `shouldBe` Nothing

    it "answers from the memo of the parser's readings as from none, and forgets it when the lines above change" $ do
      let (first', memo) = answer document noEarlier noReadings text atEnd
          (again, _) = answer document noEarlier memo text atEnd
          (other, _) = answer document noEarlier memo "# Title\n\n[[!notes]]\n\n[[/" atEnd
      (map (.label) . (.candidates) <$> first') `shouldBe` Just ["[[/div]]"]
      again `shouldBe` first'
      (map (.label) . (.candidates) <$> other) `shouldBe` Just ["[[/notes]]"]

  describe "sources" $ do
    it "offers a document's markers, and a presentation's" $ do
      let offered kind = map (.label) . (.candidates) <$> answered (sources (held kind [])) "[[!" (lineRequest "[[!" "")
      offered Document `shouldBe` Just ["[[!spoiler]]", "[[!div]]", "[[!toc]]"]
      length <$> offered Presentation `shouldBe` Just 7
    it "offers the kinds of box the administration defined" $ do
      let defined = [BoxKind {name = "exercise", label = "Exercise", colour = Teal, icon = Pencil}]
      (map (.label) . (.candidates) <$> answered (sources (held Document defined)) "> [!ex" (lineRequest "> [!ex" ""))
        `shouldBe` Just ["[!EXERCISE]"]

  describe "the pages offered after [[" $ do
    let pages =
          [ linkable "home" "Team" []
          , linkable "handbook" "Handbook" ["handbook"]
          , linkable "editing" "Editing" ["handbook", "editing"]
          , linkable "guide" "Guide" ["handbook", "guide"]
          , linkable "old" "Old notes" ["handbook", "guide-old"]
          , linkable "deep" "Guide to deploys" ["handbook", "ops", "guide-to-deploys"]
          , linkable "far" "Guide" ["projects", "guide"]
          , linkable "inside" "The user guide" ["projects", "the-user-guide"]
          , linkable "other" "Projects" ["projects"]
          ]
        onHandbook = Held {kind = Document, boxes = [], editing = PageId "editing", pages = linkablesFrom handbookEditing pages, entries = [], mathMacros = [], pageRedefinesEnvironments = False}
        offered typed = map (\c -> (c.label, c.note)) . (.candidates) <$> answered (sources onHandbook) typed (lineRequest typed "")
    it "offers every page but the one edited, the nearer first: siblings, then its branch, then the others" $
      (map fst <$> offered "[[")
        `shouldBe` Just ["Guide", "Old notes", "Handbook", "Guide to deploys", "Team", "Projects", "Guide", "The user guide"]
    it "puts a title's start before a later word or a path's segment, and those before a match inside" $
      (map fst <$> offered "See [[gui")
        `shouldBe` Just ["Guide", "Guide to deploys", "Guide", "Old notes", "The user guide"]
    it "shows where each page is, so that two of one title are told apart" $
      (filter ((== "Guide") . fst) <$> offered "[[Guide")
        `shouldBe` Just [("Guide", Just "handbook/guide"), ("Guide", Just "projects/guide")]
    it "shows the home page as such" $
      (lookup "Team" <$> offered "[[Te") `shouldBe` Just (Just (Just "the home page"))
    it "opens no list when no page has the letters" $
      offered "[[zebra" `shouldBe` Nothing
    it "writes the link with its end, the cursor after it" $ do
      written onHandbook "[[Gui" "" `shouldBe` Just "[[Guide]]"
      written onHandbook "[[Old" "" `shouldBe` Just "[[guide-old|Old notes]]"
      written onHandbook "[[Projects" "" `shouldBe` Just "[[../projects|Projects]]"
    it "replaces the end typed already, and keeps a label typed already" $ do
      -- The cursor after "Gui" of "[[Gui|For me]]": the entry replaces
      -- "[[Gui" and "|For me]]", and writes the label back as it was.
      let reach typed afterwards = (\c -> (c.from, c.through)) <$> answered (sources onHandbook) (typed <> afterwards) (lineRequest typed afterwards)
      reach "[[Gui" "de]] now" `shouldBe` Just (0, 4)
      written onHandbook "[[Gui" "de]] now" `shouldBe` Just "[[Guide]]"
      reach "[[Old" "|Mine]]" `shouldBe` Just (0, 7)
      written onHandbook "[[Old" "|Mine]]" `shouldBe` Just "[[guide-old|Mine]]"
    it "escapes the bar of a label it writes in a table's cell, and reads a link inside emphasis (#294, third round)" $ do
      let inTable typed afterwards =
            answered (sources onHandbook) ("| h | i |\n| --- | --- |\n" <> typed <> afterwards) (Request {line = 3, before = typed, after = afterwards, nodes = ["Paragraph", "Document"]})
              >>= head
              . (.candidates)
      (fst . skeletonText . (.skeleton) <$> inTable "| [[Old" "]] | x |") `shouldBe` Just "[[guide-old\\|Old notes]]"
      written onHandbook "**[[Gu" "ide]]**" `shouldBe` Just "[[Guide]]"
    it "opens no list where the text holds a carriage return, which the parser reads as a line break and the editor does not (#295's first round)" $
      answered (sources onHandbook) "See [[Guide|a\rb]] now" (lineRequest "See [[Gu" "ide|a\rb]] now") `shouldBe` Nothing
    modifyMaxSuccess (* 5) . prop "leaves one wiki link to the page chosen at the cursor, with the label read there, and every other inline as before, or offers nothing" $
      checkCoverage (forAll genLinkLine (choosesPage onHandbook))
    -- WP5.124 (f): the answer reads the text above from a start the last
    -- check kept ("Wiki.Core.Markdown.Starts"' 'linesFromStart'), with
    -- the starts of a check of the same text, of the text before a change,
    -- or none; it is the answer from the whole text above.
    modifyMaxSuccess (* 3) . prop "answers as from the whole text above, with the starts of the same text, of a text one change away, or none" $
      checkCoverage $
        forAll ((,,) <$> genLinkLine <*> elements [[], definitionsAbove] <*> genChange) $ \(l, defs, change) ->
          let l' = case l of LinkLine a p w t r lb c af le fa -> LinkLine (defs <> a) p w t r lb c af le fa
              upToLine = T.concat [a <> "\n" | a <- l'.above]
              typedUpTo = l'.pre <> fst l'.wrap <> "[[" <> l'.typed
              afterCursor = l'.rest <> l'.label <> (if l'.closed then "]]" else "") <> snd l'.wrap <> l'.lineEnd <> l'.after <> l'.farther
              whole = upToLine <> typedUpTo <> afterCursor
              request = Request {line = length l'.above + 1, before = typedUpTo, after = T.takeWhile (/= '\n') afterCursor, nodes = ["Paragraph", "Document"]}
              ls = T.splitOn "\n" whole
              atWhole = At {place = InText, above = l'.above, before = typedUpTo, after = request.after, below = drop request.line ls, cutBelow = False}
              expected = complete (sources onHandbook) (readLine l'.above) atWhole
              inMode earlier' = counterexample (show earlier'.definitions) $ fst (answer (sources onHandbook) earlier' noReadings whole request) === expected
           in counterexample (toS whole)
                $ cover 25 (maybe False ((< length l'.above) . length) (linesFromStart (earlierOf whole) l'.above)) "read from a start"
                  . cover 10 (maybe False (elem "[a]: x") (linesFromStart (earlierOf whole) l'.above)) "with definitions"
                  . cover 30 (isJust expected) "a page offered"
                $ conjoin [inMode (earlierOf whole), inMode (earlierOf (changedFrom whole change)), inMode noEarlier]

  describe "the / menu, made from the toolbar" $ do
    let menuHeld = (held Document [BoxKind {name = "exercise", label = "Exercise", colour = Teal, icon = Pencil}]) {entries = slashEntries (boxChoices [BoxKind {name = "exercise", label = "Exercise", colour = Teal, icon = Pencil}])}
        menu typed = map (\c -> (c.label, c.note)) . (.candidates) <$> answered (sources menuHeld) ("Some text " <> typed) (lineRequest ("Some text " <> typed) "")
        labelsFor typed = map fst <$> menu typed
    it "lists everything the toolbar writes, in its order, before a letter is typed" $
      (take 8 <$> labelsFor "/") `shouldBe` Just ["Bold", "Italic", "Strikethrough", "Inline spoiler", "Heading", "Bulleted list", "Numbered list", "Task list"]
    it "puts a label's start first, the shorter label first, then a later word, then a keyword" $ do
      (take 2 <$> labelsFor "/ta") `shouldBe` Just ["Table", "Task list"]
      (head =<< labelsFor "/b") `shouldBe` Just "Bold"
      labelsFor "/latex" `shouldBe` Just ["Inline math", "Display math", "TikZ picture"]
      labelsFor "/kbd" `shouldBe` Just ["Keyboard key"]
      (filter (`elem` ["Note", "Exercise"]) <$> labelsFor "/box") `shouldBe` Just ["Note", "Exercise"]
      labelsFor "/list" `shouldBe` Just ["Task list", "Bulleted list", "Numbered list"]
    it "shows what each entry writes in grey" $ do
      (lookup "Table" =<< menu "/ta") `shouldBe` Just (Just "| a | b |")
      (lookup "Exercise" =<< menu "/ex") `shouldBe` Just (Just "> [!EXERCISE]")
      (lookup "Link to a page" =<< menu "/li") `shouldBe` Just (Just "[Title](page)")
    it "opens nothing for letters no entry has" $
      labelsFor "/zqx" `shouldBe` Nothing
    it "offers every control of the toolbar, or names it among those it does not offer" $ do
      let offeredNames = map (.label) (slashEntries (boxChoices []))
          namesOf = \case
            BoxControl -> map snd (boxChoices [])
            HtmlControl -> map htmlTagLabel htmlTags
            control -> [controlName control]
          -- Each name of each control offered is an entry; none of a
          -- control left out is.
          missing c
            | controlName c `elem` notOffered = filter (`elem` offeredNames) (namesOf c)
            | otherwise = filter (`notElem` offeredNames) (namesOf c)
      concatMap missing (concat toolbarGroups) `shouldBe` []
      filter (`elem` offeredNames) notOffered `shouldBe` []
    it "says in its answer what an entry writes in place of the line, and which opens a dialog" $ do
      let answerOf typed = completionJson <$> answered (sources menuHeld) typed (lineRequest typed "")
          optionOf name typed =
            answerOf typed >>= \case
              Object o -> case KeyMap.lookup "options" o of
                Just (Array options) -> find (\case Object x -> KeyMap.lookup "label" x == Just (String name); _ -> False) (toList options)
                _ -> Nothing
              _ -> Nothing
      -- A command with several places gives them all, in the order Tab
      -- goes through them, and where the writing goes on after them
      -- (#301); a command with one place, its selection alone.
      optionOf "Table" "/ta"
        `shouldBe` Just
          ( object
              [ "label" .= ("Table" :: Text)
              , "insert" .= ("| Column 1 | Column 2 | Column 3 |\n| -------- | -------- | -------- |\n| Text     | Text     | Text     |" :: Text)
              , "select" .= [2 :: Int, 10]
              , "line" .= ("/ta" :: Text)
              , "query" .= [0 :: Int, 3]
              , "detail" .= ("| a | b |" :: Text)
              , "places" .= [[2 :: Int, 10], [13, 21], [24, 32], [72, 76], [83, 87], [94, 98]]
              , "after" .= (104 :: Int)
              ]
          )
      optionOf "Link" "Go /li"
        `shouldBe` Just (object ["label" .= ("Link" :: Text), "insert" .= ("Go [link text](https://)" :: Text), "select" .= [4 :: Int, 13], "line" .= ("Go /li" :: Text), "query" .= [3 :: Int, 6], "detail" .= ("[text](https://)" :: Text), "places" .= [[4 :: Int, 13], [15, 23]], "after" .= (24 :: Int)])
      optionOf "Link to a page" "/li" `shouldBe` Just (object ["label" .= ("Link to a page" :: Text), "template" .= ("${0}" :: Text), "detail" .= ("[Title](page)" :: Text), "action" .= ("link-page" :: Text)])
      optionOf "Mermaid diagram" "/me" `shouldBe` Just (object ["label" .= ("Mermaid diagram" :: Text), "insert" .= ("```mermaid\ngraph LR\n  A --> B\n```" :: Text), "select" .= [11 :: Int, 29], "line" .= ("/me" :: Text), "query" .= [0 :: Int, 3], "detail" .= ("```mermaid" :: Text)])
      (answerOf "/ta" >>= \case Object o -> KeyMap.lookup "escape" o; _ -> Nothing) `shouldBe` Just (String "dismisses")

  -- WP5.124 (f): the first trigger on a long page reads the text above
  -- from a start the last check kept, not from the top. The check read the
  -- page before the trigger's line was typed, as in the editor, where it
  -- runs once the typing pauses.
  describe "the first trigger on a long page" $ do
    let firstAnswer page (k, typed) = do
          let ls = T.splitOn "\n" page
              now = T.intercalate "\n" (take k ls <> [typed, ""] <> drop k ls)
              request = Request {line = k + 1, before = typed, after = "", nodes = ["Paragraph", "Document"]}
          -- The check, made before, and the text, as the editor holds them.
          earlier <- evaluate (earlierOf page)
          _ <- evaluate (IntSet.size earlier.starts + IntMap.size earlier.definitions + sum (fmap T.length earlier.lines) + T.length now)
          allocated (let a = fst (answer (sources onLong) earlier noReadings now request) in a == a)
        onLong = (held Document []) {entries = slashEntries []}
    -- Measured natively, for /, [[! and [[: 1.3, 1.0 and 6.6 MB at the
    -- guide's middle, 5.8, 3.6 and 22 MB near its end, 13, 8.6 and 45 MB
    -- in the deck (the deck's 300,000 characters split into lines and
    -- compared with the check's take most of the last). Read from the
    -- top, as before, each was 140 MB to 1.6 GB.
    it "reads the text from a start in the middle and near the end of the Markdown guide, and in the fiftieth slide of a long deck" $
      for_ [("the guide's middle", markdownReference, 50, 15_000_000), ("near the guide's end", markdownReference, 90, 50_000_000), ("the fiftieth slide", longDeck, 82, 100_000_000)] $ \(name, page, percent, bound) -> do
        let k = paragraphAt page percent
        costs <- for ["/t", "[[!", "[[Gu", "$\\f", "Text \\f"] (\typed -> (typed,) <$> firstAnswer page (k, typed))
        (name :: Text, [(t, c) | (t, c) <- costs, c > bound]) `shouldBe` (name, [])
    -- #301: a line like a definition in a code listing near the top, a
    -- TypeScript index signature in a fence, took every start below it:
    -- [[! and [[ read the whole text above (140 MB to 1.6 GB), and / and
    -- a backslash opened nothing. Below it now, the text is read from the
    -- paragraph above the cursor's line, and each answer stays within the
    -- bound near the guide's end: measured natively on 2026-11-25,
    -- unoptimised, 5.7, 3.6, 21 and 13 MB. A [[ at that paragraph costs
    -- 19 MB without the fence too, more than the bound at the guide's
    -- middle above.
    it "reads the text from a start in the middle of the Markdown guide below a TypeScript fence near its top" $ do
      let ls = T.splitOn "\n" markdownReference
          page = T.intercalate "\n" (take 2 ls <> ["```ts", "interface Prices {", "  [key: string]: number", "}", "```", ""] <> drop 2 ls)
          k = paragraphAt page 50
          lines' = T.splitOn "\n" page
          opens typed =
            let now = T.intercalate "\n" (take k lines' <> [typed, ""] <> drop k lines')
             in isJust (fst (answer (sources onLong) (earlierOf page) noReadings now (Request {line = k + 1, before = typed, after = "", nodes = ["Paragraph", "Document"]})))
      costs <- for ["/t", "[[!", "[[Gu", "$\\f"] $ \typed -> do
        cost <- firstAnswer page (k, typed)
        pure (typed, maybe False ((< 20) . length) (linesFromStart (earlierOf page) (take k lines')), cost < 50_000_000)
      costs `shouldBe` [(t, True, True) | t <- ["/t", "[[!", "[[Gu", "$\\f"]]
      -- The / menu and the commands of math open there.
      map opens ["/t", "$\\f"] `shouldBe` [True, True]
    it "where no start serves, reads the whole text above for a marker or a link, and opens nothing for a / or a backslash without reading it" $ do
      -- Before the first check (none kept) and more than the limit above:
      -- a marker and a link to a page are answered as from the whole
      -- text; a / is not answered, and costs no parse (0.4 MB natively,
      -- the text split into lines; a parse of the guide above is 150 MB).
      let k = paragraphAt markdownReference 50
          ls = T.splitOn "\n" markdownReference
          asked typed = Request {line = k + 1, before = typed, after = "", nodes = ["Paragraph", "Document"]}
          now typed = T.intercalate "\n" (take k ls <> [typed, ""] <> drop k ls)
          whole' typed = complete (sources onLong) (readLine (take k ls)) At {place = InText, above = take k ls, before = typed, after = "", below = "" : drop k ls, cutBelow = False}
          unchecked typed = fst (answer (sources onLong) noEarlier noReadings (now typed) (asked typed))
      map unchecked ["[[!", "[[Gu"] `shouldBe` map whole' ["[[!", "[[Gu"]
      isJust (whole' "/t") `shouldBe` True
      text' <- evaluate (now "/t")
      cost <- allocated (isNothing (fst (answer (sources onLong) noEarlier noReadings text' (asked "/t"))))
      (unchecked "/t", cost < 2_000_000) `shouldBe` (Nothing, True)
      -- A backslash and a letter, in prose and in a formula, the same.
      for_ ["Text \\f", "$\\f"] $ \typed -> do
        typed' <- evaluate (now typed)
        cost' <- allocated (isNothing (fst (answer (sources onLong) noEarlier noReadings typed' (asked typed))))
        (typed, unchecked typed, cost' < 2_000_000) `shouldBe` (typed, Nothing, True)

  describe "commands in math" $ do
    let macros = [MathCommand {name = "\\R", origin = "this page", arguments = 0}, MathCommand {name = "\\vect", origin = "Team", arguments = 1}, MathCommand {name = "\\vect", origin = "the wiki", arguments = 0}]
        mathHeld = (held Document []) {mathMacros = macros}
        mathAnswer typed rest = answered (sources mathHeld) (typed <> rest) (lineRequest typed rest)
    it "offers the macros of the page, its spaces and the wiki with where each comes from, the nearer shadowing the farther" $ do
      (map (\c -> (c.label, c.note)) . (.candidates) <$> mathAnswer "$\\ve" "$")
        `shouldBe` Just [("\\vec", Just "KaTeX"), ("\\vee", Just "KaTeX"), ("\\vect", Just "Team"), ("\\verb", Just "KaTeX"), ("\\vert", Just "KaTeX"), ("\\veebar", Just "KaTeX")]
    it "writes a command that takes no argument by its name as text, the cursor after it, and one that takes some with its braces in place of the cursor's line, each pair a place Tab goes to (#301)" $ do
      let option typed label' = do
            Object o <- completionJson <$> mathAnswer typed "$"
            Array options <- KeyMap.lookup "options" o
            find (\case Object p -> KeyMap.lookup "label" p == Just (String label'); _ -> False) (toList options)
      option "$\\alp" "\\alpha"
        `shouldBe` Just (object ["label" .= ("\\alpha" :: Text), "insert" .= ("\\alpha" :: Text), "detail" .= ("KaTeX" :: Text)])
      option "$\\fr" "\\frac"
        `shouldBe` Just (object ["label" .= ("\\frac" :: Text), "insert" .= ("$\\frac{}{}$" :: Text), "select" .= [7 :: Int, 7], "line" .= ("$\\fr$" :: Text), "query" .= [1 :: Int, 4], "places" .= [[7 :: Int, 7], [9, 9]], "after" .= (10 :: Int), "detail" .= ("KaTeX" :: Text)])
      -- One pair is a walk too: Tab goes on to after it.
      option "$\\sq" "\\sqrt"
        `shouldBe` Just (object ["label" .= ("\\sqrt" :: Text), "insert" .= ("$\\sqrt{}$" :: Text), "select" .= [7 :: Int, 7], "line" .= ("$\\sq$" :: Text), "query" .= [1 :: Int, 4], "places" .= [[7 :: Int, 7]], "after" .= (8 :: Int), "detail" .= ("KaTeX" :: Text)])
      -- An environment: its begin and its end, the cursor between them, and
      -- Tab to after the end.
      option "$\\be" "\\begin{pmatrix}"
        `shouldBe` Just (object ["label" .= ("\\begin{pmatrix}" :: Text), "insert" .= ("$\\begin{pmatrix}\\end{pmatrix}$" :: Text), "select" .= [16 :: Int, 16], "line" .= ("$\\be$" :: Text), "query" .= [1 :: Int, 4], "places" .= [[16 :: Int, 16]], "after" .= (29 :: Int), "detail" .= ("KaTeX" :: Text)])
      -- The nearer macro's arguments: Team's \vect takes one.
      option "$\\ve" "\\vect"
        `shouldBe` Just (object ["label" .= ("\\vect" :: Text), "insert" .= ("$\\vect{}$" :: Text), "select" .= [7 :: Int, 7], "line" .= ("$\\ve$" :: Text), "query" .= [1 :: Int, 4], "places" .= [[7 :: Int, 7]], "after" .= (8 :: Int), "detail" .= ("Team" :: Text)])
    it "offers the macros of the page's block of macros, and not a definition in a listing or a later formula" $ do
      let page = T.intercalate "\n" ["# Title", "", "```math", "\\newcommand{\\R}{\\mathbb{R}}", "\\DeclareMathOperator{\\Tr}{Tr}", "```", "", "Text $\\def\\later{x}$.", "", "```tex", "\\newcommand{\\listed}{x}", "```"]
      map (.name) (pageMacroCommands page) `shouldBe` ["\\R", "\\Tr"]
      map (\c -> (c.name, c.origin, c.arguments)) (macroCommands "Team" "\\newcommand{\\vect}[1]{\\vec{#1}}\n\\let\\eps=\\varepsilon") `shouldBe` [("\\vect", "Team", 1), ("\\eps", "Team", 0)]
    it "offers no environment where the definitions in force define \\begin or \\end: a formula earlier on the page, a space's macro of no argument, a space named like KaTeX (#392's review)" $ do
      let environments h = filter ("\\begin{" `T.isPrefixOf`) . map (.label) . (.candidates) <$> answered (sources h) "$\\be$" (lineRequest "$\\be" "$")
          page = T.intercalate "\n" ["# Title", "", "Text $\\def\\end{q}$ here.", "", "$\\be$"]
      length <$> environments mathHeld `shouldBe` Just 19
      environments mathHeld {pageRedefinesEnvironments = redefinesEnvironments page} `shouldBe` Just []
      environments mathHeld {mathMacros = macroCommands "Team" "\\newcommand{\\end}{q}"} `shouldBe` Just []
      environments mathHeld {mathMacros = macroCommands "KaTeX" "\\def\\end{q}"} `shouldBe` Just []
      environments mathHeld {mathMacros = macroCommands "the wiki" "\\renewcommand{\\begin}[1]{\\mathrm{#1}}"} `shouldBe` Just []
    it "reads the wiki's built-in macros with the arguments KaTeX gives them, as KaTeX read web/src/katex-macros.tex when Wiki.Core.Math.KatexCommands was generated" $ do
      tex <- decodeUtf8 <$> BS.readFile "../web/src/katex-macros.tex"
      definedMacroArities tex `shouldBe` [(name, Just n) | (name, n) <- builtinMacroArguments]
    it "reads the macros kept with the editor, not the page, at each answer" $ do
      -- The editor keeps the macros it offers, worked out when their
      -- sources change. An answer on the long deck with them allocates what
      -- one without them does, give or take the commands matched; working
      -- them out from the page at each answer, as #299's first version did,
      -- added 28 MB to every answer on a large page, after / and [[ too.
      let page = longDeck <> "\n\n```tex\n\\newcommand{\\listed}{x}\n```"
          k = paragraphAt page 82
          ls = T.splitOn "\n" page
          now typed = T.intercalate "\n" (take k ls <> [typed, ""] <> drop k ls)
          asked typed = Request {line = k + 1, before = typed, after = "", nodes = ["Paragraph", "Document"]}
      earlier <- evaluate (earlierOf page)
      _ <- evaluate (IntSet.size earlier.starts + IntMap.size earlier.definitions + sum (fmap T.length earlier.lines))
      kept <- evaluate (pageMacroCommands page <> macroCommands "the wiki" "\\newcommand{\\R}{\\mathbb{R}}")
      _ <- evaluate (T.length (show kept :: Text))
      _ <- evaluate (length katexOrdered)
      for_ ["/t", "$\\f", "[[Gu"] $ \typed -> do
        text' <- evaluate (now typed)
        let costOf h = allocated (let a = fst (answer (sources h) earlier noReadings text' (asked typed)) in a == a)
        with' <- costOf mathHeld {mathMacros = kept}
        without <- costOf mathHeld {mathMacros = []}
        (typed, with' < without + 2_000_000) `shouldBe` (typed, True)
    it "answers a backslash's first letter within a bound of allocation: KaTeX's thousand commands are ordered once, not at each answer" $ do
      _ <- evaluate (length katexOrdered)
      -- KaTeX's environments are ordered once too, by the first answer
      -- that lists one (#301).
      _ <- evaluate (let a = mathAnswer "$\\be" "$" in a == a)
      for_ ["$\\f", "$\\fr", "$\\x", "$\\al", "$\\b", "$\\be", "$\\begi"] $ \typed -> do
        typed' <- evaluate (T.copy typed)
        cost <- allocated (let a = mathAnswer typed' "$" in a == a)
        (typed, cost < mathBound) `shouldBe` (typed, True)

  describe "the pages offered in a space of 10,000 pages" $
    it "are matched and ranked within a bound of allocation, their paths and links worked out before, and again no more" $ do
      -- The work is counted, not timed ('allocated'). Each answer only
      -- matches and ranks the pages kept, and allocates under 1 MB
      -- natively when the letters match pages, 18 MB when they match none
      -- and every page is looked at. An answer that worked out each
      -- page's path (each page above it searched for in the list) and its
      -- link, as the editor did before #294's review, allocated 11 GB.
      let kept = linkables space largeSpace (slugs ["section-0", "page-77"])
          h = Held {kind = Document, boxes = [], editing = PageId "p77", pages = kept, entries = [], mathMacros = [], pageRedefinesEnvironments = False}
      _ <- evaluate (kept == kept)
      let asked typed = answered (sources h) typed (lineRequest typed "")
      for_ ["[[", "[[pag", "[[zz"] $ \typed -> do
        first' <- evaluate typed >>= \t -> allocated (let a = asked t in a == a)
        again <- evaluate (T.copy typed) >>= \t -> allocated (let a = asked t in a == a)
        (typed, first' < answerBound, again <= first') `shouldBe` (typed, True, True)

  describe "a link with a long address or label after the cursor" $
    it "is answered within a bound of allocation, whatever its length, and not at all past 2,000 characters" $ do
      -- The parser is asked about at most 'afterBound' characters after
      -- the cursor, a few times: under 4 MB natively for either link. A
      -- link that ends further away is offered nothing. Asking the parser
      -- again for each character of what followed the cursor, as before
      -- #294's second review, allocated 1.6 GB for 1,000 characters.
      let h = Held {kind = Document, boxes = [], editing = PageId "none", pages = linkablesFrom [] [linkable "guide" "Guide" ["guide"]], entries = [], mathMacros = [], pageRedefinesEnvironments = False}
      for_ [1_000, 10_000] $ \n -> do
        let lineAfter = T.replicate n "x" <> "|label]] end"
            page = "Some text\n\nSee [[Gu" <> lineAfter
        _ <- evaluate (T.length page)
        cost <- allocated (let a = answered (sources h) page (Request {line = 3, before = "See [[Gu", after = lineAfter, nodes = ["Paragraph", "Document"]}) in a == a)
        let offered = isJust (answered (sources h) page (Request {line = 3, before = "See [[Gu", after = lineAfter, nodes = ["Paragraph", "Document"]}))
        (n, cost < linkBound, offered) `shouldBe` (n, True, n < 2_000)

  describe "the pages of other spaces offered after [[ (#301)" $ do
    let onboarding = hitIn "team" ["handbook", "onboarding"] "Onboarding"
        teamHome = hitIn "team" [] "Team handbook"
        wiki = hitIn "projects" ["the-wiki"] "The wiki"
        elsewhere = [elsewhereSource spaceNames [onboarding, teamHome, wiki]]
        offeredElsewhere typed afterwards = map (\c -> (c.label, c.note)) . (.candidates) <$> answered elsewhere (typed <> afterwards) (lineRequest typed afterwards)
        writtenElsewhere typed afterwards label = do
          found <- answered elsewhere (typed <> afterwards) (lineRequest typed afterwards)
          c <- find (\c -> c.label == label) found.candidates
          Just (T.take found.from typed <> fst (skeletonText c.skeleton) <> T.drop found.through afterwards)
    it "shows each page by its title, with its space's name and where it is in grey, in the lookup's order" $
      offeredElsewhere "See [[on" ""
        `shouldBe` Just
          [ ("Onboarding", Just "Team · team/handbook/onboarding")
          , ("Team handbook", Just "Team · team, the home page")
          , ("The wiki", Just "Projects · team/projects/the-wiki")
          ]
    it "writes the page's own address with its title, and a space's address with the space's name for its home page" $ do
      writtenElsewhere "See [[on" "" "Onboarding" `shouldBe` Just "See [[/s/team/p/handbook/onboarding/|Onboarding]]"
      writtenElsewhere "See [[te" "" "Team handbook" `shouldBe` Just "See [[/s/team/|Team]]"
      writtenElsewhere "[[wi" "" "The wiki" `shouldBe` Just "[[/s/team/projects/p/the-wiki/|The wiki]]"
    it "keeps the reader's own label, replaces the rest of a link typed already, and escapes the bar in a table's cell" $ do
      writtenElsewhere "[[on" "|Read this]] now" "Onboarding" `shouldBe` Just "[[/s/team/p/handbook/onboarding/|Read this]] now"
      writtenElsewhere "[[on" "board]] now" "Onboarding" `shouldBe` Just "[[/s/team/p/handbook/onboarding/|Onboarding]] now"
      let inCell = do
            found <- answered [elsewhereSource spaceNames [hitIn "team" ["plans"] "Plans | Q3"]] "| h | i |\n| --- | --- |\n| [[pl | x |" (Request {line = 3, before = "| [[pl", after = " | x |", nodes = ["Paragraph", "Document"]})
            fst . skeletonText . (.skeleton) <$> head found.candidates
      inCell `shouldBe` Just "[[/s/team/p/plans/\\|Plans \\| Q3]]"
    it "offers nothing but after [[, and nothing with no page found" $ do
      offeredElsewhere "[[!di" "" `shouldBe` Nothing
      answered elsewhere "`[[on`" (Request {line = 1, before = "`[[on", after = "`", nodes = ["InlineCode", "Paragraph", "Document"]}) `shouldBe` Nothing
      answered [elsewhereSource spaceNames []] "[[on" (lineRequest "[[on" "") `shouldBe` Nothing
    it "reads a link written alone on a line, and on the line after a block's start, as the link, and the block still ends at its own end" $ do
      let alone = "[[/s/team/p/handbook/onboarding/|Onboarding]]"
          linksIn t = [(destination, plainText description) | Right blocks <- [parseMarkdown t], Link _ destination _ description <- allInlines blocks]
      linksIn alone `shouldBe` [("/s/team/p/handbook/onboarding/", "Onboarding")]
      linksIn ("[[!div]]\n" <> alone <> "\n[[/div]]") `shouldBe` [("/s/team/p/handbook/onboarding/", "Onboarding")]
      [() | Right blocks <- [parseMarkdown ("[[!div]]\n" <> alone <> "\n[[/div]]\n\nAfter")], Ast.Div {} <- blocks] `shouldBe` [()]
    modifyMaxSuccess (* 3) . prop "writes a link the parser reads as one wiki link with the label written, which leads from a page of another space to the page found" $
      checkCoverage (forAll genElsewhere elsewhereResolves)

  describe "linkLetters" $ do
    let lettersOf t request = fst (linkLetters noEarlier noReadings t request)
    it "gives the letters typed after [[ where a list of pages opens" $ do
      lettersOf "See [[onb" (lineRequest "See [[onb" "") `shouldBe` Just "onb"
      lettersOf "[[on c]]" (lineRequest "[[on c" "]]") `shouldBe` Just "on c"
    it "gives none where no list of pages opens, or the line is not the request's" $ do
      lettersOf "[[!di" (lineRequest "[[!di" "") `shouldBe` Nothing
      lettersOf "[[onb" (Request {line = 1, before = "[[onb", after = "", nodes = ["CodeText", "FencedCode", "Document"]}) `shouldBe` Nothing
      lettersOf "[[onbo" (lineRequest "[[onb" "") `shouldBe` Nothing

  describe "pageLinkText, over generated trees of pages" $ do
    it "names a sibling by its title when the title reads back as its slug" $
      pageLinkText (slugs ["a", "b"]) (slugs ["a", "on-call"]) "On-call" `shouldBe` ("On-call", Nothing)
    it "names the parent with ., and pages above with .." $ do
      pageLinkText (slugs ["a", "b"]) (slugs ["a"]) "A" `shouldBe` (".", Just "A")
      pageLinkText (slugs ["a", "b", "c"]) (slugs ["a"]) "A" `shouldBe` ("..", Just "A")
      pageLinkText (slugs ["a", "b"]) [] "Home" `shouldBe` ("..", Just "Home")
    it "writes a bracket and an ampersand of the label as character references" $
      pageLinkText (slugs ["x"]) (slugs ["q-a"]) "Q&A [draft]" `shouldBe` ("q-a", Just "Q&amp;A [draft&#93;")
    modifyMaxSuccess (* 3) . prop "writes, for every page and every other, a link the parser and resolvePageLink read as that page and its title, the shortest of the forms considered" $
      forAll genTree linksResolve

  describe "completionJson" $ do
    it "shows a candidate's place and its origin as the detail, the wiki's own marker neither" $ do
      let made = Completion {from = 2, through = 0, candidates = [own, page, fromPlugin], enter = EnterChooses, escape = EscapeCloses}
          own = Candidate {label = "[[!div]]", skeleton = [Literal "[[!div]]", Field 0 ""], writing = AsSnippet, action = Nothing, note = Nothing, origin = Nothing}
          page = Candidate {label = "Guide", skeleton = [Literal "[[Guide]]", Field 0 ""], writing = AsSnippet, action = Nothing, note = Just "handbook/guide", origin = Nothing}
          fromPlugin = Candidate {label = "[[!day]]", skeleton = [Literal "[[!day]]{", Field 1 "date", Literal "}"], writing = AsSnippet, action = Nothing, note = Just "today", origin = Just (FromPlugin "calendar")}
      completionJson made
        `shouldBe` object
          [ "from" .= (2 :: Int)
          , "through" .= (0 :: Int)
          , "options"
              .= [ object ["label" .= ("[[!div]]" :: Text), "template" .= ("[[!div]]${0}" :: Text)]
                 , object ["label" .= ("Guide" :: Text), "template" .= ("[[Guide]]${0}" :: Text), "detail" .= ("handbook/guide" :: Text)]
                 , object ["label" .= ("[[!day]]" :: Text), "template" .= ("[[!day]]\\{${1:date}\\}" :: Text), "detail" .= ("today · from plugin calendar" :: Text)]
                 ]
          , "enter" .= ("chooses" :: Text)
          , "escape" .= ("closes" :: Text)
          ]
    it "says how many characters Enter waits for" $
      completionJson Completion {from = 0, through = 0, candidates = [], enter = EnterChoosesAfter 2, escape = EscapeCloses}
        `shouldBe` object ["from" .= (0 :: Int), "through" .= (0 :: Int), "options" .= ([] :: [Value]), "enter" .= object ["after" .= (2 :: Int)], "escape" .= ("closes" :: Text)]

-- | 'LinkLine' is a block with a wiki link being typed in it: the lines
-- above, the text before the link on its line (a container's marks, a
-- heading's, a table's cell or a footnote's label among them), what
-- wraps the link (emphasis, strong, struck, an inline spoiler), what of
-- the address is before the cursor and after it, the label written (with
-- its @|@, escaped or not; on several lines, long, or across a blank
-- line, which ends the block before the link would end), whether the
-- link is closed, what follows it on its line and makes the line what it
-- is (a table's cells, a spoiler's description closed, a box's title, a
-- link's or an image's text closed, a heading's closing marks, a
-- parenthesis or a bracket), and the text after that (lines below too).
data LinkLine = LinkLine
  { above :: [Text]
  , pre :: Text
  , wrap :: (Text, Text)
  , typed :: Text
  , rest :: Text
  , label :: Text
  , closed :: Bool
  , after :: Text
  , lineEnd :: Text
  , farther :: Text
  }
  deriving stock (Show)

genLinkLine :: Gen LinkLine
genLinkLine = do
  (above, pre, lineEnd) <-
    elements
      [ ([], "", "")
      , (["Some text"], "", "")
      , (["# Notes", ""], "See ", "")
      , ([], "# ", "")
      , (["| h | i |", "| --- | --- |"], "| ", " | x |")
      , (["| h | i | j |", "| --- | --- | --- |"], "| a | ", " | x | y |")
      , ([], "- ", "")
      , (["- an item"], "  - ", "")
      , ([], "> ", "")
      , (["Text.[^n]", ""], "[^n]: ", "")
      , ([], "[[x]] and ", "")
      , ([], "`a` ", "")
      , -- What follows the link on its line makes the line what it is.
        ([], "[[!spoiler]]{s}(See ", ")")
      , ([], "> [!NOTE] See ", " here")
      , ([], "[See ", " there](https://example.com)")
      , ([], "![See ", " there](picture.png)")
      , ([], "## See ", " ##")
      , ([], "Text (see ", ") and more")
      , ([], "Text [see ", "] and more")
      ]
  wrap <- elements [("", ""), ("", ""), ("**", "**"), ("_", "_"), ("~~", "~~"), ("||", "||")]
  typed <- elements ["", "Gu", "Ol", "Pro", "gu "]
  rest <- elements ["", "ide", "-a", " x", "a&amp;b", "a\\", "a[b", T.replicate 300 "y", "ide\nto it"]
  label <- elements ["", "", "|Mine", "|[draft&#93;", "|a [b", "\\|escaped", "|Q&amp;A", "|", "|x|y", "| spaced ", "|read\nmore", "|read\n  more", "|" <> T.replicate 300 "z", "|read\n\nmore"]
  closed <- elements [True, True, False]
  following <- elements ["", " now", " and [[b]]", " [[c|C]] end", " `x`", "\nnext line", " now\n\n[[d]]"]
  -- Longer than every bound of the completion (2,000 characters after
  -- the cursor), the cursor at the text's start, in its middle or near
  -- its end.
  -- A long list's further items, or a long quotation's paragraphs, which
  -- the link's list or quotation takes in: one top-level block longer
  -- than the bound, a block of its own after the cursor's.
  farther <- elements ["", "", "\n\n" <> longText, "\n" <> longList, "\n>\n" <> longQuote]
  longAbove <- elements [[], [], concat (replicate 30 ["A paragraph above, with some words in it.", ""])]
  pure LinkLine {above = longAbove <> above, pre, wrap, typed, rest, label, closed, after = following, lineEnd, farther}
  where
    longText = T.intercalate "\n\n" (replicate 24 (T.replicate 8 "More words. "))
    longList = T.intercalate "\n" (replicate 60 "- Another item of the same long list")
    longQuote = T.intercalate "\n>\n" (replicate 60 "> Another paragraph of the same quotation")

-- | Choosing the first page offered on a 'LinkLine', as CodeMirror writes
-- it (the snippet's further lines after the cursor line's indentation),
-- read by the wiki's parser, in two halves:
--
-- * Whatever is offered, accepted, reads so. When the entry replaces text
--   after the cursor (a link there, closed), the block is read with one
--   wiki link to the page at the cursor, its text the label read there
--   before, or the page's own when it had none (or its address, where no
--   label can be written), and every other inline where it was. When it
--   replaces nothing after the cursor, the parser read no link of the
--   cursor's own before (none, or one that took a later link's end); the
--   block is read with one wiki link more than without the @[[@, one to
--   the page, and everything after the cursor is as it was.
-- * Nothing is declined needlessly: wherever the parser reads a closed
--   link at the cursor, and reads it as a link to a word, with everything
--   else where it was, once the word is its address, a page is offered;
--   and wherever it reads no link at the cursor, and a link to a word
--   written there with its end is read as one more link, a page is
--   offered. Over texts longer than the completion's bounds too, where
--   the block that holds the cursor ends within 2,000 characters of it.
choosesPage :: Held -> LinkLine -> Property
choosesPage h l =
  cover 70 offered "a page offered" . cover 40 replaceable "a link at the cursor whose address a word may replace" $
    conjoin
      [ counterexample "a link whose address a word may replace, and no page offered" (not replaceable || offered)
      , case found of
          Nothing -> property True
          Just answer' -> case head answer'.candidates of
            Nothing -> counterexample "no page" (property False)
            Just c -> accepted answer' c
      ]
  where
    found = answered (sources h) whole (Request {line = cursorLine, before = typedUpTo, after = restOfLine, nodes = ["Paragraph", "Document"]})
    offered = maybe False (not . null . (.candidates)) found
    accepted answer' c =
      let (text, _) = skeletonText c.skeleton
          indentation = T.takeWhile (\ch -> ch == ' ' || ch == '\t') typedUpTo
          snippet' = if c.writing == AsText then text else T.intercalate ("\n" <> indentation) (T.splitOn "\n" text)
          result = upToLine <> T.take answer'.from typedUpTo <> snippet' <> T.drop answer'.through afterCursor
          page = fromMaybe (panic "no page") (find (\p -> p.title == c.label) h.pages)
          ownTitles = [page.title, page.address]
       in counterexample (toS (whole <> "\n  =>\n" <> result)) $
            if answer'.through > 0
              then
                let title = maybe "" snd (find ((sentinel `T.isPrefixOf`) . fst) (linksOf marked))
                    titleNow = map snd (filter ((== page.address) . fst) (linksOf result))
                 in conjoin
                      [ outside (== page.address) (blocksOf result) === outside (sentinel `T.isPrefixOf`) (blocksOf marked)
                      , counterexample (show titleNow) $ case titleNow of
                          [t] -> if sentinel `T.isInfixOf` title then t `elem` ownTitles else t == title
                          _ -> False
                      ]
              else
                conjoin
                  [ -- Before, the parser read no link at the cursor, or one
                    -- that took a later link's end, not one of its own.
                    counterexample "a link of its own was there" $
                      not (any ((sentinel `T.isPrefixOf`) . fst) (linksOf marked)) || length (linksOf marked) <= length (linksOf unopened)
                  , T.takeEnd (T.length afterCursor) result === afterCursor
                  , length (linksOf result) === length (linksOf unopened) + 1
                  , counterexample "no link to the page" $ case filter ((== page.address) . fst) (linksOf result) of
                      [(_, t)] -> t `elem` ownTitles
                      _ -> False
                  ]
    upToLine = T.concat [a <> "\n" | a <- l.above]
    cursorLine = length l.above + 1
    typedUpTo = l.pre <> fst l.wrap <> "[[" <> l.typed
    afterCursor = l.rest <> l.label <> (if l.closed then "]]" else "") <> snd l.wrap <> l.lineEnd <> l.after <> l.farther
    restOfLine = T.takeWhile (/= '\n') afterCursor
    whole = upToLine <> typedUpTo <> afterCursor
    -- The block with a word in place of the letters typed, which names the
    -- link at the cursor; and the same without the @[[@.
    sentinel = "Zqxjvtest"
    marked = upToLine <> l.pre <> fst l.wrap <> "[[" <> sentinel <> afterCursor
    unopened = upToLine <> l.pre <> fst l.wrap <> sentinel <> afterCursor
    -- The block with a word as the address of a closed link at the
    -- cursor, its label kept.
    worded = upToLine <> l.pre <> fst l.wrap <> "[[Zqxjvword" <> l.label <> "]]" <> snd l.wrap <> l.lineEnd <> l.after <> l.farther
    -- The block with a link to a word written at the @[[@ of a link not
    -- closed yet, as an entry writes its own end.
    wordedOpen = upToLine <> l.pre <> fst l.wrap <> "[[Zqxjvword]]" <> afterCursor
    -- The completion reads at most 2,000 characters after the cursor: a
    -- block still open there is not read whole, and may be declined. The
    -- block that holds the cursor ends within them where the text cut
    -- there is read with a block after it, at any depth, in the order of
    -- the text (footnotes aside: the parser puts them last).
    endsNear =
      let cursorOffset = T.length upToLine + T.length l.pre + T.length (fst l.wrap) + 2 + T.length sentinel
          inOrder b =
            b : case b of
              BlockQuote bs -> concatMap inOrder bs
              List _ items -> concat [concatMap inOrder bs | ListItem _ bs <- items]
              Alert _ _ bs -> concatMap inOrder bs
              Div _ bs -> concatMap inOrder bs
              Spoiler _ bs -> concatMap inOrder bs
              _ -> []
          holdsOurs b = case b of
            Footnotes _ -> False
            Alert _ boxHead _ -> any isOurs (allInlines [Plain (fromMaybe [] boxHead.title)])
            Spoiler spoilerHead _ -> any isOurs (allInlines [Plain spoilerHead.description])
            BlockQuote _ -> False
            List _ _ -> False
            Div _ _ -> False
            _ -> any isOurs (allInlines [b])
          isOurs = \case
            Link attributes address _ _ -> ("class", "wikilink") `elem` attributes && sentinel `T.isPrefixOf` address
            _ -> False
          notFootnotes = \case
            Footnotes _ -> False
            _ -> True
       in T.length marked - cursorOffset <= 2000 || case break holdsOurs (concatMap inOrder (filter notFootnotes (blocksOf (T.take (cursorOffset + 2000) marked)))) of
            (_, _ : _ : _) -> True
            _ -> False
    replaceable = endsNear && (closedReplaceable || openReplaceable)
    openReplaceable =
      not l.closed
        && not (any ((sentinel `T.isPrefixOf`) . fst) (linksOf marked))
        && any ((== "Zqxjvword") . fst) (linksOf wordedOpen)
        && length (linksOf wordedOpen) == length (linksOf unopened) + 1
    closedReplaceable =
      l.closed
        && any ((sentinel `T.isPrefixOf`) . fst) (linksOf marked)
        && length (linksOf marked) > length (linksOf unopened)
        && any ((== "Zqxjvword") . fst) (linksOf worded)
        && outside (== "Zqxjvword") (blocksOf worded) == outside (sentinel `T.isPrefixOf`) (blocksOf marked)
    blocksOf t = either (const []) identity (parseMarkdown t)
    -- The blocks with the link at the cursor a mark where it stands
    -- ('besidesLink'), and each spoiler's start marker as written, which
    -- holds the link's text, left out.
    outside isOurs = map unmarked . besidesLink isOurs
    unmarked = \case
      Spoiler spoilerHead bs -> Spoiler spoilerHead {Ast.marker = ""} (map unmarked bs)
      BlockQuote bs -> BlockQuote (map unmarked bs)
      List kind items -> List kind [ListItem task (map unmarked bs) | ListItem task bs <- items]
      Div attributes bs -> Div attributes (map unmarked bs)
      Alert kind boxHead bs -> Alert kind boxHead (map unmarked bs)
      Footnotes notes -> Footnotes [Footnote n label (map unmarked bs) | Footnote n label bs <- notes]
      other -> other
    linksOf t = [(address, plainText text) | Link attributes address _ text <- allInlines (blocksOf t), ("class", "wikilink") `elem` attributes]

-- | The bytes allocated in forcing a value: what its work costs, counted
-- rather than timed, so that a slow or a busy machine changes nothing.
allocated :: Bool -> IO Int64
allocated value = do
  setAllocationCounter 0
  _ <- evaluate value
  negate <$> getAllocationCounter

-- | The allocation an answer about a link with a long text after the
-- cursor stays within.
linkBound :: Int64
linkBound = 20_000_000

-- | The allocation an answer after a backslash in math stays within,
-- measured natively on 2026-11-26 (#301):
--
-- * 0.83 MB for letters whose commands take no argument (@\\al@);
-- * 1.08 to 1.10 MB where some listed take arguments (@\\f@, @\\fr@,
--   @\\x@): the braces of each are written, and the formula with them is
--   parsed again once for each number of arguments;
-- * 1.12 to 1.16 MB where environments are listed (@\\be@, @\\begi@):
--   about 0.15 MB for the entries, and 0.19 MB for the one check, per
--   answer, that the math reads the same with them written (0.78 to 0.83
--   MB before the environments; whether a macro defines @\\begin@ or
--   @\\end@ is the editor's to know from the definitions, not looked up
--   in the commands at each answer);
-- * 1.58 MB after @\\b@ alone, the most: 68 commands, some with braces,
--   and 19 environments (1.24 MB before the environments).
--
-- The regression the bound guards, KaTeX's commands built and sorted again
-- at each answer, allocates 4.2 to 4.9 MB for the same letters, well over
-- it.
mathBound :: Int64
mathBound = 2_000_000

-- | KaTeX's commands in the list's order, forced before an answer is
-- counted: the program orders them once.
katexOrdered :: [MathCommand]
katexOrdered = mathCommands []

-- | The allocation an answer over 'largeSpace' stays within.
answerBound :: Int64
answerBound = 40_000_000

-- | A space of 10,000 pages under its home page: a hundred sections of a
-- hundred pages each, titled "Page n" (the sections "Section n").
largeSpace :: [PageSummary]
largeSpace =
  summary "p0" Nothing "Home" (slugOf "home") 0
    : concat
      [ summary ("s" <> show s) (Just (PageId "p0")) ("Section " <> show s) (slugOf ("section-" <> show s)) s
          : [summary ("p" <> show n) (Just (PageId ("s" <> show s))) ("Page " <> show n) (slugOf ("page-" <> show n)) n | k <- [1 .. 99 :: Int], let n = s * 100 + k]
      | s <- [0 .. 99 :: Int]
      ]
  where
    slugOf = either (panic . show) identity . mkSlug

-- | What the editor holds of a page of a kind, with no page to link to.
held :: PageKind -> [BoxKind] -> Held
held kind boxes = Held {kind, boxes, editing = PageId "none", pages = [], entries = [], mathMacros = [], pageRedefinesEnvironments = False}

-- | The path of the page edited in the tests of the pages offered.
handbookEditing :: [Slug]
handbookEditing = slugs ["handbook", "editing"]

-- | A question about the first line of a text: before the cursor and after.
lineRequest :: Text -> Text -> Request
lineRequest typed rest = Request {line = 1, before = typed, after = rest, nodes = ["Paragraph", "Document"]}

-- | What the first entry writes, as text, for a line typed so far.
written :: Held -> Text -> Text -> Maybe Text
written h typed afterwards = do
  found <- answered (sources h) (typed <> afterwards) (lineRequest typed afterwards)
  first' <- head found.candidates
  guard (first'.writing == AsText)
  Just (fst (skeletonText first'.skeleton))

linkable :: Text -> Text -> [Text] -> (PageId, Text, [Slug])
linkable i title path = (PageId i, title, slugs path)

slugs :: [Text] -> [Slug]
slugs = map (either (panic . show) identity . mkSlug)

----------------------------------------------------------------------
-- Generated trees of pages

-- | Titles that slugify alike, that a link's address cannot hold, or its
-- label, and plain ones.
titles :: [Text]
titles = ["On-call", "On call", "on-call", "Guide", "guide", "Plans | Q3", "A]b", "Ops/Dev", "Q&A", "Who is on call?", "Café", "Notes: 2026", "x", "Deep", "Ops", "Release 2.0", "[draft]"]

-- | A tree of pages under a home page, each title from 'titles', each slug
-- its title's, made unique among its siblings as the wiki does (@-2@).
genTree :: Gen [PageSummary]
genTree = do
  n <- choose (1, 12 :: Int)
  foldM add [summary "p0" Nothing "Home" (slug' "home") 0] [1 .. n]
  where
    add pages i = do
      parent <- elements (map (\p -> p.id) pages)
      title <- elements titles
      let base = fromMaybe "page" (slugText <$> slugify title)
          taken = [slugText p.slug | p <- pages, p.parent == Just parent]
          free = fromMaybe base (head [s | s <- base : [base <> "-" <> show k | k <- [2 :: Int ..]], s `notElem` taken])
      pure (pages <> [summary ("p" <> show i) (Just parent) title (slug' free) i])
    slug' = either (panic . show) identity . mkSlug

summary :: Text -> Maybe PageId -> Text -> Slug -> Int -> PageSummary
summary i parent title slug position =
  PageSummary {id = PageId i, space = space.id, parent, title, slug, createdAt = time, updatedAt = time, position, template = False, kind = Document}
  where
    time = UTCTime (fromGregorian 2026 11 16) 0

-- | The space the trees are in: the demo's Docs, its home page the tree's.
space :: Space
space = (fromMaybe (panic "no docs") (find (\s -> slugText s.slug == "docs") (Map.elems (demoState demoNow).spaces))) {homePage = Just (PageId "p0")}

-- | For every page edited and every other page, the link written is read
-- by the wiki's parser as a wiki link whose text is the page's title, its
-- address is a path ('classifyDestination'), and 'resolvePageLink' leads it
-- to that page; and no shorter one among the forms considered (any number
-- of levels up to a page above both, then slugs down, the last one the
-- title where 'pageLinkText' allows a title) is also read so.
linksResolve :: [PageSummary] -> Property
linksResolve pages =
  conjoin
    [ counterexample (toS (show (pathOf e) <> " -> " <> show (pathOf p) <> ": " <> writtenLink)) $
        (readsAs (pathOf e) writtenLink, [f | f <- forms (pathOf e) p, T.length f < T.length writtenLink, readsAs (pathOf e) f == Just (target p)])
          === (Just (target p), [])
    | e <- pages
    , p <- pages
    , p.id /= e.id
    , let (address, label) = pageLinkText (pathOf e) (pathOf p) p.title
          writtenLink = "[[" <> address <> maybe "" ("|" <>) label <> "]]"
    ]
  where
    spaces = space : filter (\s -> s.id /= space.id) (Map.elems (demoState demoNow).spaces)
    spacePath = [space.slug]
    pathOf p = fromMaybe (panic "no path") (S.pagePath space (\i -> find (\q -> q.id == i) pages) p.id)
    target p = (pageRoute spacePath (pathOf p), p.title)
    -- What a written link is read as: where it leads, and its text.
    readsAs linking written' = case parseMarkdown written' of
      Right blocks
        | [Link _ destination _ description] <- [i | i@Link {} <- allInlines blocks]
        , ToPath _ <- classifyDestination destination
        , Just (PageLink route "") <- resolvePageLink spaces spacePath (Just (PageId "p0")) pages linking destination ->
            Just (route, plainText description)
      _ -> Nothing
    -- The forms considered: any number of levels up to a page above both,
    -- then the slugs down, the last one the title where 'pageLinkText'
    -- allows a title; each written with the title as its label and
    -- without one.
    forms linking p =
      nub
        [ "[[" <> address <> labelPart <> "]]"
        | let parent = take (length linking - 1) linking
        , level <- [0 .. length parent]
        , take level parent `isPrefixOf` pathOf p
        , let ups = replicate (length parent - level) ".."
              down = map slugText (drop level (pathOf p))
        , address <- joined (ups <> down) : [joined (ups <> initDef [] down <> [p.title]) | not (null down), titleAllowed p.title]
        , labelPart <- ["", "|" <> labelOf p.title]
        ]
    joined = \case
      [] -> "."
      segments -> T.intercalate "/" segments
    labelOf = T.replace "]" "&#93;" . T.replace "&" "&amp;"
    -- 'pageLinkText''s rule for a title as the last segment.
    titleAllowed t = T.all (\c -> isAlphaNum c || c == ' ' || c == '-') t && T.strip t == t && not (T.null t)

-- | Definitions of links' references and of a footnote, and a paragraph
-- that uses them, above a place: the completion, reading from a start
-- below them, is handed them.
definitionsAbove :: [Text]
definitionsAbove = ["[a]: /url", "", "[^n]: A note.", "    More of it.", "", "See [a] and [^n].", ""]

-- | What the editor keeps of a check of a text ("Wiki.Core.Markdown.Problems").
earlierOf :: Text -> Earlier
earlierOf old = let found = Problems.checkText Document old in earlierFrom old found.starts found.definitions

-- | A change since the check, as (a thousandth of the text, to its line's
-- start or not, characters taken away, text put in their place).
genChange :: Gen (Int, Bool, Int, Text)
genChange = (,,,) <$> choose (0, 1000) <*> elements [False, True] <*> elements [0, 1, 3, 10, 40] <*> elements ["", "x", "\n", "\n\n", "- ", "> ", "```\n", "[[!notes]]\n", "[^9]: x\n", "[a]: /z\n"]

-- | The text a check read, given the text now and a change since.
changedFrom :: Text -> (Int, Bool, Int, Text) -> Text
changedFrom now (per, toLine, removed, was) = T.take at now <> was <> T.drop (at + min removed (T.length now - at)) now
  where
    near = T.length now * per `div` 1000
    at = if toLine then T.length (T.dropWhileEnd (/= '\n') (T.take near now)) else near

-- | The first line of the first paragraph at or after a share of a text,
-- in percent, from 0: where a trigger is typed on a line of its own.
paragraphAt :: Text -> Int -> Int
paragraphAt text percent = fromMaybe (length ls) (find starts [length ls * percent `div` 100 ..])
  where
    ls = T.splitOn "\n" text
    starts i = maybe False T.null (atMay ls (i - 1)) && maybe False (not . T.null) (atMay ls i)

-- | A presentation of sixty slides of a hundred lines each, as the
-- browser suite's long deck: paragraphs, a list of ten items, a fence, a
-- formula.
longDeck :: Text
longDeck = T.intercalate "\n" (["# Long", ""] <> concatMap slide [1 .. 60 :: Int])
  where
    slide n =
      (<> [""]) . take 99 $
        ["## Slide " <> show n, ""]
          <> para n 0 12
          <> para n 12 12
          <> map ("- " <>) (sentences n 24 10)
          <> ["", "```haskell"]
          <> ["step" <> show i <> " x = x * " <> show n <> " + " <> show i | i <- [0 .. 9 :: Int]]
          <> ["```", ""]
          <> ["$$\\int_0^{" <> show n <> "} x^2 \\, dx = \\frac{" <> show n <> "^3}{3}$$", ""]
          <> concat [para n k 12 | k <- [34, 46 .. 94]]
    para n start count = sentences n start count <> [""]
    sentences n start count = ["Line " <> show k <> " of slide " <> show n <> ": the quick brown fox jumps over the lazy dog." | k <- [start .. start + count - 1 :: Int]]

----------------------------------------------------------------------
-- The pages of other spaces

-- | The demo's spaces, but for the one the edited page is in ('space'),
-- each with its path from the top.
otherSpaces :: [(Space, [Text])]
otherSpaces =
  [ (s, path)
  | (slugName, path) <- [("team", ["team"]), ("projects", ["team", "projects"]), ("blog", ["blog"])]
  , s <- Map.elems (demoState demoNow).spaces
  , slugText s.slug == slugName
  ]

-- | The demo's spaces' names, by their ids, as the editor gives them to
-- 'elsewhereSource'.
spaceNames :: Map.Map SpaceId Text
spaceNames = Map.fromList [(s.id, s.name) | s <- Map.elems (demoState demoNow).spaces]

-- | A page the lookup found in the demo's space of this slug, at its path
-- there, with its title.
hitIn :: Text -> [Text] -> Text -> TitleHit
hitIn slugName path title = case find (\(s, _) -> slugText s.slug == slugName) otherSpaces of
  Just (s, spacePath) -> TitleHit {page = PageId ("p-" <> slugName), space = s.id, spacePath = slugs spacePath, pagePath = slugs path, title}
  Nothing -> panic ("no space " <> slugName)

-- | 'ElsewhereCase' is a link to a page of another space being typed: the
-- page found; the lines above, the text before the link on its line, the
-- letters typed after the @[[@, what of the link the reader typed already
-- after the cursor, what makes the line what it is after that, and the
-- lines below.
data ElsewhereCase = ElsewhereCase
  { hit :: TitleHit
  , above :: [Text]
  , pre :: Text
  , typed :: Text
  , rest :: Text
  , lineEnd :: Text
  , below :: [Text]
  }
  deriving stock (Show)

genElsewhere :: Gen ElsewhereCase
genElsewhere = do
  (s, spacePath) <- elements otherSpaces
  -- A page's path whose segment is a word of the route's own (@p@), which
  -- its address writes with a @~@; and the space's home page.
  pagePath <- elements [[], ["onboarding"], ["handbook", "on-call"], ["p"], ["q-a", "pages"]]
  title <- elements titles
  (above, pre, lineEnd, below) <-
    elements
      [ ([], "", "", [])
      , ([], "See ", " now", [])
      , (["| h | i |", "| --- | --- |"], "| ", " | x |", [])
      , (["[[!div]]"], "", "", ["[[/div]]"])
      , ([], "- ", "", [])
      , ([], "**", "**", [])
      ]
  typed <- elements ["on", "Gu", "q&"]
  -- A label the reader typed is after a bar, escaped in a table's cell,
  -- whose bars the table takes otherwise.
  let bar = if pre == "| " then "\\|" else "|"
  rest <- elements ["", "", "board]]", bar <> "Mine]]", bar <> "Q&amp;A]]"]
  let hit = TitleHit {page = PageId "p-found", space = s.id, spacePath = slugs spacePath, pagePath = slugs pagePath, title}
  pure ElsewhereCase {hit, above, pre, typed, rest, lineEnd, below}

-- | The entry of the page found writes a link that the parser reads as the
-- line's one wiki link, with the reader's own label where one was typed
-- and otherwise the page's title (the space's name for its home page),
-- and that leads from a page of the demo's Docs to the page found: its
-- own address, the space's for its home page.
elsewhereResolves :: ElsewhereCase -> Property
elsewhereResolves c =
  cover 15 (null c.hit.pagePath) "a space's home page"
    . cover 15 (isJust keptLabel) "a label kept"
    . cover 10 (c.pre == "| ") "in a table's cell"
    $ case answered [elsewhereSource spaceNames [c.hit]] text request >>= \found -> (found,) <$> head found.candidates of
      Nothing -> counterexample "nothing offered" (property False)
      Just (found, candidate) ->
        let line' = T.take found.from upTo <> fst (skeletonText candidate.skeleton) <> T.drop found.through fromCursor
            result = T.intercalate "\n" (c.above <> [line'] <> c.below)
            written' = [(destination, plainText description) | Right blocks <- [parseMarkdown result], Link _ destination _ description <- allInlines blocks]
         in counterexample (toS result) $
              case written' of
                [(destination, label)] ->
                  conjoin
                    [ label === expectedLabel
                    , resolvePageLink spaces' docsPath (Just (PageId "p0")) docsPages (slugs ["guide"]) destination === Just (PageLink target "")
                    ]
                other -> counterexample (show other) (property False)
  where
    upTo = c.pre <> "[[" <> c.typed
    fromCursor = c.rest <> c.lineEnd
    text = T.intercalate "\n" (c.above <> [upTo <> fromCursor] <> c.below)
    request = Request {line = length c.above + 1, before = upTo, after = fromCursor, nodes = ["Paragraph", "Document"]}
    home = null c.hit.pagePath
    keptLabel = T.stripPrefix "|" c.rest <|> T.stripPrefix "\\|" c.rest
    expectedLabel
      | Just kept <- keptLabel = T.replace "&amp;" "&" (T.dropEnd 2 kept)
      | home = fromMaybe c.hit.title (Map.lookup c.hit.space spaceNames)
      | otherwise = c.hit.title
    target = pageRoute c.hit.spacePath c.hit.pagePath
    spaces' = Map.elems (demoState demoNow).spaces
    docsPath = [space.slug]
    docsPages = [summary "p0" Nothing "Home" (slugOf "home") 0, summary "p1" (Just (PageId "p0")) "Guide" (slugOf "guide") 1]
    slugOf = either (panic . show) identity . mkSlug
