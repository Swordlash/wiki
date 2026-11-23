{-# LANGUAGE TemplateHaskell #-}

-- | The completion of the wiki's own Markdown in the editor, in a browser
-- (WP5.124 (b) of docs/PLAN.md): what only CodeMirror shows of it. What
-- opens a list, what it offers and what an entry writes are tested
-- natively ("Test.Markdown.Complete" of wiki-core, and "Test.Complete");
-- here, that the list opens on typing (after @[[@, with the space's
-- pages, which the preview then leads to), that Enter writes the entry's
-- snippet with the cursor at its first place and Tab moves to the next
-- (a brace of the snippet read back exactly, the indentation of a list's
-- item added), and that the list's and the snippet's keys stop on their
-- way to the form.
module Test.Browser.Complete (spec) where

import Control.Monad.UUID (MonadUUID)
import Data.Text qualified as T
import Halogen.JSBits (JSText, Safety (..), browserJS, fromJSText)
import Halogen.VDom.DOM.Monad.Class (MonadBrowserDOM)
import Protolude hiding (find)
import Test.Browser.Mount (slugs, startChanged, startWith)
import Test.Browser.Text (completing, completionAnswered, editorSelection, editorText, markdown, proposeText, selectText, setMarkdown, untilListTakesKeys)
import Test.Hspec.Halogen
import Wiki.Api.ServerSettings (ServerSettingsOf (boxes))
import Wiki.App.Assistant (MonadAssistant)
import Wiki.App.Backend.Mem.State (MemState (..))
import Wiki.App.Islands (MonadIslands)
import Wiki.App.Route (Route (..), pageLinkText)
import Wiki.App.Shell qualified as Shell
import Wiki.Core.Markdown.Box (BoxColour (..), BoxIcon (..), BoxKind (..))
import Wiki.Core.Markdown.Edit qualified as Edit

-- The last key that reached the window: test/jsbits/test-keys.js.
$( browserJS
     ["jsbits/test-keys.js"]
     [ ("js_watch_keys", "wiki_test_watch_keys", Safe, [t|IO ()|])
     , ("js_last_key", "wiki_test_last_key", Safe, [t|IO JSText|])
     ]
 )

spec :: forall m -> (Typeable m, MonadBrowserTest m, MonadBrowserDOM m, MonadIslands m, MonadAssistant m, MonadUUID m) => Spec
-- The reads look once, with the program idle: a group that types into the
-- editor cannot read at quiet, which would sit out the editor's pause
-- before it saves a draft after each key. The list comes on CodeMirror's
-- own timer, which the program does not know of: a read of it waits for
-- CodeMirror's answer first ('completionAnswered', 'untilListTakesKeys').
-- The preview, after its counted pause, is read after a 'waitQuiet'.
spec m = readsOnce Idle $ do
  -- Each case of 'snippetCases' on the same page: its text under the
  -- title line, the cursor at its mark; its letters typed, the list
  -- awaited with the labels the case names; then each key pressed, and
  -- the text and the selection after it.
  it "writes the chosen marker's snippet, the cursor at its first place and Tab at the next, a brace and a list item's indentation kept" $ runPage $ do
    ui <- editing helloEditor
    text <- editorText ui
    for_ snippetCases $ \(name, before, typed, offered, steps) -> do
      let (body, at) = unmarked before
      setMarkdown ui (titleLines <> body)
      selectText ui (T.length titleLines + at) (T.length titleLines + at)
      typeText text typed
      _ <- completionAnswered ui
      ((name,) <$> labels ui) `shouldReturn` (name, offered)
      untilListTakesKeys ui
      for_ steps $ \(key, after) -> do
        press key
        shown <- withSelection ui
        (name, key, shown) `shouldBe` (name, key, after)

  it "lists GitHub's kinds of box and the administration's after [!, narrowed by the letters typed" $ runPage $ do
    ui <- startChanged m (\st -> st {server = st.server {boxes = [exercise]}}) (Just "user-editor") helloEditor <* waitQuiet
    text <- editorText ui
    setMarkdown ui (titleLines <> "> ")
    selectText ui (T.length titleLines + 2) (T.length titleLines + 2)
    typeText text "[!"
    _ <- completionAnswered ui
    labels ui `shouldReturn` ["[!NOTE]", "[!TIP]", "[!IMPORTANT]", "[!WARNING]", "[!CAUTION]", "[!EXERCISE]"]
    typeText text "Ex"
    _ <- completionAnswered ui
    labels ui `shouldReturn` ["[!EXERCISE]"]
    untilListTakesKeys ui
    press "Enter"
    withSelection ui `shouldReturn` "> [!EXERCISE]\n> ‸"

  it "links the page chosen after [[ as the preview leads it, writing no second ]] when one was typed already" $ runPage $ do
    ui <- editing helloEditor
    text <- editorText ui
    find ui ".editor-side-by-side" >>= click
    setMarkdown ui titleLines
    selectText ui (T.length titleLines) (T.length titleLines)
    typeText text "See [[not"
    -- The space's pages: the blog's other post, by its title, with where
    -- it is beside it.
    _ <- completionAnswered ui
    labels ui `shouldReturn` ["Notes on Haskell"]
    (findAll ui ".cm-tooltip-autocomplete .cm-completionDetail" >>= traverse textContent) `shouldReturn` ["notes-on-haskell"]
    untilListTakesKeys ui
    press "Enter"
    -- The preview leads it to the page: a link, not a link to nothing. It
    -- follows after its pause.
    waitQuiet
    length <$> findAll ui ".editor-preview .page-body a.wikilink[href$='/s/blog/p/notes-on-haskell/']" `shouldReturn` 1
    length <$> findAll ui ".editor-preview .page-body .link-unresolved" `shouldReturn` 0
    withSelection ui `shouldReturn` "See [[Notes on Haskell]]‸"
    -- With the end typed already, the link takes it.
    setMarkdown ui (titleLines <> "And [[]] too.")
    selectText ui (T.length titleLines + 6) (T.length titleLines + 6)
    typeText text "Not"
    _ <- completionAnswered ui
    labels ui `shouldReturn` ["Notes on Haskell"]
    untilListTakesKeys ui
    press "Enter"
    withSelection ui `shouldReturn` "And [[Notes on Haskell]]‸ too."

  it "links a page typed in the middle of the long Markdown guide, the rest of the guide as it was" $ runPage $ do
    ui <- editingAs "user-admin" guidePage
    text <- editorText ui
    guide <- markdown ui >>= \t -> t <$ (t `shouldSatisfy` ((> 10_000) . T.length))
    -- A line of its own between two paragraphs in the middle: thousands
    -- of characters before it and after it.
    let (upTo, rest) = T.breakOn "\n\n" (T.drop (T.length guide `div` 2) guide)
        at = T.length guide `div` 2 + T.length upTo + 1
        (address, label) = pageLinkText (slugs ["markdown-guide"]) (slugs ["markdown-guide", "diagrams"]) "Diagrams"
        -- The editor counts UTF-16 units: an emoji above is two.
        units = at + T.length (T.filter ((> 0xFFFF) . ord) (T.take at guide))
    T.null rest `shouldBe` False
    selectText ui units units
    typeText text "See [[Diag"
    _ <- completionAnswered ui
    labels ui `shouldReturn` ["Diagrams"]
    untilListTakesKeys ui
    press "Enter"
    markdown ui `shouldReturn` T.take at guide <> "See [[" <> address <> maybe "" ("|" <>) label <> "]]" <> T.drop at guide

  it "keeps the list's Escape and the snippet's Tab and Escape from the form, and lets them through once neither is there" $ runPage $ do
    ui <- editing helloEditor
    text <- editorText ui
    setMarkdown ui titleLines
    selectText ui (T.length titleLines) (T.length titleLines)
    typeText text "[[!sp"
    untilListTakesKeys ui
    -- Escape closes the list, and leaves the text.
    reached "Escape" `shouldReturn` ""
    completing ui `shouldReturn` ""
    markdown ui `shouldReturn` titleLines <> "[[!sp"
    typeText text "o"
    untilListTakesKeys ui
    press "Enter"
    withSelection ui `shouldReturn` "[[!spoiler]]{«name»}(Description)\n\n[[/spoiler]]"
    -- Tab moves to the next place, and is not the form's indentation.
    reached "Tab" `shouldReturn` ""
    withSelection ui `shouldReturn` "[[!spoiler]]{name}(«Description»)\n\n[[/spoiler]]"
    -- Escape stops filling the snippet in, and goes no further.
    reached "Escape" `shouldReturn` ""
    -- With neither a list nor a snippet, Escape is the form's, and so is
    -- Tab, which the form takes to indent.
    reached "Escape" `shouldReturn` "Escape"
    reached "Tab" `shouldReturn` "Tab"
  -- The "/" menu (WP5.124 (d)): what each entry writes, and where it
  -- opens, are tested natively ("Test.Markdown.CompleteOracle"); here,
  -- that CodeMirror writes it as one step of the undo, with Tab going to
  -- its places, that Escape keeps the "/" from opening it again, and that
  -- an entry opens the toolbar's dialog.
  it "writes the table after /ta in the middle of the long Markdown guide, its first cell selected, a word typed over it undone apart from the table" $ runPage $ do
    ui <- editingAs "user-admin" guidePage
    text <- editorText ui
    guide <- markdown ui >>= \t -> t <$ (t `shouldSatisfy` ((> 10_000) . T.length))
    -- An empty line between two paragraphs in the middle: thousands of
    -- characters before it and after it. The editor counts UTF-16 units.
    let (upTo, _) = T.breakOn "\n\n" (T.drop (T.length guide `div` 2) guide)
        at = T.length guide `div` 2 + T.length upTo + 1
        units = at + T.length (T.filter ((> 0xFFFF) . ord) (T.take at guide))
        typed = T.take at guide <> "/ta" <> T.drop at guide
        -- What the toolbar's command writes there, the / and its letters
        -- gone.
        (table, Edit.Selection _ selected) = Edit.apply Edit.Table guide (Edit.cursor at)
    selectText ui units units
    typeText text "/ta"
    _ <- completionAnswered ui
    (take 2 <$> labels ui) `shouldReturn` ["Table", "Task list"]
    untilListTakesKeys ui
    press "Enter"
    markdown ui `shouldReturn` table
    -- "Column 1" is selected, as the toolbar selects it.
    (snd <$> editorSelection ui) `shouldReturn` selected + (units - at)
    -- A word typed over "Column 1" is a step of the undo of its own; the
    -- next undo takes the table back, leaving "/ta" as typed.
    typeText text "Name"
    press "Control+z"
    markdown ui `shouldReturn` table
    press "Control+z"
    markdown ui `shouldReturn` typed

  it "takes Enter after a / alone as a new line, closes the / menu on Escape, and the same / does not open it again as letters are typed" $ runPage $ do
    ui <- editing helloEditor
    text <- editorText ui
    setMarkdown ui titleLines
    selectText ui (T.length titleLines) (T.length titleLines)
    -- A / at the end of a line of prose, the list shown: Enter is still a
    -- new line, and writes nothing of the list's.
    typeText text "Either /"
    untilListTakesKeys ui
    press "Enter"
    markdown ui `shouldReturn` titleLines <> "Either /\n"
    typeText text "Some /ta"
    untilListTakesKeys ui
    press "Escape"
    length <$> findAll ui ".cm-tooltip-autocomplete" `shouldReturn` 0
    typeText text "b"
    completionAnswered ui `shouldReturn` ""
    length <$> findAll ui ".cm-tooltip-autocomplete" `shouldReturn` 0
    -- With no list, Enter is a new line; a / typed anew opens it.
    press "Enter"
    typeText text "/ta"
    _ <- completionAnswered ui
    (take 1 <$> labels ui) `shouldReturn` ["Table"]
    markdown ui `shouldReturn` titleLines <> "Either /\nSome /tab\n/ta"
    -- An assistant's undecided change on the line stays its own once an
    -- entry is written, which changes only what differs: rejecting it puts
    -- back what it replaced.
    setMarkdown ui (titleLines <> "Some text")
    proposeText ui (T.length titleLines + 5) (T.length titleLines + 9) "words"
    selectText ui (T.length titleLines + 10) (T.length titleLines + 10)
    typeText text " /b"
    _ <- completionAnswered ui
    (take 1 <$> labels ui) `shouldReturn` ["Bold"]
    untilListTakesKeys ui
    press "Enter"
    find ui ".cm-wiki-change-reject" >>= click
    markdown ui `shouldReturn` titleLines <> "Some text **bold text**"
    -- And where the command changes the line's start too: the change
    -- between, untouched, stays.
    setMarkdown ui (titleLines <> "Some text")
    proposeText ui (T.length titleLines + 5) (T.length titleLines + 9) "words"
    selectText ui (T.length titleLines + 10) (T.length titleLines + 10)
    typeText text " /he"
    _ <- completionAnswered ui
    (take 1 <$> labels ui) `shouldReturn` ["Heading"]
    untilListTakesKeys ui
    press "Enter"
    find ui ".cm-wiki-change-reject" >>= click
    markdown ui `shouldReturn` titleLines <> "# Some text "
    -- A change below the line while the list is open: the answer was made
    -- from the text before it, and Enter writes nothing and closes the
    -- list.
    setMarkdown ui (titleLines <> "\n\nEnd.")
    selectText ui (T.length titleLines) (T.length titleLines)
    typeText text "/ta"
    untilListTakesKeys ui
    proposeText ui (T.length titleLines + 4) (T.length titleLines + 4) "New prose"
    press "Enter"
    markdown ui `shouldReturn` titleLines <> "/ta\nNew prose\nEnd."
    completing ui `shouldReturn` ""

  it "opens the dialog of a link to a page from the / menu, the / and its letters gone" $ runPage $ do
    ui <- editing helloEditor
    text <- editorText ui
    setMarkdown ui titleLines
    selectText ui (T.length titleLines) (T.length titleLines)
    typeText text "See /li"
    _ <- completionAnswered ui
    (take 2 <$> labels ui) `shouldReturn` ["Link", "Link to a page"]
    untilListTakesKeys ui
    press "ArrowDown"
    press "Enter"
    length <$> findAll ui ".link-dialog" `shouldReturn` 1
    markdown ui `shouldReturn` titleLines <> "See "
  -- Commands in math (WP5.124 (e)): where the list opens and what it
  -- offers are tested natively ("Test.Markdown.CompleteMath"); here, the
  -- list in the long guide, a space's macro in it with where it comes
  -- from, the command's name written with the cursor after it, and a line
  -- break's two backslashes left to Enter.
  it "completes \\fr to \\frac in a formula in the middle of the long guide, the cursor after it, as a step of the undo, and lists the space's macros" $ runPage $ do
    ui <- editingAs "user-admin" guidePage
    text <- editorText ui
    guide <- markdown ui >>= \t -> t <$ (t `shouldSatisfy` ((> 10_000) . T.length))
    let (upTo, _) = T.breakOn "\n\n" (T.drop (T.length guide `div` 2) guide)
        at = T.length guide `div` 2 + T.length upTo + 1
        units = at + T.length (T.filter ((> 0xFFFF) . ord) (T.take at guide))
    selectText ui units units
    typeText text "$\\fr"
    _ <- completionAnswered ui
    (take 1 <$> labels ui) `shouldReturn` ["\\frac"]
    untilListTakesKeys ui
    press "Enter"
    markdown ui `shouldReturn` T.take at guide <> "$\\frac" <> T.drop at guide
    (snd <$> editorSelection ui) `shouldReturn` units + T.length "$\\frac"
    -- In a brace typed after it, the Docs space's macro, with the space's
    -- name beside it.
    typeText text "{\\Ho"
    _ <- completionAnswered ui
    (findAll ui ".cm-tooltip-autocomplete .cm-completionDetail" >>= traverse textContent) `shouldReturn` ["Docs"]
    labels ui `shouldReturn` ["\\Hom"]
    -- The command written is a step of the undo of its own: one undo takes
    -- back "{\\Ho", the next the command, leaving "$\\fr" as typed.
    press "Escape"
    press "Control+z"
    markdown ui `shouldReturn` T.take at guide <> "$\\frac" <> T.drop at guide
    press "Control+z"
    markdown ui `shouldReturn` T.take at guide <> "$\\fr" <> T.drop at guide

  it "leaves a line break's two backslashes to Enter in display math" $ runPage $ do
    ui <- editing helloEditor
    text <- editorText ui
    setMarkdown ui (titleLines <> "$$\na")
    selectText ui (T.length titleLines + 4) (T.length titleLines + 4)
    typeText text " \\\\"
    press "Enter"
    typeText text "b"
    markdown ui `shouldReturn` titleLines <> "$$\na \\\\\nb"
  where
    helloEditor = EditPage (slugs ["blog"]) (slugs ["hello-world"])
    guidePage = EditPage (slugs ["docs"]) (slugs ["markdown-guide"])
    -- The editor of a page, as the editor or as someone else. CodeMirror's
    -- attach is waited for in a fork, which only quiet covers: nothing is
    -- typed yet, so quiet costs no pause of the draft.
    editing :: Route -> PageM s (Mounted s Shell.Query Void m)
    editing = editingAs "user-editor"
    editingAs :: Text -> Route -> PageM s (Mounted s Shell.Query Void m)
    editingAs who page = fst <$> startWith m (Just who) page <* waitQuiet
    exercise = BoxKind {name = "exercise", label = "Exercise", colour = Teal, icon = Pencil}
    -- A key pressed, and the key that reached the window after it, if
    -- any: "" for one the editor stopped on its way.
    reached key = do
      unsafeIOToPageM js_watch_keys
      press key
      unsafeIOToPageM (fromJSText <$> js_last_key)

-- | The labels of the completion's list shown, in its order.
labels :: Mounted s q o m -> PageM s [Text]
labels ui = findAll ui ".cm-tooltip-autocomplete .cm-completionLabel" >>= traverse textContent

-- | The editor's text under the title line, with its selection marked:
-- @‸@ for a cursor, @«…»@ around what is selected. The texts here are
-- ASCII, so UTF-16 units are characters.
withSelection :: Mounted s q o m -> PageM s Text
withSelection ui = do
  text <- T.drop (T.length titleLines) <$> markdown ui
  (start, end) <- bimap (subtract (T.length titleLines)) (subtract (T.length titleLines)) <$> editorSelection ui
  pure $
    if start == end
      then T.take start text <> "‸" <> T.drop start text
      else T.take start text <> "«" <> T.take (end - start) (T.drop start text) <> "»" <> T.drop end text

-- | The cases of a marker's snippet, one a line: a name, the text with
-- the cursor at @‸@, the letters typed there, the list's labels then, and
-- each key pressed with the text and the selection after it ('withSelection').
-- The page is a document, which is offered no presentation's markers.
snippetCases :: [(Text, Text, Text, [Text], [(Text, Text)])]
snippetCases =
  [ ("a block", "‸", "[[!di", ["[[!div]]"], [("Enter", "[[!div]]\n‸\n[[/div]]")])
  , ("a block in a quotation", "> ‸", "[[!di", ["[[!div]]"], [("Enter", "> [[!div]]\n> ‸\n> [[/div]]")])
  , ("a block in a list's item", "- a\n\n  ‸", "[[!di", ["[[!div]]"], [("Enter", "- a\n\n  [[!div]]\n  ‸\n  [[/div]]")])
  , ("a document's markers", "‸", "[[!", ["[[!spoiler]]", "[[!div]]", "[[!toc]]"], [("ArrowDown", "[[!‸"), ("ArrowDown", "[[!‸"), ("Enter", "[[!toc]]‸")])
  , ("a spoiler's places in turn, its braces read back", "‸", "[[!sp", ["[[!spoiler]]"], spoilerSteps)
  , ("the innermost block's end", "[[!spoiler]]{a}(A)\n\n[[!div]]\n\nText\n\n‸", "[[/", ["[[/div]]"], [("Enter", "[[!spoiler]]{a}(A)\n\n[[!div]]\n\nText\n\n[[/div]]‸")])
  , ("a box's kind", "> ‸", "[!wa", ["[!WARNING]"], [("Enter", "> [!WARNING]\n> ‸")])
  ]
  where
    spoilerSteps =
      [ ("Enter", "[[!spoiler]]{«name»}(Description)\n\n[[/spoiler]]")
      , ("Tab", "[[!spoiler]]{name}(«Description»)\n\n[[/spoiler]]")
      , ("Tab", "[[!spoiler]]{name}(Description)\n‸\n[[/spoiler]]")
      , ("Tab", "[[!spoiler]]{name}(Description)\n\n[[/spoiler]]‸")
      ]

-- | What the cases write under: the page's title line, which the editor
-- keeps, and a blank line.
titleLines :: Text
titleLines = "# Hello, world\n\n"

-- | A text with the cursor's mark, @‸@, taken out, and where the mark was.
unmarked :: Text -> (Text, Int)
unmarked t = let (before, after) = T.breakOn "‸" t in (before <> T.drop 1 after, T.length before)
