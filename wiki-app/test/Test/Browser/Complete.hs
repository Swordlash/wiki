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
import Test.Browser.Text (completing, editorSelection, editorText, listReady, markdown, selectText, setMarkdown)
import Test.Hspec.Halogen
import Wiki.Api.ServerSettings (ServerSettingsOf (boxes))
import Wiki.App.Assistant (MonadAssistant)
import Wiki.App.Backend.Mem.State (MemState (..))
import Wiki.App.Islands (MonadIslands)
import Wiki.App.Route (Route (..), pageLinkText)
import Wiki.Core.Markdown.Box (BoxColour (..), BoxIcon (..), BoxKind (..))

-- The last key that reached the window: test/jsbits/test-keys.js.
$( browserJS
     ["jsbits/test-keys.js"]
     [ ("js_watch_keys", "wiki_test_watch_keys", Safe, [t|IO ()|])
     , ("js_last_key", "wiki_test_last_key", Safe, [t|IO JSText|])
     ]
 )

spec :: forall m -> (Typeable m, MonadBrowserTest m, MonadBrowserDOM m, MonadIslands m, MonadAssistant m, MonadUUID m) => Spec
spec m = do
  -- Each case of 'snippetCases' on the same page: its text under the
  -- title line, the cursor at its mark; its letters typed, the list
  -- awaited with the labels the case names; then each key pressed, and
  -- the text and the selection after it.
  it "writes the chosen marker's snippet, the cursor at its first place and Tab at the next, a brace and a list item's indentation kept" $ runPage $ do
    (ui, _) <- startWith m (Just "user-editor") helloEditor
    text <- editorText ui
    for_ snippetCases $ \(name, before, typed, offered, steps) -> do
      let (body, at) = unmarked before
      setMarkdown ui (titleLines <> body)
      selectText ui (T.length titleLines + at) (T.length titleLines + at)
      typeText text typed
      eventually $ ((name,) <$> labels ui) `shouldReturn` (name, offered)
      listReady ui
      for_ steps $ \(key, after) -> do
        press key
        shown <- withSelection ui
        (name, key, shown) `shouldBe` (name, key, after)

  it "lists GitHub's kinds of box and the administration's after [!, narrowed by the letters typed" $ runPage $ do
    ui <- startChanged m (\st -> st {server = st.server {boxes = [exercise]}}) (Just "user-editor") helloEditor
    text <- editorText ui
    setMarkdown ui (titleLines <> "> ")
    selectText ui (T.length titleLines + 2) (T.length titleLines + 2)
    typeText text "[!"
    eventually $ labels ui `shouldReturn` ["[!NOTE]", "[!TIP]", "[!IMPORTANT]", "[!WARNING]", "[!CAUTION]", "[!EXERCISE]"]
    typeText text "Ex"
    eventually $ labels ui `shouldReturn` ["[!EXERCISE]"]
    listReady ui
    press "Enter"
    withSelection ui `shouldReturn` "> [!EXERCISE]\n> ‸"

  it "links the page chosen after [[ as the preview leads it, writing no second ]] when one was typed already" $ runPage $ do
    (ui, _) <- startWith m (Just "user-editor") helloEditor
    text <- editorText ui
    find ui ".editor-side-by-side" >>= click
    setMarkdown ui titleLines
    selectText ui (T.length titleLines) (T.length titleLines)
    typeText text "See [[not"
    -- The space's pages: the blog's other post, by its title, with where
    -- it is beside it.
    eventually $ labels ui `shouldReturn` ["Notes on Haskell"]
    (findAll ui ".cm-tooltip-autocomplete .cm-completionDetail" >>= traverse textContent) `shouldReturn` ["notes-on-haskell"]
    listReady ui
    press "Enter"
    -- The preview leads it to the page: a link, not a link to nothing.
    eventually $ length <$> findAll ui ".editor-preview .page-body a.wikilink[href$='/s/blog/p/notes-on-haskell/']" `shouldReturn` 1
    length <$> findAll ui ".editor-preview .page-body .link-unresolved" `shouldReturn` 0
    withSelection ui `shouldReturn` "See [[Notes on Haskell]]‸"
    -- With the end typed already, the link takes it.
    setMarkdown ui (titleLines <> "And [[]] too.")
    selectText ui (T.length titleLines + 6) (T.length titleLines + 6)
    typeText text "Not"
    eventually $ labels ui `shouldReturn` ["Notes on Haskell"]
    listReady ui
    press "Enter"
    withSelection ui `shouldReturn` "And [[Notes on Haskell]]‸ too."

  it "links a page typed in the middle of the long Markdown guide, the rest of the guide as it was" $ runPage $ do
    (ui, _) <- startWith m (Just "user-admin") (EditPage (slugs ["docs"]) (slugs ["markdown-guide"]))
    text <- editorText ui
    guide <- eventually $ markdown ui >>= \t -> t <$ (t `shouldSatisfy` ((> 10_000) . T.length))
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
    eventually $ labels ui `shouldReturn` ["Diagrams"]
    listReady ui
    press "Enter"
    markdown ui `shouldReturn` T.take at guide <> "See [[" <> address <> maybe "" ("|" <>) label <> "]]" <> T.drop at guide

  it "keeps the list's Escape and the snippet's Tab and Escape from the form, and lets them through once neither is there" $ runPage $ do
    (ui, _) <- startWith m (Just "user-editor") helloEditor
    text <- editorText ui
    setMarkdown ui titleLines
    selectText ui (T.length titleLines) (T.length titleLines)
    typeText text "[[!sp"
    listReady ui
    -- Escape closes the list, and leaves the text.
    reached "Escape" `shouldReturn` ""
    completing ui `shouldReturn` ""
    markdown ui `shouldReturn` titleLines <> "[[!sp"
    typeText text "o"
    listReady ui
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
  where
    helloEditor = EditPage (slugs ["blog"]) (slugs ["hello-world"])
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
