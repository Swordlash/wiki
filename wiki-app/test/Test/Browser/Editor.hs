{-# LANGUAGE TemplateHaskell #-}

-- | The editor in a browser, in the shell, over the demo's wiki: typing
-- Markdown and seeing the preview follow, saving (and staying, or closing)
-- and reading the page back from the backend, a new page made from its
-- title and given a file before anything is saved, leaving with and without
-- unsaved changes, Cancel keeping or discarding the draft, a save over
-- someone else's and the conflict view's three ways out of it (keep mine,
-- take theirs, merge by hand), files attached by a drop, a paste or a file input, text
-- pasted, the toolbar and
-- its shortcuts, and the preview shown on its own or beside the Markdown.
module Test.Browser.Editor (spec) where

import Control.Monad.UUID (MonadUUID)
import Data.IORef (IORef, modifyIORef', readIORef)
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Halogen.Activity.Control (TestClock, advance, withTestClock)
import Halogen.JSBits (Inert (..), JSText, Safety (..), browserJS, fromJSText, inBrowser, toJSText)
import Halogen.JSBits.Promise (Promise (..), wait)
import Halogen.VDom.DOM.Monad.Class (MonadBrowserDOM)
import Protolude hiding (find, wait)
import Test.Browser.Mount (clickButton, slugs, startChanged, startWith)
import Test.Browser.Text (clearBody, clickKeepingFocus, completing, completionAnswered, editorSelection, editorText, elsewhereAsked, listReady, markdown, mathIn, problemsMarked, selectText, setMarkdown, untilListTakesKeys, utf16Length, watchElsewhere)
import Test.Hspec (around)
import Test.Hspec.Halogen
import Wiki.Api.ServerSettings (ServerSettingsOf (..))
import Wiki.App.Assistant (MonadAssistant)
import Wiki.App.Backend (MonadWiki (..))
import Wiki.App.Backend.Admin (MonadAdmin (..))
import Wiki.App.Backend.Mem (runMemWiki)
import Wiki.App.Backend.Mem.State (MemState (..), insertPage, insertUser)
import Wiki.App.Demo (demoNow)
import Wiki.App.Islands (MonadIslands)
import Wiki.App.Route (Route (..))
import Wiki.App.View.Editor (draftDelay, elsewhereDelay, previewDelay, savedNoticeTime)
import Wiki.App.View.PagePicker qualified as PagePicker
import Wiki.Core.Attachment (Attachment (name), FileName (..))
import Wiki.Core.Draft (Draft (..))
import Wiki.Core.Id (UserId (..))
import Wiki.Core.Markdown.Ast (Block (..), Inline (..), parseMarkdown)
import Wiki.Core.Markdown.Box (BoxKind (..))
import Wiki.Core.Page (Page (attachments, body, id, title, updatedAt))
import Wiki.Core.Slug (slugText)
import Wiki.Core.Space (Space (id, mathMacros, slug))
import Wiki.Core.User (Login (..), User (..))

-- The page has no file dialog the harness can answer, so a test hands the
-- editor a file the way the browser would: test/jsbits/test-files.js.
-- Whether a key reached the browser is read by a listener of its own:
-- test/jsbits/test-keys.js. The colour the style sheet gives an element:
-- test/jsbits/test-style.js. Where an element is in the window:
-- test/jsbits/test-layout.js. The address the test page is at:
-- test/jsbits/test-location.js.
$( browserJS
     ["jsbits/test-files.js", "jsbits/test-keys.js", "jsbits/test-style.js", "jsbits/test-layout.js", "jsbits/test-location.js"]
     [ ("js_drop_filePromise", "wiki_test_drop_file", Safe, [t|JSText -> JSText -> JSText -> JSText -> IO (Promise ())|])
     , ("js_pastePromise", "wiki_test_paste", Safe, [t|JSText -> JSText -> JSText -> JSText -> JSText -> IO (Promise Bool)|])
     , ("js_choose_filePromise", "wiki_test_choose_file", Safe, [t|JSText -> JSText -> JSText -> JSText -> IO (Promise ())|])
     , ("js_watch_keys", "wiki_test_watch_keys", Safe, [t|IO ()|])
     , ("js_last_key_prevented", "wiki_test_last_key_prevented", Safe, [t|IO Bool|])
     , ("js_computed_colour", "wiki_test_computed_colour", Safe, [t|JSText -> IO JSText|])
     , ("js_computed_style", "wiki_test_computed_style", Safe, [t|JSText -> JSText -> IO JSText|])
     , ("js_rect", "wiki_test_rect", Safe, [t|JSText -> IO JSText|])
     , ("js_on_top", "wiki_test_on_top", Safe, [t|JSText -> IO JSText|])
     , ("js_location_hash", "wiki_test_location_hash", Safe, [t|IO JSText|])
     ]
 )

js_drop_file :: JSText -> JSText -> JSText -> JSText -> IO ()
js_drop_file a1 a2 a3 a4 = if inBrowser then wait =<< js_drop_filePromise a1 a2 a3 a4 else pure inert

js_paste :: JSText -> JSText -> JSText -> JSText -> JSText -> IO Bool
js_paste a1 a2 a3 a4 a5 = if inBrowser then wait =<< js_pastePromise a1 a2 a3 a4 a5 else pure inert

js_choose_file :: JSText -> JSText -> JSText -> JSText -> IO ()
js_choose_file a1 a2 a3 a4 = if inBrowser then wait =<< js_choose_filePromise a1 a2 a3 a4 else pure inert

spec :: forall m -> (Typeable m, MonadBrowserTest m, MonadBrowserDOM m, MonadIslands m, MonadAssistant m, MonadUUID m) => Spec
-- The reads look once, with the program idle. Every example runs on the
-- test's clock (core's 'withTestClock'): the editor's pauses (the
-- preview's, the check's, the draft's, the "Saved" notice's, the link
-- dialog's search) sleep until the example advances the clock past them,
-- and a wait for the program to be quiet does not wait for a pause asleep
-- on it. So an example sits out no pause it does not read the work of,
-- and reads the preview, the draft or a search's hits once, after the
-- pause it advances ('previewNow', 'draftNow', 'pickerNow').
spec m = readsOnce Idle $ around withTestClock $ do
  it "previews what is typed, with its formulas drawn, and saves it" $ \clock -> runPage $ do
    (ui, wiki) <- startWith m (Just "user-editor") helloEditor
    find ui ".editor-heading" >>= (`shouldHaveText` "Editing Hello, world")
    sideBySide ui
    text <- editorText ui
    clearBody ui
    typeText text "Euler: $e^{i\\pi} + 1 = 0$"
    -- The preview follows after its pause.
    previewNow clock
    find ui ".editor-preview .page-body h1" >>= (`shouldHaveText` "Hello, world")
    formula <- find ui ".editor-preview .wiki-math-inline .katex"
    findIn formula "annotation" >>= (`shouldHaveText` "e^{i\\pi} + 1 = 0")
    -- More typing redraws the preview, but not a formula it has not touched.
    island <- find ui ".editor-preview .wiki-math-inline"
    typeText text "\n\nAnd more."
    previewNow clock
    find ui ".editor-preview .page-body" >>= textContent >>= (`shouldSatisfy` T.isInfixOf "And more.")
    find ui ".editor-preview .wiki-math-inline" >>= (`shouldBeSameElement` island)
    find ui ".editor-save-close" >>= click
    find ui ".page-view .page-body h1" >>= (`shouldHaveText` "Hello, world")
    _ <- find ui ".page-view .wiki-math-inline .katex"
    saved <- readPage wiki ["blog"] ["hello-world"]
    saved.body `shouldBe` "# Hello, world\n\nEuler: $e^{i\\pi} + 1 = 0$\n\nAnd more."

  it "saves with Ctrl+S and stays, saying Saved for a while, and indents with Tab" $ \clock -> runPage $ do
    (ui, wiki) <- startWith m (Just "user-editor") helloEditor
    text <- editorText ui
    notice <- find ui ".editor-saved-notice"
    getAttribute notice "role" `shouldReturn` Just "status"
    notice `shouldHaveText` ""
    clearBody ui
    typeText text "- a list"
    -- A line break and nothing more: Enter would start the list's next
    -- item.
    press "Shift+Enter"
    press "Tab"
    typeText text "- nested"
    markdown ui `shouldReturn` "# Hello, world\n\n- a list\n  - nested"
    press "Control+s"
    notice `shouldHaveText` "Saved"
    -- The focus is still in the text after the save.
    press "x"
    markdown ui `shouldReturn` "# Hello, world\n\n- a list\n  - nestedx"
    press "Backspace"
    find ui ".editor-heading" >>= (`shouldHaveText` "Editing Hello, world")
    saved <- readPage wiki ["blog"] ["hello-world"]
    saved.body `shouldBe` "# Hello, world\n\n- a list\n  - nested"
    -- Still the same text, with the cursor where it was.
    editorText ui >>= (`shouldBeSameElement` text)
    -- Enter on the nested item starts another one, and Enter again on
    -- that empty item goes up to the outer list.
    press "Enter"
    press "Enter"
    typeText text "more"
    -- The notice goes by itself.
    perform (advance clock savedNoticeTime)
    notice `shouldHaveText` ""
    -- A second save is made against the first: no conflict.
    press "Control+s"
    notice `shouldHaveText` "Saved"
    length <$> findAll ui ".error-conflict" `shouldReturn` 0
    again <- readPage wiki ["blog"] ["hello-world"]
    again.body `shouldBe` "# Hello, world\n\n- a list\n  - nested\n- more"
    again.updatedAt `shouldSatisfy` (> saved.updatedAt)

  it "saves, saves again, and shows the page after Save and close" $ \_ -> runPage $ do
    (ui, wiki) <- startWith m (Just "user-editor") helloEditor
    text <- editorText ui
    clearBody ui
    typeText text "One."
    find ui ".editor-save" >>= click
    find ui ".editor-saved-notice" >>= (`shouldHaveText` "Saved")
    typeText text " Two."
    find ui ".editor-save" >>= click
    eventually $ (\p -> p.body) <$> readPage wiki ["blog"] ["hello-world"] >>= (`shouldBe` "# Hello, world\n\nOne. Two.")
    typeText text " Three."
    find ui ".editor-save-close" >>= click
    find ui ".page-view .page-body p" >>= (`shouldHaveText` "One. Two. Three.")
    length <$> findAll ui ".error-conflict" `shouldReturn` 0

  it "writes a new page from the space's New page, made from its title first" $ \_ -> runPage $ do
    (ui, wiki) <- startWith m (Just "user-editor") (SpacePages (slugs ["blog"]))
    find ui ".space-view .actions a[href$='#/s/blog/new-page/']" >>= click
    find ui ".new-page-view h1" >>= (`shouldHaveText` "New page in Blog")
    find ui ".new-page-title" >>= (`typeText` "Third post")
    find ui ".new-page-view button.primary" >>= click
    find ui ".editor-heading" >>= (`shouldHaveText` "Editing Third post")
    -- Made, empty, before anything is written.
    made <- readPage wiki ["blog"] ["third-post"]
    made.body `shouldBe` ""
    find ui ".editor-title-text" >>= (`shouldHaveText` "Third post")
    editorText ui >>= (`typeText` "Some **news**.")
    -- Shown on its own, the preview has the draft at once; Save is there.
    find ui ".editor-show-preview" >>= click
    -- The title line is the page's title, shown once.
    find ui ".editor-preview .page-body h1" >>= (`shouldHaveText` "Third post")
    length <$> findAll ui ".editor-preview .page-title" `shouldReturn` 0
    find ui ".editor-preview .page-body strong" >>= (`shouldHaveText` "news")
    find ui ".editor-save-close" >>= click
    find ui ".page-view .page-body h1" >>= (`shouldHaveText` "Third post")
    find ui ".breadcrumbs [aria-current=page]" >>= (`shouldHaveText` "Third post")
    created <- readPage wiki ["blog"] ["third-post"]
    created.body `shouldBe` "# Third post\n\nSome **news**."

  it "writes a new page under another from its New subpage" $ \_ -> runPage $ do
    (ui, wiki) <- startWith m (Just "user-editor") (Page (slugs ["blog"]) (slugs ["hello-world"]))
    find ui ".page-view .actions a[href$='#/s/blog/new-page/hello-world/']" >>= click
    find ui ".new-page-view h1" >>= (`shouldHaveText` "New page under Hello, world")
    find ui ".new-page-title" >>= (`typeText` "Follow-up")
    find ui ".new-page-view button.primary" >>= click
    find ui ".editor-heading" >>= (`shouldHaveText` "Editing Follow-up")
    find ui ".editor-save-close" >>= click
    find ui ".page-view .page-body h1" >>= (`shouldHaveText` "Follow-up")
    created <- readPage wiki ["blog"] ["hello-world", "follow-up"]
    created.title `shouldBe` "Follow-up"
    created.body `shouldBe` "# Follow-up\n\n"

  it "attaches a file to a new page before anything is saved" $ \_ -> runPage $ do
    (ui, wiki) <- startWith m (Just "user-editor") (NewPage (slugs ["blog"]) Nothing)
    find ui ".new-page-title" >>= (`typeText` "With a picture")
    find ui ".new-page-view button.primary" >>= click
    find ui ".editor-heading" >>= (`shouldHaveText` "Editing With a picture")
    length <$> findAll ui ".editor-attach.disabled" `shouldReturn` 0
    -- Once CodeMirror has the text, which the reference goes into.
    _ <- editorText ui
    perform (chooseFile ".editor-attach input" "dot.png" "image/png" dotPng)
    eventually $ markdown ui `shouldReturn` "# With a picture\n\n![dot.png](attachment:dot.png)"
    find ui ".editor-attachments li a" >>= (`shouldHaveText` "dot.png")
    find ui ".editor-hint" >>= textContent >>= (`shouldSatisfy` (not . T.isInfixOf "first"))
    attached <- readPage wiki ["blog"] ["with-a-picture"]
    map (\a -> a.name.text) attached.attachments `shouldBe` ["dot.png"]
    find ui ".editor-save-close" >>= click
    _ <- find ui ".page-view .page-body img[alt='dot.png']"
    pass

  it "asks on Cancel with changes whether to keep the draft, and keeps it or not as told" $ \_ -> runPage $ do
    (ui, wiki) <- startWith m (Just "user-editor") helloEditor
    text <- editorText ui
    clearBody ui
    typeText text "Kept for later."
    find ui ".editor-cancel" >>= click
    _ <- find ui ".editor-cancel-confirm"
    -- Going on editing leaves everything as it was.
    clickButton ui ".editor-cancel-confirm button" "Go on editing"
    length <$> findAll ui ".editor-cancel-confirm" `shouldReturn` 0
    markdown ui `shouldReturn` "# Hello, world\n\nKept for later."
    editorText ui >>= (`shouldBeSameElement` text)
    find ui ".editor-cancel" >>= click
    clickButton ui ".editor-cancel-confirm button" "Keep the draft"
    -- Gone to the page, without being asked again whether to leave.
    find ui ".page-view .page-body h1" >>= (`shouldHaveText` "Hello, world")
    length <$> findAll ui ".leave-confirm" `shouldReturn` 0
    -- The draft is there the next time, and readers never see it.
    unchanged <- readPage wiki ["blog"] ["hello-world"]
    unchanged.body `shouldSatisfy` T.isPrefixOf "# Hello, world"
    find ui ".page-view .actions a[href$='/edit/']" >>= click
    find ui ".editor-draft-found" >>= textContent >>= (`shouldSatisfy` T.isInfixOf "Restored your unsaved draft from")
    markdown ui `shouldReturn` "# Hello, world\n\nKept for later."
    -- This time the changes are let go, and the draft with them.
    find ui ".editor-cancel" >>= click
    clickButton ui ".editor-cancel-confirm button" "Discard my changes"
    find ui ".page-view .page-body h1" >>= (`shouldHaveText` "Hello, world")
    find ui ".page-view .actions a[href$='/edit/']" >>= click
    markdown ui >>= (`shouldSatisfy` T.isPrefixOf "# Hello, world")
    length <$> findAll ui ".editor-draft-found" `shouldReturn` 0

  -- Issue #193: "Draft saved" beside the buttons pushed them along as it
  -- appeared, so that a click meant for Save could land on Cancel.
  it "shows Draft saved and Saved under the top bar, without moving the buttons" $ \clock -> runPage $ do
    let buttons = [".editor-save", ".editor-save-close", ".editor-cancel"]
        -- A notice's top edge is at or below the bottom of the editor's
        -- top, so that the notice covers none of the top's controls.
        belowTheTop notice = do
          box <- edges notice
          top <- edges ".editor-top"
          case (box, top) of
            ([_, noticeTop, _, _], [_, _, _, topBottom]) -> noticeTop `shouldSatisfy` (>= topBottom)
            _ -> expectationFailure ("no box for " <> notice <> " or .editor-top")
    (ui, _) <- startWith m (Just "user-editor") helloEditor
    text <- editorText ui
    typeText text " More."
    -- Typing turns Cancel from a link into a button that asks first, so
    -- the boxes are read after the typing, before the draft is saved.
    find ui ".editor-draft-status" >>= (`shouldHaveText` "")
    before <- traverse rectOf buttons
    draftNow clock
    find ui ".editor-draft-status" >>= (`shouldHaveText` "Draft saved")
    traverse rectOf buttons `shouldReturn` before
    belowTheTop ".editor-draft-status"
    -- "Saved" shows in the same place. Saving drops the draft, which turns
    -- Cancel back into a link, so only the two save buttons are compared.
    find ui ".editor-save" >>= click
    find ui ".editor-saved-notice" >>= (`shouldHaveText` "Saved")
    eventually (traverse rectOf (take 2 buttons) `shouldReturn` take 2 before)
    belowTheTop ".editor-saved-notice"

  -- Issue #339: with the search's panel open, "Draft saved" hung over the
  -- panel's Previous, Next and Close buttons.
  it "shows Draft saved clear of the open search's panel and its buttons" $ \clock -> runPage $ do
    (ui, _) <- startWith m (Just "user-editor") helloEditor
    text <- editorText ui
    typeText text " More."
    press "Control+f"
    _ <- find ui ".editor-search"
    -- The draft is saved after the typing's pause.
    draftNow clock
    find ui ".editor-draft-status" >>= (`shouldHaveText` "Draft saved")
    -- The notice's box and the panel's, left, top, right and bottom, share
    -- no point inside both.
    let apart = \case
          ([nl, nt, nr, nb], [pl, pt, pr, pb]) -> nr <= pl || pr <= nl || nb <= pt || pb <= nt
          _ -> False
    boxes <- (,) <$> edges ".editor-draft-status" <*> edges ".editor-search"
    boxes `shouldSatisfy` apart
    -- Each of the panel's buttons is what is drawn all over its box.
    for_ [".editor-search-previous", ".editor-search-next", ".editor-search-close"] $ \button ->
      onTop button `shouldReturn` ""

  it "goes back to the page on Cancel when nothing has changed" $ \_ -> runPage $ do
    (ui, _) <- startWith m (Just "user-editor") helloEditor
    find ui ".editor-cancel" >>= click
    find ui ".page-view .page-body h1" >>= (`shouldHaveText` "Hello, world")
    length <$> findAll ui ".leave-confirm" `shouldReturn` 0

  it "asks before leaving unsaved changes, and stays or leaves as told" $ \_ -> runPage $ do
    (ui, wiki) <- startWith m (Just "user-editor") helloEditor
    text <- editorText ui
    clearBody ui
    typeText text "Draft"
    find ui ".wiki-sidebar .page-tree a[href$='#/s/blog/p/notes-on-haskell/']" >>= click
    _ <- find ui ".leave-confirm"
    clickButton ui ".leave-confirm button" "Stay"
    length <$> findAll ui ".leave-confirm" `shouldReturn` 0
    markdown ui `shouldReturn` "# Hello, world\n\nDraft"
    -- Staying goes back to the editor's entry of the history, which the
    -- browser does after Stay returns: the next link is followed from
    -- there, not from the entry of the link not followed.
    eventually $ unsafeIOToPageM (fromJSText <$> js_location_hash) `shouldReturn` "#/s/blog/p/hello-world/edit/"
    -- Again, and leaving goes there; the work is lost.
    find ui ".wiki-sidebar .page-tree a[href$='#/s/blog/p/notes-on-haskell/']" >>= click
    find ui ".leave-confirm button.danger" >>= click
    find ui ".breadcrumbs [aria-current=page]" >>= (`shouldHaveText` "Notes on Haskell")
    unchanged <- readPage wiki ["blog"] ["hello-world"]
    unchanged.body `shouldSatisfy` T.isPrefixOf "# Hello, world"

  it "opens the conflict view on a save over someone else's, keeping the text as a draft, and keeps mine" $ \_ -> runPage $ do
    (ui, wiki) <- startWith m (Just "user-editor") helloEditor
    text <- editorText ui
    page <- someoneElseSaves wiki
    clearBody ui
    typeText text "My words."
    find ui ".editor-save" >>= click
    -- Their version, compared with the author's text, beside it.
    find ui ".editor-conflict" >>= textContent >>= (`shouldSatisfy` T.isInfixOf "Ada Admin saved this page")
    find ui ".editor-form" >>= (`shouldHaveClass` "conflicting")
    find ui ".editor-conflict-pane h2" >>= (`shouldHaveText` "Their version, compared with yours")
    find ui ".editor-conflict-pane .diff-removed .diff-text" >>= (`shouldHaveText` "My words.")
    find ui ".editor-conflict-pane .diff-added .diff-text" >>= (`shouldHaveText` "Someone else's words.")
    length <$> findAll ui ".error-conflict" `shouldReturn` 0
    -- Nothing is lost meanwhile: the text is in the editor and in the draft.
    markdown ui `shouldReturn` "# Hello, world\n\nMy words."
    eventually $ (fmap (\d -> d.body) <$> draftOf wiki page) `shouldReturn` Just "# Hello, world\n\nMy words."
    clickButton ui ".editor-conflict button" "Keep mine"
    find ui ".editor-saved-notice" >>= (`shouldHaveText` "Saved")
    length <$> findAll ui ".editor-conflict" `shouldReturn` 0
    length <$> findAll ui ".editor-conflict-pane" `shouldReturn` 0
    saved <- readPage wiki ["blog"] ["hello-world"]
    saved.body `shouldBe` "# Hello, world\n\nMy words."
    draftOf wiki page `shouldReturn` Nothing

  it "takes theirs in place of the author's text when told to" $ \_ -> runPage $ do
    (ui, wiki) <- startWith m (Just "user-editor") helloEditor
    text <- editorText ui
    _ <- someoneElseSaves wiki
    clearBody ui
    typeText text "My words."
    find ui ".editor-save" >>= click
    clickButton ui ".editor-conflict button" "Take theirs"
    eventually (markdown ui `shouldReturn` "# Hello, world\n\nSomeone else's words.")
    length <$> findAll ui ".editor-conflict-pane" `shouldReturn` 0
    find ui ".editor-show-preview" >>= click
    find ui ".editor-preview .page-body p" >>= (`shouldHaveText` "Someone else's words.")
    -- The toolbar works on the text loaded again, whose undo has nothing of
    -- what was there before.
    find ui ".editor-back" >>= click
    find ui ".editor-toolbar button[aria-label='Bold']" >>= click
    markdown ui `shouldReturn` "# Hello, world\n\n**bold text**Someone else's words."
    press "Control+z"
    markdown ui `shouldReturn` "# Hello, world\n\nSomeone else's words."
    -- And the next save is made over theirs.
    find ui ".editor-save" >>= click
    find ui ".editor-saved-notice" >>= (`shouldHaveText` "Saved")
    length <$> findAll ui ".editor-conflict" `shouldReturn` 0

  it "merges by hand: the author's text stays, their changes beside it, and the save is over theirs" $ \clock -> runPage $ do
    (ui, wiki) <- startWith m (Just "user-editor") helloEditor
    text <- editorText ui
    page <- someoneElseSaves wiki
    clearBody ui
    typeText text "My words."
    find ui ".editor-save" >>= click
    clickButton ui ".editor-conflict button" "Merge by hand"
    find ui ".editor-conflict-pane h2" >>= (`shouldHaveText` "Their changes since you began")
    -- What their save changed in the version the edit began from.
    find ui ".editor-conflict-pane .diff-added .diff-text" >>= (`shouldHaveText` "Someone else's words.")
    find ui ".editor-conflict-pane .diff-removed .diff-text" >>= (`shouldHaveText` "This is the first post of a blog that is also a wiki. Every page is Markdown, written in a split view with the page rendered next to it as you type.")
    markdown ui `shouldReturn` "# Hello, world\n\nMy words."
    focus text
    press "Control+End"
    typeText text " And theirs: someone else's words."
    -- The draft follows, as an edit of their version.
    draftNow clock
    (fmap (\d -> d.body) <$> draftOf wiki page) `shouldReturn` Just "# Hello, world\n\nMy words. And theirs: someone else's words."
    find ui ".editor-save" >>= click
    find ui ".editor-saved-notice" >>= (`shouldHaveText` "Saved")
    length <$> findAll ui ".editor-conflict-pane" `shouldReturn` 0
    saved <- readPage wiki ["blog"] ["hello-world"]
    saved.body `shouldBe` "# Hello, world\n\nMy words. And theirs: someone else's words."

  -- Typing in CodeMirror with the preview's turns between the keys ends in
  -- the text typed, in the editor, the preview and the page saved. The
  -- turns are put between the keys by the test's clock, which the
  -- preview's pause ('previewDelay', 'H.delayCounted') sleeps on: a turn
  -- after each key of the first words, then a run of keys with no turn
  -- between, and a turn after it. Each key's input is handled before its
  -- turn (the driver waits for it), so a render that comes between a key
  -- and its input, the fault of e9055578, is not this example's: the
  -- editor on the in-memory DOM puts it in that order ("Test.EditorDom").
  it "ends in the text typed when the preview's turns come between the keys" $ \clock -> runPage $ do
    (ui, wiki) <- startWith m (Just "user-editor") helloEditor
    sideBySide ui
    text <- editorText ui
    original <- markdown ui
    focus text
    press "Control+End"
    let turn = perform (advance clock previewDelay)
        pieces = [" w" <> show i | i <- [1 .. 12 :: Int]]
        long = T.replicate 3 " the quick brown fox jumps over the lazy dog"
    -- The clock holds the preview: a key, and the preview shows the text
    -- without it until the preview's turn.
    let previewText = find ui ".editor-preview .page-body" >>= textContent
    press "Z"
    previewText >>= (`shouldSatisfy` (not . T.isInfixOf "Z"))
    turn
    previewText >>= (`shouldSatisfy` T.isInfixOf "Z")
    for_ pieces $ \word -> for_ (T.unpack word) $ \c -> press (T.singleton c) >> turn
    traverse_ (press . T.singleton) (T.unpack long)
    turn
    let expected = original <> "Z" <> mconcat pieces <> long
    markdown ui `shouldReturn` expected
    previewText >>= (`shouldSatisfy` T.isInfixOf "w12 the quick")
    find ui ".editor-save-close" >>= click
    find ui ".page-view .page-body h1" >>= (`shouldHaveText` "Hello, world")
    saved <- readPage wiki ["blog"] ["hello-world"]
    saved.body `shouldBe` expected

  it "attaches a dropped image and embeds it at the cursor" $ \clock -> runPage $ do
    (ui, wiki) <- startWith m (Just "user-editor") helloEditor
    sideBySide ui
    text <- editorText ui
    clearBody ui
    typeText text "Look: "
    perform (dropFile ".editor-cm .cm-content" "dot.png" "image/png" dotPng)
    eventually $ markdown ui `shouldReturn` "# Hello, world\n\nLook: ![dot.png](attachment:dot.png)"
    -- The preview follows after its pause.
    previewNow clock
    image <- find ui ".editor-preview .page-body img[alt='dot.png']"
    getAttribute image "src" >>= (`shouldSatisfy` maybe False (T.isPrefixOf "blob:"))
    find ui ".editor-attachments li a" >>= (`shouldHaveText` "dot.png")
    find ui ".editor-save-close" >>= click
    _ <- find ui ".page-view .page-body img[alt='dot.png']"
    saved <- readPage wiki ["blog"] ["hello-world"]
    map (\a -> a.name.text) saved.attachments `shouldBe` ["dot.png"]

  it "attaches a pasted image at the cursor, and a file pasted into the textarea, putting in nothing else" $ \clock -> runPage $ do
    (ui, wiki) <- startWith m (Just "user-editor") helloEditor
    sideBySide ui
    text <- editorText ui
    clearBody ui
    typeText text "Look: "
    -- With the file's name as text beside it, as a file manager copies a
    -- file: the file is attached, and its name is not put in.
    prevented <- unsafeIOToPageM (pasteFile ".editor-cm .cm-content" "dot.png" "dot.png" "image/png" dotPng)
    prevented `shouldBe` True
    settle
    eventually $ markdown ui `shouldReturn` "# Hello, world\n\nLook: ![dot.png](attachment:dot.png)"
    -- The preview follows after its pause.
    previewNow clock
    image <- find ui ".editor-preview .page-body img[alt='dot.png']"
    getAttribute image "src" >>= (`shouldSatisfy` maybe False (T.isPrefixOf "blob:"))
    find ui ".editor-attachments li a" >>= (`shouldHaveText` "dot.png")
    -- A paste in the textarea (the editor before CodeMirror has come) is
    -- the panes' too, and the browser is kept from putting the name in.
    inTextarea <- unsafeIOToPageM (pasteFile ".editor-text" "notes.txt" "notes.txt" "text/plain" "aGVsbG8K")
    inTextarea `shouldBe` True
    settle
    eventually $ markdown ui `shouldReturn` "# Hello, world\n\nLook: ![dot.png](attachment:dot.png)[notes.txt](attachment:notes.txt)"
    findAll ui ".editor-attachments li a" >>= traverse textContent >>= (`shouldBe` ["dot.png", "notes.txt"])
    find ui ".editor-save-close" >>= click
    _ <- find ui ".page-view .page-body img[alt='dot.png']"
    saved <- readPage wiki ["blog"] ["hello-world"]
    map (\a -> a.name.text) saved.attachments `shouldBe` ["dot.png", "notes.txt"]
    saved.body `shouldBe` "# Hello, world\n\nLook: ![dot.png](attachment:dot.png)[notes.txt](attachment:notes.txt)"

  it "puts pasted text in unchanged but for its line endings, attaching nothing" $ \clock -> runPage $ do
    (ui, _) <- startWith m (Just "user-editor") helloEditor
    sideBySide ui
    text <- editorText ui
    attached <- length <$> findAll ui ".editor-attachments li"
    clearBody ui
    typeText text "Say: "
    _ <- unsafeIOToPageM (pasteText ".editor-cm .cm-content" "*Hello*, <b>world</b>\r\n\n- [x] one\r\n")
    settle
    eventually $ markdown ui `shouldReturn` "# Hello, world\n\nSay: *Hello*, <b>world</b>\n\n- [x] one\n"
    previewHas clock ui "b" "world"
    length <$> findAll ui ".editor-attachments li" `shouldReturn` attached

  it "attaches a chosen file as a link, its name with spaces in angle brackets" $ \clock -> runPage $ do
    (ui, _) <- startWith m (Just "user-editor") helloEditor
    sideBySide ui
    _ <- editorText ui
    clearBody ui
    perform (chooseFile ".editor-attach input" "meeting notes.txt" "text/plain" "aGVsbG8K")
    eventually $ markdown ui `shouldReturn` "# Hello, world\n\n[meeting notes.txt](<attachment:meeting notes.txt>)"
    -- The preview follows after its pause.
    previewNow clock
    download <- find ui ".editor-preview .page-body a[download='meeting notes.txt']"
    getAttribute download "href" >>= (`shouldSatisfy` maybe False (T.isPrefixOf "blob:"))

  it "makes a selected word bold from the toolbar, in the Markdown and the preview" $ \clock -> runPage $ do
    (ui, _) <- startWith m (Just "user-editor") helloEditor
    sideBySide ui
    text <- editorText ui
    clearBody ui
    typeText text "Some words"
    press "Control+Shift+ArrowLeft"
    editorSelection ui `shouldReturn` (21, 26)
    find ui ".editor-toolbar button[aria-label='Bold']" >>= click
    markdown ui `shouldReturn` "# Hello, world\n\nSome **words**"
    editorSelection ui `shouldReturn` (23, 28)
    previewHas clock ui "strong" "words"
    -- Again, and the word is plain again.
    find ui ".editor-toolbar button[aria-label='Bold']" >>= click
    markdown ui `shouldReturn` "# Hello, world\n\nSome words"

  it "makes every line selected a list item from the toolbar, and another list from a shortcut" $ \_ -> runPage $ do
    (ui, _) <- startWith m (Just "user-editor") helloEditor
    text <- editorText ui
    clearBody ui
    typeText text "one\ntwo\nthree"
    -- The three lines, under the title line, which is not one of them.
    press "Shift+ArrowUp"
    press "Shift+ArrowUp"
    press "Shift+Home"
    find ui ".editor-toolbar button[aria-label='Numbered list']" >>= click
    markdown ui `shouldReturn` "# Hello, world\n\n1. one\n2. two\n3. three"
    press "Control+Shift+8"
    markdown ui `shouldReturn` "# Hello, world\n\n- one\n- two\n- three"
    find ui ".editor-toolbar button[aria-label='Bulleted list']" >>= click
    markdown ui `shouldReturn` "# Hello, world\n\none\ntwo\nthree"

  -- Each case of 'enterCases' on the same page: its text under the title
  -- line, its selection made, its keys pressed, and the text and the
  -- cursor after them. What Enter writes is Wiki.Core.Markdown.Enter's,
  -- tested natively; these show the keymap asking it and making its
  -- answer, in the master.
  it "continues a list or a box on Enter, goes up a level and out on an empty line, and breaks the line alone on Shift+Enter" $ \_ -> runPage $ do
    (ui, _) <- startWith m (Just "user-editor") helloEditor
    _ <- editorText ui
    for_ enterCases $ \(name, before, keys, after) -> do
      let (text, start, end) = unmarked before
          offset = utf16Length titleLines
      setMarkdown ui (titleLines <> text)
      selectText ui (offset + utf16Length (T.take start text)) (offset + utf16Length (T.take end text))
      traverse_ press keys
      now <- T.drop (T.length titleLines) <$> markdown ui
      (cursorAt, _) <- editorSelection ui
      (name, marked now (cursorAt - offset)) `shouldBe` (name, after)

  -- Enter reads the text from where the last check of the whole text
  -- found a block starting with nothing open above it (Wiki.Core.Markdown.Starts),
  -- no more than Wiki.Core.Markdown.Enter's limit. A paragraph longer than
  -- the limit above a list: Enter there continues the list only from the
  -- list's start, which the check gives (its problem, a marker line of no
  -- block, is its signal). Then HTML that reads on over a heading and a
  -- list (Codex's case on #292): the list's line is HTML, and Enter a
  -- line break alone. Then a list of two hundred items, longer than the
  -- limit: each item starts a text Enter may read, so it goes on.
  it "reads the text from where the last check found a block starting, past a long paragraph, over HTML that reads on, and in a long list" $ \clock -> runPage $ do
    (ui, _) <- startWith m (Just "user-editor") helloEditor
    _ <- editorText ui
    let long = T.intercalate " " (replicate 500 "word")
        listed = titleLines <> long <> "\n\n[[!nothing]]\n\n- one"
        html = titleLines <> long <> "\n\n[[!nothing]]\n\n[[!nothing]]\n\n- a\n<a>\n# Heading\n- "
        enterAtEnd text count = do
          setMarkdown ui text
          -- The check runs after the preview's pause.
          previewNow clock
          length <$> problemsMarked ui `shouldReturn` count
          selectText ui (utf16Length text) (utf16Length text)
          press "Enter"
    enterAtEnd listed 1
    markdown ui `shouldReturn` listed <> "\n- "
    enterAtEnd html 2
    markdown ui `shouldReturn` html <> "\n"
    let items = titleLines <> "[[!nothing]]\n\n" <> T.intercalate "\n" ["- item " <> show i | i <- [1 .. 200 :: Int]]
    enterAtEnd items 1
    markdown ui `shouldReturn` items <> "\n- "

  it "leaves Ctrl+B's placeholder selected, for the typing to replace" $ \_ -> runPage $ do
    (ui, _) <- startWith m (Just "user-editor") helloEditor
    text <- editorText ui
    clearBody ui
    typeText text "Hi "
    press "Control+b"
    markdown ui `shouldReturn` "# Hello, world\n\nHi **bold text**"
    editorSelection ui `shouldReturn` (21, 30)
    typeText text "there"
    markdown ui `shouldReturn` "# Hello, world\n\nHi **there**"

  it "keeps Ctrl+K from the browser, and puts in a link the browser's undo takes back" $ \_ -> runPage $ do
    (ui, _) <- startWith m (Just "user-editor") helloEditor
    _ <- editorText ui
    clearBody ui
    unsafeIOToPageM js_watch_keys
    press "Control+k"
    unsafeIOToPageM js_last_key_prevented `shouldReturn` True
    markdown ui `shouldReturn` "# Hello, world\n\n[link text](https://)"
    editorSelection ui `shouldReturn` (17, 26)
    press "Control+z"
    markdown ui `shouldReturn` "# Hello, world\n\n"
    -- A key the editor does not answer to is the browser's.
    press "x"
    unsafeIOToPageM js_last_key_prevented `shouldReturn` False

  it "links a page found in the \"Link to a page\" dialog, with its title as the text" $ \clock -> runPage $ do
    (ui, _) <- startWith m (Just "user-editor") helloEditor
    sideBySide ui
    text <- editorText ui
    clearBody ui
    typeText text "See "
    find ui ".editor-toolbar button[aria-label='Link to a page']" >>= click
    find ui ".link-dialog .search-input" >>= (`typeText` "diagrams")
    pickerNow clock
    find ui ".link-dialog .search-hit .search-hit-title" >>= (`shouldHaveText` "Diagrams")
    -- Enter with no hit chosen picks the first.
    press "Enter"
    eventually $ length <$> findAll ui ".link-dialog" `shouldReturn` 0
    markdown ui `shouldReturn` "# Hello, world\n\nSee [Diagrams](/s/docs/p/markdown-guide/diagrams/)"
    -- The text has the focus again, after the link.
    typeText text "."
    markdown ui `shouldReturn` "# Hello, world\n\nSee [Diagrams](/s/docs/p/markdown-guide/diagrams/)."
    -- The address leads to the page.
    -- The preview follows after its pause.
    previewNow clock
    _ <- find ui ".editor-preview .page-body a[href$='#/s/docs/p/markdown-guide/diagrams/']"
    pure ()

  it "links the selection to a section of a page, from Ctrl+Shift+K" $ \clock -> runPage $ do
    (ui, _) <- startWith m (Just "user-editor") helloEditor
    text <- editorText ui
    clearBody ui
    typeText text "the macros"
    press "Control+Shift+ArrowLeft"
    press "Control+Shift+K"
    field <- find ui ".link-dialog .search-input"
    find ui ".link-dialog-section-toggle input" >>= click
    typeText field "mathematics"
    pickerNow clock
    hit <- do
      hits <- findAll ui ".link-dialog a.search-hit[href$='#/s/docs/p/markdown-guide/mathematics/']"
      maybe (expectationFailure "no hit for Mathematics") pure (head hits)
    click hit
    -- The page's sections, its title line not among them.
    find ui ".link-dialog-page" >>= (`shouldHaveText` "Sections of Mathematics")
    -- Listed once the page has been read.
    anchors <- eventually $ do
      listed <- traverse (`getAttribute` "data-anchor") =<< findAll ui ".link-dialog-anchor"
      listed `shouldSatisfy` elem (Just "macros")
      pure listed
    anchors `shouldSatisfy` notElem (Just "mathematics")
    find ui ".link-dialog-anchor[data-anchor='macros']" >>= click
    eventually $ length <$> findAll ui ".link-dialog" `shouldReturn` 0
    markdown ui `shouldReturn` "# Hello, world\n\nthe [macros](/s/docs/p/markdown-guide/mathematics/#macros)"

  it "writes nothing when the \"Link to a page\" dialog is closed" $ \clock -> runPage $ do
    (ui, _) <- startWith m (Just "user-editor") helloEditor
    _ <- editorText ui
    clearBody ui
    find ui ".editor-toolbar button[aria-label='Link to a page']" >>= click
    find ui ".link-dialog .search-input" >>= (`typeText` "diagrams")
    pickerNow clock
    find ui ".link-dialog .search-hit .search-hit-title" >>= (`shouldHaveText` "Diagrams")
    -- The first Escape may only clear the search input's text, as the
    -- browser does in a search field.
    press "Escape"
    press "Escape"
    eventually $ length <$> findAll ui ".link-dialog" `shouldReturn` 0
    markdown ui `shouldReturn` "# Hello, world\n\n"
    -- The header's search box is still the header's: its list is shut.
    length <$> findAll ui ".wiki-search .search-results" `shouldReturn` 0

  it "wraps the selection in an element from the HTML menu" $ \clock -> runPage $ do
    (ui, _) <- startWith m (Just "user-editor") helloEditor
    sideBySide ui
    text <- editorText ui
    clearBody ui
    typeText text "Look here"
    press "Control+Shift+ArrowLeft"
    find ui ".editor-toolbar button[aria-label='HTML']" >>= click
    clickButton ui ".toolbar-menu button" "Mark<mark>"
    markdown ui `shouldReturn` "# Hello, world\n\nLook <mark>here</mark>"
    length <$> findAll ui ".toolbar-menu" `shouldReturn` 0
    previewHas clock ui "mark" "here"

  it "makes the lines selected a box from the Box menu, numbered in the preview, and changes its kind" $ \clock -> runPage $ do
    (ui, _) <- startWith m (Just "user-editor") helloEditor
    sideBySide ui
    text <- editorText ui
    clearBody ui
    typeText text "Read this first"
    press "Shift+Home"
    find ui ".editor-toolbar button[aria-label='Box']" >>= click
    -- GitHub's five, then the demo's own kind; no "New kind…" for an editor.
    labels <- traverse textContent =<< findAll ui ".box-menu button"
    labels `shouldBe` ["Note[!NOTE]", "Tip[!TIP]", "Important[!IMPORTANT]", "Warning[!WARNING]", "Caution[!CAUTION]", "Exercise[!EXERCISE]"]
    length <$> findAll ui ".box-menu-new" `shouldReturn` 0
    clickButton ui ".box-menu button" "Tip[!TIP]"
    markdown ui `shouldReturn` "# Hello, world\n\n> [!TIP]\n> Read this first"
    length <$> findAll ui ".toolbar-menu" `shouldReturn` 0
    previewHas clock ui ".alert-tip .alert-title" "Tip 1"
    -- On the marker's line, another kind replaces the box's.
    find ui ".editor-toolbar button[aria-label='Box']" >>= click
    clickButton ui ".box-menu button" "Exercise[!EXERCISE]"
    markdown ui `shouldReturn` "# Hello, world\n\n> [!EXERCISE]\n> Read this first"
    previewHas clock ui ".alert-exercise .alert-title" "Exercise 1"

  it "hides the selected words as an inline spoiler from the toolbar, and the selected lines as a block spoiler" $ \clock -> runPage $ do
    (ui, _) <- startWith m (Just "user-editor") helloEditor
    sideBySide ui
    text <- editorText ui
    clearBody ui
    typeText text "Some words"
    press "Control+Shift+ArrowLeft"
    find ui ".editor-toolbar button[aria-label='Inline spoiler']" >>= click
    markdown ui `shouldReturn` "# Hello, world\n\nSome ||words||"
    previewHas clock ui "span.spoiler" "words"
    -- The line, from its start, in a block spoiler, its name selected to
    -- be typed over.
    press "End"
    press "Shift+Home"
    find ui ".editor-toolbar button[aria-label='Block spoiler']" >>= click
    markdown ui `shouldReturn` "# Hello, world\n\n[[!spoiler]]{name}(Description)\nSome ||words||\n[[/spoiler]]"
    editorSelection ui `shouldReturn` (29, 33)
    previewHas clock ui "details.spoiler > summary" "Description"

  it "shows a spoiler that is not closed in red in the preview as it is typed, and as a spoiler once it is" $ \clock -> runPage $ do
    (ui, _) <- startWith m (Just "user-editor") helloEditor
    sideBySide ui
    text <- editorText ui
    clearBody ui
    typeText text "[[!spoiler]]{answer}(The answer)\nForty-two."
    previewHas clock ui ".spoiler-error .spoiler-error-reason" "Spoiler “answer” is not closed: add [[/spoiler]]{answer}"
    previewHas clock ui ".spoiler-unclosed p" "Forty-two."
    computedStyle ".editor-preview .spoiler-unclosed" "color" >>= (`shouldNotBe` "")
    typeText text "\n[[/spoiler]]{answer}"
    previewHas clock ui "details.spoiler[data-spoiler='answer'] > summary" "The answer"
    length <$> findAll ui ".editor-preview .spoiler-error" `shouldReturn` 0

  it "defines a new kind of box in a dialog, without leaving the page, writes a box of it, and lists it at once" $ \_ -> runPage $ do
    (ui, wiki) <- startWith m (Just "user-admin") helloEditor
    find ui ".editor-toolbar button[aria-label='Box']" >>= click
    find ui ".box-menu .box-menu-new" >>= (`shouldHaveText` "New kind…")
    find ui ".box-menu .box-menu-new" >>= click
    -- The administration's form, in a dialog over the editor. One of
    -- GitHub's five is refused, and the dialog stays with what was typed.
    kind <- find ui ".new-kind-dialog .server-box-form input[name=kind]"
    typeText kind "note"
    find ui ".new-kind-dialog .server-box-form button[type=submit]" >>= click
    find ui ".new-kind-dialog .error" >>= textContent >>= (`shouldSatisfy` T.isInfixOf "one of GitHub's five")
    clear kind
    typeText kind "lemma"
    find ui ".new-kind-dialog .server-box-form input[name=label]" >>= (`typeText` "Lemma")
    find ui ".new-kind-dialog .server-box-form button[type=submit]" >>= click
    eventually (length <$> findAll ui ".new-kind-dialog" `shouldReturn` 0)
    -- The server keeps it, a box of it is in the text, and the menu lists
    -- it without a reload.
    kinds <- unsafeIOToPageM (runMemWiki wiki (either (panic . show) (\s -> s.boxes) <$> serverSettings))
    map (\k -> k.name) kinds `shouldSatisfy` elem "lemma"
    markdown ui >>= (`shouldSatisfy` T.isInfixOf "> [!LEMMA]")
    find ui ".editor-toolbar button[aria-label='Box']" >>= click
    eventually (find ui ".box-menu button[data-kind='LEMMA']" >>= textContent >>= (`shouldSatisfy` T.isPrefixOf "Lemma"))
    -- The dialog's form, refused or saved, did not save the page.
    blog <- unsafeIOToPageM (runMemWiki wiki (either (panic . show) identity <$> getSpace (slugs ["blog"])))
    stored <- unsafeIOToPageM (runMemWiki wiki (either (panic . show) identity <$> getPage blog.id (slugs ["hello-world"])))
    stored.body `shouldSatisfy` (not . T.isInfixOf "[!LEMMA]")

  it "offers no new kind of box to someone who is not an administrator" $ \_ -> runPage $ do
    (ui, _) <- startWith m (Just "user-editor") helloEditor
    find ui ".editor-toolbar button[aria-label='Box']" >>= click
    _ <- find ui ".box-menu button[data-kind='TIP']"
    length <$> findAll ui ".box-menu .box-menu-new" `shouldReturn` 0

  it "edits the page's block of macros and the space's from the Math macros panel" $ \clock -> runPage $ do
    (ui, wiki) <- startWith m (Just "user-editor") helloEditor
    sideBySide ui
    text <- editorText ui
    clearBody ui
    typeText text "Half: $\\half + \\twice$"
    find ui ".editor-toolbar .editor-macros-toggle" >>= click
    _ <- find ui ".editor-macros"
    -- The page's block, applied into the text under the title.
    page <- find ui ".editor-macros-page .macros-source"
    -- A block that would show something is refused: the page could not
    -- find it again as its block of definitions.
    typeText page "\\newcommand{\\half}{\\frac{1}{2}}x"
    eventually (find ui ".editor-macros-page .macros-error" >>= textContent >>= (`shouldSatisfy` T.isInfixOf "only definitions"))
    find ui ".editor-macros-page button[type=submit]" >>= (`getProperty` "disabled") >>= (`shouldBe` "true")
    clear page
    typeText page "\\newcommand{\\half}{\\frac{1}{2}}"
    eventually (length <$> findAll ui ".editor-macros-page .macros-error" `shouldReturn` 0)
    find ui ".editor-macros-page button[type=submit]" >>= click
    markdown ui `shouldReturn` "# Hello, world\n\n```math\n\\newcommand{\\half}{\\frac{1}{2}}\n```\n\nHalf: $\\half + \\twice$"
    -- The block shows nothing in the preview; the formula has the macro.
    previewNow clock
    length <$> findAll ui ".editor-preview .wiki-math-display" `shouldReturn` 0
    -- The space's, which an editor of the whole space saves at once.
    space <- find ui ".editor-macros-space .macros-source"
    typeText space "\\newcommand{\\twice}{2}"
    find ui ".editor-macros-space button[type=submit]" >>= click
    eventually (find ui ".editor-macros-space .settings-saved" >>= (`shouldHaveText` "Saved"))
    blog <- unsafeIOToPageM (runMemWiki wiki (either (panic . show) identity <$> getSpace (slugs ["blog"])))
    blog.mathMacros `shouldBe` "\\newcommand{\\twice}{2}"
    -- Neither panel's form saved the page: the text with the page's block
    -- is still only in the editor.
    stored <- unsafeIOToPageM (runMemWiki wiki (either (panic . show) identity <$> getPage blog.id (slugs ["hello-world"])))
    stored.body `shouldSatisfy` (not . T.isInfixOf "\\half")

  it "colours a word from the palette, changes its colour, highlights it and takes the colour away" $ \clock -> runPage $ do
    (ui, _) <- startWith m (Just "user-editor") helloEditor
    sideBySide ui
    text <- editorText ui
    clearBody ui
    typeText text "Look here"
    press "Control+Shift+ArrowLeft"
    let pick swatch = do
          find ui ".editor-toolbar button[aria-label='Colour']" >>= click
          find ui (".colour-menu button[aria-label='" <> swatch <> "']") >>= click
    pick "Red text"
    markdown ui `shouldReturn` "# Hello, world\n\nLook <span class=\"c-red\">here</span>"
    editorSelection ui `shouldReturn` (41, 45)
    length <$> findAll ui ".toolbar-menu" `shouldReturn` 0
    previewHas clock ui "span.c-red" "here"
    -- The style sheet draws the class, in a colour of its own.
    plain <- computedColour ".editor-preview .page-body p"
    red <- computedColour ".editor-preview .page-body span.c-red"
    red `shouldNotBe` plain
    red `shouldNotBe` ""
    -- Another colour replaces the first, rather than going inside it.
    pick "Blue text"
    markdown ui `shouldReturn` "# Hello, world\n\nLook <span class=\"c-blue\">here</span>"
    previewHas clock ui "span.c-blue" "here"
    pick "Yellow highlight"
    markdown ui `shouldReturn` "# Hello, world\n\nLook <span class=\"c-blue\"><mark class=\"h-yellow\">here</mark></span>"
    pick "No text colour"
    markdown ui `shouldReturn` "# Hello, world\n\nLook <mark class=\"h-yellow\">here</mark>"
    previewHas clock ui "mark.h-yellow" "here"
    pick "No highlight"
    markdown ui `shouldReturn` "# Hello, world\n\nLook here"

  it "shows the Markdown as it will look while it is typed, its markers dimmed" $ \_ -> runPage $ do
    (ui, wiki) <- startWith m (Just "user-editor") helloEditor
    text <- editorText ui
    clearBody ui
    typeText text "Some **bold** and _it_.\n\n```python\ndef f():\n```"
    let typed = "# Hello, world\n\nSome **bold** and _it_.\n\n```python\ndef f():\n```"
        -- The text of every element of a class, in order.
        classed cls = mconcat <$> (findAll ui (".editor-cm ." <> cls) >>= traverse textContent)
    -- The title line is a level-one heading.
    eventually $ classed "cm-md-h1" `shouldReturn` "# Hello, world"
    classed "cm-md-h1.cm-md-syntax" `shouldReturn` "#"
    classed "cm-md-strong" `shouldReturn` "**bold**"
    classed "cm-md-strong.cm-md-syntax" `shouldReturn` "****"
    classed "cm-md-emphasis" `shouldReturn` "_it_"
    classed "cm-md-emphasis.cm-md-syntax" `shouldReturn` "__"
    -- Fenced code on its own background, highlighted as the page will be.
    length <$> findAll ui ".editor-cm .cm-line.cm-md-codeblock" `shouldReturn` 3
    classed "hljs-keyword" `shouldReturn` "def"
    -- And the style sheet draws them so: a heading larger, strong bold,
    -- emphasis in italics, the markers in a colour of their own.
    body <- computedStyle ".editor-cm .cm-content" "font-size"
    heading <- computedStyle ".editor-cm .cm-md-h1" "font-size"
    (readPx heading > readPx body) `shouldBe` True
    computedStyle ".editor-cm .cm-md-strong" "font-weight" `shouldReturn` "700"
    computedStyle ".editor-cm .cm-md-emphasis" "font-style" `shouldReturn` "italic"
    marker <- computedColour ".editor-cm .cm-md-strong.cm-md-syntax"
    plain <- computedColour ".editor-cm .cm-content"
    marker `shouldNotBe` plain
    -- What is saved is what was typed, byte for byte.
    markdown ui `shouldReturn` typed
    find ui ".editor-save" >>= click
    eventually $ (\p -> p.body) <$> readPage wiki ["blog"] ["hello-world"] >>= (`shouldBe` typed)

  it "takes back a toolbar command as one step of the undo, and the typing after it as another" $ \_ -> runPage $ do
    (ui, _) <- startWith m (Just "user-editor") helloEditor
    text <- editorText ui
    clearBody ui
    typeText text "Some words"
    press "Control+Shift+ArrowLeft"
    -- A colour rewrites more than the selection, still one step.
    find ui ".editor-toolbar button[aria-label='Colour']" >>= click
    find ui ".colour-menu button[aria-label='Red text']" >>= click
    markdown ui `shouldReturn` "# Hello, world\n\nSome <span class=\"c-red\">words</span>"
    press "ArrowRight"
    typeText text "!"
    markdown ui `shouldReturn` "# Hello, world\n\nSome <span class=\"c-red\">words!</span>"
    press "Control+z"
    markdown ui `shouldReturn` "# Hello, world\n\nSome <span class=\"c-red\">words</span>"
    press "Control+z"
    markdown ui `shouldReturn` "# Hello, world\n\nSome words"

  it "shows the preview on its own, and comes back to the Markdown as it was left" $ \_ -> runPage $ do
    (ui, _) <- startWith m (Just "user-admin") (EditPage (slugs ["docs"]) (slugs ["markdown-guide"]))
    text <- editorText ui
    original <- markdown ui
    length <$> findAll ui ".editor-preview" `shouldReturn` 0
    focus text
    press "Control+End"
    typeText text "\n\nTyped last."
    for_ [1 .. 5 :: Int] (const (press "ArrowLeft"))
    -- The cursor five characters from the end: 'at' counts characters, to
    -- cut the text; the selection counts UTF-16 units, as the browser
    -- does, and the guide has characters of two units (🤔) before it.
    let typed = original <> "\n\nTyped last."
        at = T.length typed - 5
        atUnits = utf16Length typed - 5
    editorSelection ui `shouldReturn` (atUnits, atUnits)
    scroller <- find ui ".editor-cm .cm-scroller"
    scrolled <- getProperty scroller "scrollTop"
    scrolled `shouldNotBe` "0"
    find ui ".editor-show-preview" >>= click
    shouldBeHidden text
    find ui ".editor-preview .page-body" >>= textContent >>= (`shouldSatisfy` T.isInfixOf "Typed last.")
    find ui ".editor-save" >>= shouldBeVisible
    find ui ".editor-back" >>= click
    shouldBeVisible text
    shouldBeHidden =<< find ui ".editor-preview"
    markdown ui `shouldReturn` typed
    editorSelection ui `shouldReturn` (atUnits, atUnits)
    getProperty scroller "scrollTop" `shouldReturn` scrolled
    -- The focus is back at the cursor: the next key goes there.
    press "X"
    markdown ui `shouldReturn` (T.take at typed <> "X" <> T.drop at typed)

  it "shows both panes with the side by side preview, and remembers it" $ \_ -> runPage $ do
    (ui, _) <- startWith m (Just "user-editor") helloEditor
    length <$> findAll ui ".editor-preview" `shouldReturn` 0
    sideBySide ui
    editorText ui >>= shouldBeVisible
    find ui ".editor-preview" >>= shouldBeVisible
    find ui ".editor-preview .page-body h1" >>= (`shouldHaveText` "Hello, world")
    -- Out of the editor and back into it: the choice is the browser's.
    find ui ".editor-cancel" >>= click
    find ui ".page-view .actions a[href$='/edit/']" >>= click
    find ui ".editor-side-by-side" >>= \button -> getAttribute button "aria-pressed" `shouldReturn` Just "true"
    find ui ".editor-preview" >>= shouldBeVisible

  it "finds TeX math in the text where the wiki's own parser does" $ \_ -> runPage $ do
    (ui, _) <- startWith m (Just "user-editor") helloEditor
    -- The cases of commonmark-extensions' math and of TexMath: delimiters
    -- that open and close, a blank or a digit that stops them, braces, a
    -- backslash, a formula over two lines, and math beside a mention.
    let cases =
          [ "$x$"
          , "$ x$"
          , "$x $"
          , "$5 and $6"
          , "$x$5"
          , "a $x$ and $y$ b"
          , "$a{$}b$"
          , "$a}b$"
          , "$\\$x$"
          , "$\\alpha + 1$"
          , "$a\nb$"
          , "$$x$$"
          , "$$a $ b$$"
          , "$$ $$"
          , "\\(x\\)"
          , "\\( \\)"
          , "\\[a \\\\ b\\]"
          , "\\(x"
          , "`$x$` $y$"
          , "$x @an y$ @an"
          ]
    -- Each under a title line, which the editor keeps.
    for_ cases $ \source -> do
      setMarkdown ui ("# Cases\n\n" <> source)
      found <- map stripDelimiters <$> mathIn ui
      (source, found) `shouldBe` (source, wikiMath source)

  it "underlines inline math left open on its line, from its opening to the line's end, and nothing else" $ \_ -> runPage $ do
    (ui, _) <- startWith m (Just "user-editor") helloEditor
    let underlined = T.concat <$> (findAll ui ".cm-wiki-unclosed-math" >>= traverse textContent)
    -- Open: a $ closed by nothing, a \( and a $ closed on the next line
    -- only. Closed or not math: a formula, \(…\), a price, display math
    -- over lines, code and an escaped $.
    setMarkdown ui "# Cases\n\nOpen $x + y here\n\nClosed $x$ and \\(y\\), a price of $5, and $$\nz\n$$\n\nLate \\(a\nb\\) and $c\nd$\n\nCode `$x` and \\$x"
    eventually $ underlined `shouldReturn` "$x + y here\\(a$c"
    find ui ".cm-wiki-unclosed-math" >>= (`getAttribute` "title") >>= (`shouldBe` Just "This formula is not closed on its line.")
    -- Closed, the line is no longer underlined.
    setMarkdown ui "# Cases\n\nOpen $x + y$ here"
    eventually $ underlined `shouldReturn` ""
    -- A formula that starts with a number is not a price, and a dollar in
    -- a link's title is not math.
    setMarkdown ui "# Cases\n\nCompute $2x + y here\n\nThen $2 + x\n\nSee [this](other \"Amount: $USD\") and $3.50 or $5 each."
    eventually $ underlined `shouldReturn` "$2x + y here$2 + x"

  it "completes a mention with the users whose names start with what is typed, but not in code, a link or math" $ \clock -> runPage $ do
    ui <- startChanged m withMentionable (Just "user-editor") helloEditor
    text <- editorText ui
    clearBody ui
    typeText text "Ask @an"
    -- Anna by her login and Annabel by her display name, display name
    -- first and the login after it, in the order of their display names;
    -- not Annex, whose login a mention cannot name.
    shown <- listShown ui
    (findAllIn shown ".cm-completionLabel" >>= traverse textContent) `shouldReturn` ["Anna Nowak", "Annabel Smith"]
    (findAllIn shown ".cm-completionDetail" >>= traverse textContent) `shouldReturn` ["@anna", "@jsmith"]
    press "Enter"
    markdown ui `shouldReturn` "# Hello, world\n\nAsk @anna "
    eventually $ length <$> findAll ui ".cm-tooltip-autocomplete" `shouldReturn` 0
    -- The arrows choose another, and Tab takes it rather than indenting.
    typeText text "and @AN"
    _ <- listShown ui
    press "ArrowDown"
    press "Tab"
    markdown ui `shouldReturn` "# Hello, world\n\nAsk @anna and @jsmith "
    -- Escape closes the list and leaves the text as typed.
    typeText text "or @an"
    _ <- listShown ui
    press "Escape"
    eventually $ length <$> findAll ui ".cm-tooltip-autocomplete" `shouldReturn` 0
    markdown ui `shouldReturn` "# Hello, world\n\nAsk @anna and @jsmith or @an"
    -- Chosen with a space after the cursor already: no second space.
    clearBody ui
    typeText text "Ask  now"
    typeAfter 4 ["@", "a", "n"]
    _ <- listShown ui
    press "Enter"
    markdown ui `shouldReturn` "# Hello, world\n\nAsk @anna now"
    -- Brackets with no link defined are text, where a mention counts.
    clearBody ui
    typeText text "[note ]"
    typeAfter 6 ["@", "a", "n"]
    _ <- listShown ui
    press "Escape"
    eventually $ length <$> findAll ui ".cm-tooltip-autocomplete" `shouldReturn` 0
    -- Not in a code fence, where a mention does not count.
    clearBody ui
    typeText text "```\n@an"
    noCompletion ui
    -- Nor in a link's text: typed between the brackets of one.
    clearBody ui
    typeText text "[see ](https://example.org)"
    typeAfter 5 ["@", "a", "n"]
    markdown ui `shouldReturn` "# Hello, world\n\n[see @an](https://example.org)"
    noCompletion ui
    -- Nor in a link to a page of the wiki, [[page]]. The letters typed in
    -- its address are looked up among the pages of other spaces too
    -- (#301), after a pause that sleeps on this group's clock: the
    -- question is awaited, the clock advanced past the pause, and the
    -- lookup's answer taken, before the list is looked for.
    clearBody ui
    typeText text "[[Plans ]]"
    watchElsewhere ui "[[Plans @an"
    typeAfter 8 ["@", "a", "n"]
    markdown ui `shouldReturn` "# Hello, world\n\n[[Plans @an]]"
    elsewhereAfterPause clock ui
    noCompletion ui
    -- Nor in its label.
    clearBody ui
    typeText text "[[plans|Plans ]]"
    typeAfter 14 ["@", "a", "n"]
    markdown ui `shouldReturn` "# Hello, world\n\n[[plans|Plans @an]]"
    noCompletion ui
    -- Nor in math, between dollars or TeX's own delimiters.
    clearBody ui
    typeText text "$x y$"
    typeAfter 2 [" ", "@", "a", "n"]
    markdown ui `shouldReturn` "# Hello, world\n\n$x @an y$"
    noCompletion ui
    clearBody ui
    typeText text "\\(\\)"
    typeAfter 2 ["@", "a", "n"]
    markdown ui `shouldReturn` "# Hello, world\n\n\\(@an\\)"
    noCompletion ui
    -- After the formula, where a mention counts again, it opens.
    press "End"
    traverse_ press [" ", "@", "a", "n"]
    _ <- listShown ui
    pass

  -- #301: after [[ and two letters, the pages of other spaces are looked
  -- up once a pause has ended, which sleeps on this group's clock until
  -- the example advances it: the lookup is out for as long as the example
  -- chooses. The space's own pages are shown meanwhile and take the keys;
  -- Escape closes the list then, and the answer that comes after it opens
  -- nothing. The group itself, and Enter on one of its pages, are
  -- "Test.Browser.Complete"'s.
  it "shows the space's own pages after [[ while other spaces' are looked up, and Escape then closes the list for good" $ \clock -> runPage $ do
    ui <- startChanged m (withPage "blog" "Onboarding notes" . withPage "docs" "Onboarding") (Just "user-editor") helloEditor
    text <- editorText ui
    clearBody ui
    typeText text "See [[onb"
    -- The space's own list, ready for its keys, with the other spaces'
    -- pages still asked for: their pause sleeps on the clock.
    listReady ui
    listLabels ui `shouldReturn` ["Onboarding notes"]
    completing ui `shouldReturn` "pending"
    press "Escape"
    length <$> findAll ui ".cm-tooltip-autocomplete" `shouldReturn` 0
    -- The pause ends and the lookup answers, after the Escape: no list.
    waitQuiet
    perform (advance clock elsewhereDelay)
    waitQuiet
    completing ui `shouldReturn` ""
    length <$> findAll ui ".cm-tooltip-autocomplete" `shouldReturn` 0
    markdown ui `shouldReturn` "# Hello, world\n\nSee [[onb"
    -- Typed on, the list shows both groups once the lookup answers.
    typeText text "o"
    listReady ui
    waitQuiet
    perform (advance clock elsewhereDelay)
    waitQuiet
    completionAnswered ui `shouldReturn` "active"
    listLabels ui `shouldReturn` ["Onboarding notes", "Onboarding"]

  -- #301: the reader changes (the demo's switcher, clicked with the focus
  -- left in the text, as another tab's change would come) while the list
  -- shows pages of other spaces found for the reader before. The list is
  -- closed, entries and all: Enter then writes none of them.
  it "closes the list of other spaces' pages when the reader changes, and Enter then chooses none of them" $ \clock -> runPage $ do
    ui <- startChanged m (withPage "docs" "Onboarding") (Just "user-editor") helloEditor
    text <- editorText ui
    clearBody ui
    watchElsewhere ui "See [[onb"
    typeText text "See [[onb"
    elsewhereAfterPause clock ui
    completionAnswered ui `shouldReturn` "active"
    listLabels ui `shouldReturn` ["Onboarding"]
    -- Another reader: the administrator.
    clickKeepingFocus ".user-switcher-toggle"
    clickKeepingFocus ".user-option[data-login='admin']"
    waitQuiet
    find ui ".user-switcher-toggle" >>= (`shouldHaveText` "Ada Admin")
    completing ui `shouldReturn` ""
    length <$> findAll ui ".cm-tooltip-autocomplete" `shouldReturn` 0
    press "Enter"
    markdown ui >>= (`shouldSatisfy` (not . T.isInfixOf "/s/docs"))

  -- #301, as above, with letters that match no page of the space and a
  -- page of another: the group alone is shown, and Enter writes its link;
  -- and Escape while the lookup is out, with no list shown, gives the
  -- completion up, so the answer after it opens nothing.
  it "shows other spaces' pages alone when the space has none for the letters, Enter writing the link, and Escape before their answer opens nothing" $ \clock -> runPage $ do
    ui <- startChanged m (withPage "docs" "Zebra handbook") (Just "user-editor") helloEditor
    text <- editorText ui
    clearBody ui
    watchElsewhere ui "See [[zeb"
    typeText text "See [[zeb"
    elsewhereAfterPause clock ui
    completionAnswered ui `shouldReturn` "active"
    listLabels ui `shouldReturn` ["Zebra handbook"]
    (findAll ui ".cm-tooltip-autocomplete completion-section" >>= traverse textContent) `shouldReturn` ["In other spaces"]
    untilListTakesKeys ui
    press "Enter"
    markdown ui `shouldReturn` "# Hello, world\n\nSee [[/s/docs/p/zebra-handbook/|Zebra handbook]]"
    -- Escape while the lookup is out: no list is shown, and the
    -- completion is still asked for.
    clearBody ui
    watchElsewhere ui "See [[zeb"
    typeText text "See [[zeb"
    elsewhereAsked ui
    waitQuiet
    completing ui `shouldReturn` "pending"
    length <$> findAll ui ".cm-tooltip-autocomplete" `shouldReturn` 0
    press "Escape"
    completing ui `shouldReturn` ""
    -- The pause ends and the lookup answers, after the Escape: no list.
    perform (advance clock elsewhereDelay)
    waitQuiet
    completing ui `shouldReturn` ""
    length <$> findAll ui ".cm-tooltip-autocomplete" `shouldReturn` 0
    markdown ui `shouldReturn` "# Hello, world\n\nSee [[zeb"
  where
    helloEditor = EditPage (slugs ["blog"]) (slugs ["hello-world"])
    -- The editor's question about the pages of other spaces, watched for
    -- before the keys ('watchElsewhere'), asked and taken by the program:
    -- its pause asleep on the clock, which is advanced past it, and the
    -- lookup's answer taken.
    elsewhereAfterPause clock ui = do
      elsewhereAsked ui
      waitQuiet
      perform (advance clock elsewhereDelay)
      waitQuiet
    -- The labels of the completion's list shown, in its order.
    listLabels ui = findAll ui ".cm-tooltip-autocomplete .cm-completionLabel" >>= traverse textContent
    -- A page with this title at the top of the demo's space of this slug.
    withPage :: Text -> Text -> MemState -> MemState
    withPage spaceSlug title st = case filter (\s -> slugText s.slug == spaceSlug) (Map.elems st.spaces) of
      s : _ -> fst (insertPage s.id Nothing title ("# " <> title <> "\n") demoNow st)
      [] -> panic ("no space " <> spaceSlug)
    -- The TeX of each formula the wiki's parser finds, in the order of the
    -- text: in a paragraph, or a block of its own for display math alone
    -- in one.
    wikiMath source = case parseMarkdown source of
      Right blocks -> concatMap formulas blocks
      Left _ -> []
    formulas = \case
      Para inlines -> [tex | Math _ tex <- inlines]
      MathBlock tex -> [tex]
      _ -> []
    -- A formula as the editor's tree has it, without its delimiters.
    stripDelimiters t
      | "$$" `T.isPrefixOf` t = T.dropEnd 2 (T.drop 2 t)
      | "$" `T.isPrefixOf` t = T.dropEnd 1 (T.drop 1 t)
      | otherwise = T.dropEnd 2 (T.drop 2 t)
    -- Two users whose names start with "an", one by login and one by
    -- display name, one whose login a mention cannot name, and one who
    -- does not start so.
    withMentionable =
      insertUser User {id = UserId "user-anna", login = Login "anna", displayName = "Anna Nowak", avatar = Nothing}
        . insertUser User {id = UserId "user-jsmith", login = Login "jsmith", displayName = "Annabel Smith", avatar = Nothing}
        . insertUser User {id = UserId "user-annex", login = Login "annex@example.org", displayName = "Annex Corp", avatar = Nothing}
        . insertUser User {id = UserId "user-bob", login = Login "bob", displayName = "Bob Banner", avatar = Nothing}
    -- Type keys after the first n characters of the line.
    typeAfter n keys = do
      press "Home"
      replicateM_ n (press "ArrowRight")
      traverse_ press keys
    -- The list, open and ready to choose from. The library takes Enter
    -- and Tab only once the list has been open for 75 ms (its
    -- interactionDelay), so that a key pressed just as it opens is the
    -- text's. The list opened before it was seen here, so a wait of more
    -- than that after seeing it is always enough, however slow the page.
    listShown ui = do
      eventually (completing ui `shouldReturn` "active")
      shown <- find ui ".cm-tooltip-autocomplete"
      unsafeIOToPageM (threadDelay 100_000)
      pure shown
    -- No list opens. The keys just typed have made CodeMirror ask its
    -- source ("pending"); once it has had the answer and opened nothing,
    -- its completion is idle again (""), with no list.
    noCompletion ui = do
      eventually (completing ui `shouldReturn` "")
      length <$> findAll ui ".cm-tooltip-autocomplete" `shouldReturn` 0

-- | Turn the side by side preview on, so that the preview follows the
-- typing beside the Markdown.
sideBySide :: Mounted s q o m -> PageM s ()
sideBySide ui = do
  find ui ".editor-side-by-side" >>= click
  find ui ".editor-side-by-side" >>= \b -> getAttribute b "aria-pressed" `shouldReturn` Just "true"
  find ui ".editor-preview" >>= shouldBeVisible

-- | The preview, once it has followed the text, has an element with this
-- text: the clock is advanced past the preview's pause first.
previewHas :: TestClock -> Mounted s q o m -> Text -> Text -> PageM s ()
previewHas clock ui selector expected = do
  previewNow clock
  find ui (".editor-preview .page-body " <> selector) >>= (`shouldHaveText` expected)

-- | Advance the test's clock past the preview's pause, and the check's,
-- which is as long, and wait for what they woke: the preview has followed
-- the text, and the check, which runs on a worker, has come back.
previewNow :: TestClock -> PageM s ()
previewNow clock = perform (advance clock previewDelay) >> waitQuiet

-- | Advance the test's clock past the draft's pause, and wait for the
-- draft to be written. The pause starts again if the text changed while
-- it ran, as it does when its first key came before the last, so two of
-- them are advanced.
draftNow :: TestClock -> PageM s ()
draftNow clock = perform (advance clock (2 * draftDelay)) >> waitQuiet

-- | Advance the test's clock past the link dialog's pause in the typing,
-- and wait for its search to come back.
pickerNow :: TestClock -> PageM s ()
pickerNow clock = perform (advance clock PagePicker.pause) >> waitQuiet

-- | A property the style sheet gives the first element a selector finds.
computedStyle :: Text -> Text -> PageM s Text
computedStyle selector property = unsafeIOToPageM (fromJSText <$> js_computed_style (toJSText selector) (toJSText property))

-- | Where the first element a selector finds is drawn in the window, as
-- "left top right bottom", or "" when there is none.
rectOf :: Text -> PageM s Text
rectOf selector = unsafeIOToPageM (fromJSText <$> js_rect (toJSText selector))

-- | What is drawn on top over the first element a selector finds, at its
-- centre and near its corners: "" when that element is, else the first
-- point covered and the element on top there.
onTop :: Text -> PageM s Text
onTop selector = unsafeIOToPageM (fromJSText <$> js_on_top (toJSText selector))

-- | The edges of 'rectOf', as numbers: left, top, right and bottom.
edges :: Text -> PageM s [Double]
edges selector = mapMaybe (readMaybe . T.unpack) . T.words <$> rectOf selector

-- | The colour the style sheet gives the first element a selector finds.
computedColour :: Text -> PageM s Text
computedColour selector = unsafeIOToPageM (fromJSText <$> js_computed_colour (toJSText selector))

-- | The page at a path of the demo, as the backend has it now.
-- | Someone else (the demo's administrator) saves the post after the
-- editor has loaded it: the page as it was.
someoneElseSaves :: IORef MemState -> PageM s Page
someoneElseSaves wiki = do
  page <- readPage wiki ["blog"] ["hello-world"]
  unsafeIOToPageM $ do
    editing <- (\st -> st.current) <$> readIORef wiki
    modifyIORef' wiki (\st -> st {current = Just (UserId "user-admin")})
    runMemWiki wiki (savePage page.id page.updatedAt page.title "# Hello, world\n\nSomeone else's words.") >>= either (panic . show) (const pass)
    modifyIORef' wiki (\st -> st {current = editing})
  pure page

-- | The editor's draft of a page, if there is one.
draftOf :: IORef MemState -> Page -> PageM s (Maybe Draft)
draftOf wiki page = unsafeIOToPageM (runMemWiki wiki (either (panic . show) identity <$> getDraft page.id))

readPage :: IORef MemState -> [Text] -> [Text] -> PageM s Page
readPage wiki space path =
  unsafeIOToPageM . runMemWiki wiki $ do
    s <- either (panic . show) identity <$> getSpace (slugs space)
    either (panic . show) identity <$> getPage s.id (slugs path)

-- | A paste of a file, with a text beside it, on the element a selector
-- finds; whether its default was prevented.
pasteFile :: Text -> Text -> Text -> Text -> Text -> IO Bool
pasteFile selector text name mediaType base64 = evaluate =<< js_paste (toJSText selector) (toJSText text) (toJSText name) (toJSText mediaType) (toJSText base64)

-- | A paste of text alone; whether its default was prevented.
pasteText :: Text -> Text -> IO Bool
pasteText selector text = pasteFile selector text "" "" ""

dropFile, chooseFile :: Text -> Text -> Text -> Text -> IO ()
dropFile selector name mediaType base64 = evaluate =<< js_drop_file (toJSText selector) (toJSText name) (toJSText mediaType) (toJSText base64)
chooseFile selector name mediaType base64 = evaluate =<< js_choose_file (toJSText selector) (toJSText name) (toJSText mediaType) (toJSText base64)

-- | A PNG of one pixel, as base64.
dotPng :: Text
dotPng = "iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAYAAAAfFcSJAAAADUlEQVR42mNkYPhfDwAChwGA60e6kgAAAABJRU5ErkJggg=="

-- | Enter in a list and in a box (WP5.124 (a) of docs/PLAN.md, #187), one
-- case a line: its name, the text with the cursor at @‸@ or a selection
-- between @«@ and @»@, the keys pressed, and the text with the cursor
-- after them. The text goes under the page's title line ('titleLines').
-- Each case is one way the keymap goes: an answer that writes a new line's
-- start, one that replaces the cursor's line, none (code), Shift+Enter, a
-- selection, which is never asked about, and a character of two UTF-16
-- units before the cursor, with text after it, which the ask must split
-- where the cursor is: the text before the cursor and after it are given
-- apart, so that no offset in UTF-16 units reaches Haskell's characters.
enterCases :: [(Text, Text, [Text], Text)]
enterCases =
  [ ("an item", "- one‸", ["Enter"], "- one\n- ‸")
  , ("a nested item, then up a level and out", "- one\n  - two‸", ["Enter", "Enter", "Enter"], "- one\n  - two\n‸")
  , ("a box, left on its empty line", "> [!NOTE]\n> text‸", ["Enter", "Enter"], "> [!NOTE]\n> text\n‸")
  , ("a numbered item with a box, which the parser reads as text", "1. [x] done‸", ["Enter"], "1. [x] done\n2. ‸")
  , ("Shift+Enter in a list", "- one‸", ["Shift+Enter"], "- one\n‸")
  , ("a selection, replaced by a line break", "- «one»", ["Enter"], "- \n‸")
  , ("code", "```\n- a‸\n```", ["Enter"], "```\n- a\n‸\n```")
  , ("an emoji before the cursor, a line after it", "> 🎉 one‸\n> two", ["Enter"], "> 🎉 one\n> ‸\n> two")
  ]

-- | What 'enterCases' write under: the page's title line, which the editor
-- keeps, and a blank line.
titleLines :: Text
titleLines = "# Hello, world\n\n"

-- | A text with its selection's marks taken out (@«@ and @»@, or @‸@ for a
-- cursor), and where the selection starts and ends, in characters.
unmarked :: Text -> (Text, Int, Int)
unmarked t = case T.breakOn "‸" t of
  (before, after)
    | not (T.null after) -> (before <> T.drop 1 after, T.length before, T.length before)
    | otherwise ->
        let (start, rest) = T.breakOn "«" t
            (inside, end) = T.breakOn "»" (T.drop 1 rest)
         in (start <> inside <> T.drop 1 end, T.length start, T.length start + T.length inside)

-- | A text with the cursor's mark, @‸@, put in at a position in UTF-16
-- units.
marked :: Text -> Int -> Text
marked t at =
  -- The characters that end at or before the position.
  let n = length (takeWhile (<= at) (drop 1 (scanl (+) 0 (map (utf16Length . T.singleton) (toS t :: [Char])))))
   in T.take n t <> "‸" <> T.drop n t

-- | A length in CSS pixels, as computed styles give it ("14.4px").
readPx :: Text -> Double
readPx = fromMaybe 0 . readMaybe . T.unpack . T.dropEnd 2
