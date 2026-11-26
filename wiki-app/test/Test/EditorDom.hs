-- | The editor ("Wiki.App.View.Editor"), the real component, on the
-- in-memory DOM ("Test.Harness.MemDom") over the demo's wiki, at the blog's
-- post "Hello, world" for its editor, on a clock the test owns. There is no
-- CodeMirror natively: the text is the textarea the editor renders before
-- CodeMirror takes its place.
--
-- What it shows is the fault of e9055578, put in the order that loses a
-- key, which a browser leaves to chance: the field's text changed by a key,
-- the preview's turn re-rendering the editor before the key's input is
-- handled, then the input. When the textarea's text was a value Halogen
-- rendered from the editor's state, that render wrote the text back as it
-- was before the key, and the input then read it without the key. The
-- editor renders the textarea's text only as its content, the text it was
-- loaded with, so no render writes the field while it is typed in.
module Test.EditorDom (spec) where

import Data.Aeson (object, (.=))
import Data.IORef (atomicModifyIORef')
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Halogen.Router qualified as Router
import Halogen.VDom.DOM.Monad (MemDOM, runMemDOM)
import Halogen.VDom.DOM.Monad.Native qualified as N
import Protolude hiding (find)
import Test.Harness.MemDom
import Test.Hspec
import Wiki.App.Backend (MonadWiki (..))
import Wiki.App.Backend.Mem (MemWiki, newMemWiki, runMemWiki)
import Wiki.App.Backend.Mem.State (MemState (..))
import Wiki.App.Demo (demoNow, demoState)
import Wiki.App.Route (Route (..), route)
import Wiki.App.View.Editor qualified as Editor
import Wiki.Core.Id (UserId (..))
import Wiki.Core.Page (Page (..))
import Wiki.Core.Slug (Slug, mkSlug)
import Wiki.Core.Space (Space (id))

spec :: Spec
spec = describe "Wiki.App.View.Editor, mounted on the in-memory DOM" $ do
  it "keeps a key whose input is handled after the preview's turn re-rendered the editor" $
    withTestClock $ \clock -> do
      wiki <- newMemWiki ((demoState demoNow) {current = Just (UserId "user-editor")})
      let run :: MemWiki MemDOM a -> MemDOM a
          run = runMemWiki wiki
          orPanic = either (panic . show) identity
      router <- runMemDOM (run (Router.newRouter "/" Unknown route))
      (me, spaces, blog, ps) <- runMemDOM . run $ do
        me <- session
        spaces <- listSpaces
        blog <- orPanic <$> getSpace (slugs ["blog"])
        ps <- orPanic <$> listPages blog.id
        pure (me, spaces, blog, ps)
      let input = Editor.Input {context = viewContextAs router me spaces 0, space = blog, pages = ps, path = slugs ["hello-world"], feed = Nothing, home = False}
          -- The program run until nothing runs or is queued, its pauses
          -- (the preview's, the check's, the draft's) left asleep on the
          -- clock: an advance smaller than any of them.
          idle = advance clock 0.001
      withView run Editor.component input $ \view -> do
        -- The page loaded, nothing typed yet: no pause armed.
        settle view
        -- The preview beside the text, which follows the typing.
        click view ".editor-side-by-side"
        settle view
        field <- find view ".editor-text" >>= maybe (panic "the editor has no textarea") pure
        original <- valueOf field
        -- A key, its input handled: the preview's turn is armed.
        typeIntoNode field (original <> "Q")
        idle
        -- The next key changes the field, and its input is not handled
        -- yet when the preview's turn comes and re-renders the editor.
        atomicModifyIORef' field.props (\ps' -> (Map.insert "value" (N.ScalarText (original <> "QZ")) ps', ()))
        advance clock Editor.previewDelay
        valueOf field `shouldReturn` original <> "QZ"
        -- The input, handled after the render: the draft has the key.
        N.dispatch "input" (object ["inputType" .= ("insertText" :: Text), "data" .= ("Z" :: Text)]) field
        idle
        advance clock Editor.previewDelay
        valueOf field `shouldReturn` original <> "QZ"
        textOf view ".editor-preview" >>= (`shouldSatisfy` T.isInfixOf "QZ")
      Router.stop router

  -- #394's second round: a save refused as a conflict fetches the page at
  -- the editor's address to show beside the text. The page edited was
  -- moved meanwhile, and another page took its address: nothing of that
  -- other page is offered to merge, keep over or take, and the refusal is
  -- shown as it is, the text still in the editor. (Shown failing with the
  -- conflict view offered for whichever page is at the address.)
  it "offers no merge with another page that took the edited page's address, and shows the refusal" $
    withTestClock $ \clock -> do
      wiki <- newMemWiki ((demoState demoNow) {current = Just (UserId "user-editor")})
      let run :: MemWiki MemDOM a -> MemDOM a
          run = runMemWiki wiki
          orPanic = either (panic . show) identity
      router <- runMemDOM (run (Router.newRouter "/" Unknown route))
      (me, spaces, blog, ps, hello) <- runMemDOM . run $ do
        me <- session
        spaces <- listSpaces
        blog <- orPanic <$> getSpace (slugs ["blog"])
        ps <- orPanic <$> listPages blog.id
        hello <- orPanic <$> getPage blog.id (slugs ["hello-world"])
        pure (me, spaces, blog, ps, hello)
      let input = Editor.Input {context = viewContextAs router me spaces 0, space = blog, pages = ps, path = slugs ["hello-world"], feed = Nothing, home = False}
          idle = advance clock 0.001
      withView run Editor.component input $ \view -> do
        settle view
        field <- find view ".editor-text" >>= maybe (panic "the editor has no textarea") pure
        typeIntoNode field "# Hello, world\n\nMy words."
        idle
        -- Another tab saves the post, moves it under another page, and
        -- makes a new page at its old address.
        other <- runMemDOM . run $ do
          saved <- orPanic <$> savePage hello.id hello.updatedAt hello.title "# Hello, world\n\nSomeone else's words."
          folder <- orPanic <$> createPage blog.id Nothing "Folder" "Under it."
          void (orPanic <$> movePage saved.id blog.id (Just folder.id))
          orPanic <$> createPage blog.id Nothing "Hello, world" "Another page's words."
        orPanic <$> runMemDOM (run (getPage blog.id (slugs ["hello-world"]))) >>= \p -> p.id `shouldBe` other.id
        click view ".editor-save"
        settle view
        length <$> findAll view ".editor-conflict" `shouldReturn` 0
        isJust <$> find view ".error-conflict" `shouldReturn` True
        valueOf field `shouldReturn` "# Hello, world\n\nMy words."
        -- Nothing was written under the other page.
        (\p -> p.body) . orPanic <$> runMemDOM (run (getPage blog.id (slugs ["hello-world"]))) `shouldReturn` "Another page's words."
      Router.stop router
  where
    slugs :: [Text] -> [Slug]
    slugs = mapMaybe (rightToMaybe . mkSlug)
    -- The field's text, as an input event's handler reads it: its value,
    -- or the textarea's content until a value is set.
    valueOf node =
      property "value" node >>= \case
        Just (N.ScalarText t) -> pure t
        _ -> N.textContent node
