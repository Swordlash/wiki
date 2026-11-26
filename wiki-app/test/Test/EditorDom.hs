-- | The editor ("Wiki.App.View.Editor"), the real component, on the
-- in-memory DOM ("Test.Harness.MemDom") over the demo's wiki, at the blog's
-- post "Hello, world" for its editor, on a clock the test owns. There is no
-- CodeMirror natively: the text is the textarea the editor renders before
-- CodeMirror takes its place, and CodeMirror's questions are events the
-- test dispatches on the element CodeMirror would be in.
--
-- The first spec shows the fault of e9055578, put in the order that loses
-- a key, which a browser leaves to chance: the field's text changed by a
-- key, the preview's turn re-rendering the editor before the key's input
-- is handled, then the input. When the textarea's text was a value Halogen
-- rendered from the editor's state, that render wrote the text back as it
-- was before the key, and the input then read it without the key. The
-- editor renders the textarea's text only as its content, the text it was
-- loaded with, so no render writes the field while it is typed in.
--
-- The second shows the lookup of other spaces' pages after @[[@ (#301)
-- run by the component, its lookups held in a gate: a question asked
-- while a lookup is out sends none, and is looked up once that lookup
-- answers. The rules of the replies are "Test.Elsewhere"'s.
module Test.EditorDom (spec) where

import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Primitive (PrimMonad)
import Control.Monad.Trans.Class (MonadTrans (..))
import Control.Monad.UUID (MonadUUID (..))
import Data.Aeson (object, (.=))
import Data.IORef (atomicModifyIORef')
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Halogen.Router qualified as Router
import Halogen.VDom.DOM.Monad (MemDOM, runMemDOM)
import Halogen.VDom.DOM.Monad.Class (MonadAttributes, MonadBrowserDOM, MonadDOM)
import Halogen.VDom.DOM.Monad.Native qualified as N
import Protolude hiding (find)
import Test.Harness.MemDom
import Test.Hspec
import Wiki.App.Assistant (MonadAssistant)
import Wiki.App.Assistant.Sync (MonadVault)
import Wiki.App.Backend (MonadLoad (..), MonadWiki (..), WikiError)
import Wiki.App.Backend.Admin (MonadAdmin)
import Wiki.App.Backend.Mem (MemWiki, newMemWiki, runMemWiki)
import Wiki.App.Backend.Mem.State (MemState (..))
import Wiki.App.Backend.Notifications (MonadNotifications)
import Wiki.App.Demo (demoNow, demoState)
import Wiki.App.Islands (MonadIslands)
import Wiki.App.Route (Route (..), route)
import Wiki.App.View.Editor qualified as Editor
import Wiki.Core.Id (UserId (..))
import Wiki.Core.PageMatch (TitleHit, TitleQuery (..))
import Wiki.Core.Slug (Slug, mkSlug)
import Wiki.Core.Space (Space (id))

spec :: Spec
spec = describe "Wiki.App.View.Editor, mounted on the in-memory DOM" $ do
  it "keeps a key whose input is handled after the preview's turn re-rendered the editor" $
    withTestClock $ \clock -> do
      wiki <- newMemWiki ((demoState demoNow) {current = Just (UserId "user-editor")})
      let run :: MemWiki MemDOM a -> MemDOM a
          run = runMemWiki wiki
      (router, input) <- editorInput run
      let
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

  it "looks up other spaces' pages one at a time: a question asked while a lookup is out is looked up once it answers" $
    withTestClock $ \clock -> do
      wiki <- newMemWiki ((demoState demoNow) {current = Just (UserId "user-editor")})
      gate <- newGate
      let run :: Looked (MemWiki MemDOM) a -> MemDOM a
          run = runMemWiki wiki . runLooked gate
          idle = advance clock 0.001
          lettersHeld = map (\h -> h.asked.letters)
      (router, input) <- editorInput (runMemWiki wiki)
      withView run Editor.component input $ \view -> do
        settle view
        field <- find view ".editor-text" >>= maybe (panic "the editor has no textarea") pure
        host <- find view ".editor-cm" >>= maybe (panic "the editor has no element for CodeMirror") pure
        let question n typed = do
              typeIntoNode field ("# Hello, world\n\nSee [[" <> typed)
              idle
              N.dispatch "wiki-complete-elsewhere" (object ["detail" .= object ["id" .= n, "line" .= (3 :: Int), "before" .= ("See [[" <> typed), "after" .= ("" :: Text), "nodes" .= ("Paragraph Document" :: Text)]]) host
              idle
        -- The first question: its lookup goes out once the pause ends.
        question (1 :: Int) "on"
        advance clock Editor.elsewhereDelay
        first' <- waitHeld gate ((== ["on"]) . lettersHeld)
        -- A second question, while the first lookup is out: its pause
        -- ends, and nothing more goes out. (The check of the number held
        -- fails when a lookup goes out for each question: shown so with
        -- the machine's rule taken away.)
        question 2 "onb"
        advance clock Editor.elsewhereDelay
        settleHolding view (holds gate)
        lettersHeld <$> held gate `shouldReturn` ["on"]
        -- The first lookup answers: its answer is no reply to the second
        -- question, which is looked up now.
        traverse_ (\h -> answer gate h (Right [])) first'
        later <- waitHeld gate ((== ["onb"]) . lettersHeld)
        map (\h -> h.asked.except) later `shouldBe` [Just input.space.id]
        traverse_ (\h -> answer gate h (Right [])) later
        settleHolding view (holds gate)
        held gate >>= (`shouldBe` []) . lettersHeld
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
    -- The editor's input at the blog's first post, signed in as the
    -- demo's editor, and the router it is given.
    editorInput :: (forall a. MemWiki MemDOM a -> MemDOM a) -> IO (Router.Router Route, Editor.Input)
    editorInput run = do
      let orPanic = either (panic . show) identity
      router <- runMemDOM (run (Router.newRouter "/" Unknown route))
      (me, spaces, blog, ps) <- runMemDOM . run $ do
        me <- session
        spaces <- listSpaces
        blog <- orPanic <$> getSpace (slugs ["blog"])
        ps <- orPanic <$> listPages blog.id
        pure (me, spaces, blog, ps)
      pure (router, Editor.Input {context = viewContextAs router me spaces 0, space = blog, pages = ps, path = slugs ["hello-world"], feed = Nothing, home = False})

-- | The in-memory backend with the lookups of pages by title held in a
-- gate, until the test answers each.
newtype Looked m a = Looked (ReaderT (Gate TitleQuery (Either WikiError [TitleHit])) m a)
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadUnliftIO, PrimMonad)

instance MonadTrans Looked where
  lift = Looked . lift

runLooked :: Gate TitleQuery (Either WikiError [TitleHit]) -> Looked m a -> m a
runLooked gate (Looked m) = runReaderT m gate

deriving newtype instance (MonadDOM m) => MonadDOM (Looked m)

deriving newtype instance (MonadAttributes m) => MonadAttributes (Looked m)

deriving newtype instance (MonadBrowserDOM m) => MonadBrowserDOM (Looked m)

instance (MonadUUID m) => MonadUUID (Looked m) where
  generateV4 = lift generateV4

instance (MonadWiki m, MonadIO m) => MonadWiki (Looked m) where
  findPagesByTitle q = Looked (ask >>= \gate -> liftIO (hold gate q))

instance (MonadWiki m, MonadIO m) => MonadLoad (Looked m) where
  atOnce = fmap Just
  attempt = fmap Right

instance (MonadAdmin m) => MonadAdmin (Looked m)

instance (MonadNotifications m) => MonadNotifications (Looked m)

instance (MonadVault m) => MonadVault (Looked m)

instance (MonadAssistant m) => MonadAssistant (Looked m)

instance (MonadIslands m) => MonadIslands (Looked m)
