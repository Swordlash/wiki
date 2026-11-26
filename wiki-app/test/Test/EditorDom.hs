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

import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Primitive (PrimMonad)
import Control.Monad.Trans.Class (MonadTrans (..))
import Control.Monad.UUID (MonadUUID (..))
import Data.Aeson (object, (.=))
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
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
import Wiki.App.Backend.Notifications (MonadNotifications (..))
import Wiki.App.Islands (MonadIslands)
import Wiki.App.View.Common (Context (..))
import Wiki.App.Backend.Mem.State (MemState (..))
import Wiki.App.Demo (demoNow, demoState)
import Wiki.App.Route (Route (..), route)
import Wiki.App.View.Editor qualified as Editor
import Wiki.Core.Id (PageId, UserId (..))
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

  -- #394's first round: the server's yes to whether the page's text is
  -- plain now was for the reader signed in when it was asked. Another tab
  -- signs in as someone else while it is out: the letters typed after an
  -- @ are not sent under the new reader. (Shown failing with the editor
  -- sending the letters on the yes without reading the session again.)
  it "sends no mention's letters when the reader changes while the question whether the page is plain is out" $
    withTestClock $ \clock -> withAsked $ \asked run input -> withView run Editor.component input $ \view -> do
      settle view
      host <- find view ".editor-cm" >>= maybe (panic "the editor has no element for CodeMirror") pure
      let mention n = N.dispatch "wiki-mention" (object ["detail" .= object ["id" .= (n :: Int), "letters" .= ("an" :: Text)]]) host >> advance clock 0.001
          yes = waitHeld asked.checks ((== 1) . length) >>= traverse_ (\h -> answer asked.checks h (Right ()))
      mention 1
      checking <- waitHeld asked.checks ((== 1) . length)
      setInput view (nextReader input)
      advance clock 0.001
      traverse_ (\h -> answer asked.checks h (Right ())) checking
      settleHolding view (holds asked.checks)
      readIORef asked.candidates `shouldReturn` []
      -- The same reader throughout: the letters go out on the yes.
      mention 2
      yes
      settleHolding view (holds asked.checks)
      readIORef asked.candidates `shouldReturn` ["an"]

  -- The same for the logins the preview beside the text looks up as they
  -- are typed (Wiki.App.View.Preview's resolvePeople). (Shown failing with
  -- the preview asking on the yes without reading the session again.)
  it "looks up no logins the preview's text mentions when the reader changes while the question whether the page is plain is out" $
    withTestClock $ \clock -> withAsked $ \asked run input -> withView run Editor.component input $ \view -> do
      settle view
      click view ".editor-side-by-side"
      settle view
      field <- find view ".editor-text" >>= maybe (panic "the editor has no textarea") pure
      original <- valueOf field
      typeIntoNode field (original <> "\n\nAsk @ada.")
      advance clock Editor.previewDelay
      checking <- waitHeld asked.checks ((== 1) . length)
      -- The new reader's preview asks again, for itself.
      setInput view (nextReader input)
      again <- waitHeld asked.checks ((== 2) . length)
      traverse_ (\h -> answer asked.checks h (Right ())) checking
      settleHolding view (holds asked.checks)
      readIORef asked.logins `shouldReturn` []
      -- The yes to the question asked under the reader signed in now: the
      -- logins are looked up.
      traverse_ (\h -> answer asked.checks h (Right ())) [h | h <- again, h.number `notElem` map (\c -> c.number) checking]
      settleHolding view (holds asked.checks)
      readIORef asked.logins `shouldReturn` [["ada"]]
  where
    slugs :: [Text] -> [Slug]
    slugs = mapMaybe (rightToMaybe . mkSlug)
    -- The field's text, as an input event's handler reads it: its value,
    -- or the textarea's content until a value is set.
    valueOf node =
      property "value" node >>= \case
        Just (N.ScalarText t) -> pure t
        _ -> N.textContent node

-- | What 'Asked' holds: the questions whether a page's text is plain now,
-- in a gate, until the test answers each; and what was sent after them,
-- the letters of each lookup of people for a mention and the logins of
-- each lookup of the people a text mentions, the latest first.
data Asks = Asks
  { checks :: Gate PageId (Either WikiError ())
  , candidates :: IORef [Text]
  , logins :: IORef [[Text]]
  }

-- | The in-memory backend with the questions whether a page's text is
-- plain now held in a gate, and the lookups of people kept.
newtype Asked m a = Asked (ReaderT Asks m a)
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadUnliftIO, PrimMonad)

instance MonadTrans Asked where
  lift = Asked . lift

deriving newtype instance (MonadDOM m) => MonadDOM (Asked m)

deriving newtype instance (MonadAttributes m) => MonadAttributes (Asked m)

deriving newtype instance (MonadBrowserDOM m) => MonadBrowserDOM (Asked m)

instance (MonadUUID m) => MonadUUID (Asked m) where
  generateV4 = lift generateV4

instance (MonadWiki m, MonadIO m) => MonadWiki (Asked m) where
  plainNow p = Asked (ask >>= \a -> liftIO (hold a.checks p))

instance (MonadWiki m, MonadIO m) => MonadLoad (Asked m) where
  atOnce = fmap Just
  attempt = fmap Right

instance (MonadAdmin m) => MonadAdmin (Asked m)

instance (MonadNotifications m, MonadIO m) => MonadNotifications (Asked m) where
  mentionCandidates letters = Asked (ask >>= \a -> liftIO (atomicModifyIORef' a.candidates (\ls -> (letters : ls, ())))) >> lift (mentionCandidates letters)
  mentionable ls = Asked (ask >>= \a -> liftIO (atomicModifyIORef' a.logins (\xs -> (ls : xs, ())))) >> lift (mentionable ls)

instance (MonadVault m) => MonadVault (Asked m)

instance (MonadAssistant m) => MonadAssistant (Asked m)

instance (MonadIslands m) => MonadIslands (Asked m)

-- | The editor at the blog's first post, signed in as the demo's editor,
-- over 'Asked': the body gets what was asked, the backend to run the
-- view with, and the editor's input.
withAsked :: (Asks -> (forall a. Asked (MemWiki MemDOM) a -> MemDOM a) -> Editor.Input -> IO r) -> IO r
withAsked body = do
  wiki <- newMemWiki ((demoState demoNow) {current = Just (UserId "user-editor")})
  asked <- Asks <$> newGate <*> newIORef [] <*> newIORef []
  let run :: Asked (MemWiki MemDOM) a -> MemDOM a
      run (Asked m) = runMemWiki wiki (runReaderT m asked)
      orPanic = either (panic . show) identity
  router <- runMemDOM (runMemWiki wiki (Router.newRouter "/" Unknown route))
  (me, spaces, blog, ps) <- runMemDOM . runMemWiki wiki $ do
    me <- session
    spaces <- listSpaces
    blog <- orPanic <$> getSpace (mapMaybe (rightToMaybe . mkSlug) ["blog"])
    ps <- orPanic <$> listPages blog.id
    pure (me, spaces, blog, ps)
  let input = Editor.Input {context = viewContextAs router me spaces 0, space = blog, pages = ps, path = mapMaybe (rightToMaybe . mkSlug) ["hello-world"], feed = Nothing, home = False}
  outcome <- body asked run input
  Router.stop router
  pure outcome

-- | The editor's input after a sign-in in another tab: the session's
-- number moved on, as the shell counts it.
nextReader :: Editor.Input -> Editor.Input
nextReader input = input {Editor.context = input.context {sessionNumber = input.context.sessionNumber + 1}}
