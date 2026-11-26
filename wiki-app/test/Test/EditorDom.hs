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
-- The others show the lookup of other spaces' pages after @[[@ (#301) run
-- by the component, its checks of the space and its lookups held in
-- gates: a question asked while a lookup is out sends none, and is looked
-- up once that lookup answers; and a question given up while its check is
-- out sends no lookup when the check says yes. The rules of the replies
-- are "Test.Elsewhere"'s.
module Test.EditorDom (spec) where

import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Primitive (PrimMonad)
import Control.Monad.Trans.Class (MonadTrans (..))
import Control.Monad.UUID (MonadUUID (..))
import Data.Aeson (ToJSON, encode, object, (.=))
import Data.IORef (atomicModifyIORef', newIORef, readIORef)
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Halogen.Router qualified as Router
import Halogen.VDom.DOM.Monad (MemDOM, runMemDOM)
import Halogen.VDom.DOM.Monad.Class (MonadAttributes, MonadBrowserDOM, MonadDOM)
import Halogen.VDom.DOM.Monad.Native qualified as N
import Protolude hiding (find)
import Servant.Client.Core (Response)
import Test.Harness.MemDom
import Test.Hspec
import Test.Scripted (jsonResponse, methodOf)
import Wiki.Api (PagesApi (..), SpacesApi (..), WikiApi (..))
import Wiki.Api.Error qualified as ApiError
import Wiki.App.Assistant (MonadAssistant)
import Wiki.App.Assistant.Sync (MonadVault)
import Wiki.App.Backend (MonadLoad (..), MonadWiki (..), WikiError)
import Wiki.App.Backend.Admin (MonadAdmin)
import Wiki.App.Backend.Http (HttpWiki, Transport (..), newHttpEnvWith, requestUrl, runHttpWiki)
import Wiki.App.Backend.Http.Cache (emptyCache, withSession, withSpaces)
import Wiki.App.Backend.Http.Client (endpointUrl)
import Wiki.App.Backend.Mem (MemWiki, newMemWiki, runMemWiki)
import Wiki.App.Backend.Mem.State (MemState (..))
import Wiki.App.Backend.Notifications (MonadNotifications)
import Wiki.App.Demo (demoNow, demoState)
import Wiki.App.Islands (MonadIslands)
import Wiki.App.Route (Route (..), route)
import Wiki.App.View.Editor qualified as Editor
import Wiki.Core.Draft (Draft)
import Wiki.Core.Id (PageId, UserId (..))
import Wiki.Core.Page (Page (..))
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
      -- The program run until nothing runs or is queued, its pauses (the
      -- preview's, the check's, the draft's) left asleep on the clock: an
      -- advance smaller than any of them.
      let idle = advance clock 0.001
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
      gates <- Gates <$> newGate <*> newGate
      let run :: Looked (MemWiki MemDOM) a -> MemDOM a
          run = runMemWiki wiki . runLooked gates
      (router, input) <- editorInput (runMemWiki wiki)
      withView run Editor.component input $ \view -> do
        ask' <- questions view clock
        let both = holds gates.checks <> holds gates.lookups
            -- The check that the page's text is plain now, which the
            -- backend answers yes.
            plainNow' = waitHeld gates.checks ((== 1) . length) >>= traverse_ (\h -> answer gates.checks h (Right ()))
        -- The first question: its check goes out once the pause ends, and
        -- on its yes the lookup.
        ask' (1 :: Int) "on"
        advance clock Editor.elsewhereDelay
        plainNow'
        first' <- waitHeld gates.lookups ((== ["on"]) . lettersHeld)
        -- A second question, while the first lookup is out: its pause
        -- ends, and nothing more goes out, neither its check nor its
        -- lookup. (The count of checks held fails when the second
        -- question's check goes out while the first lookup is out: shown
        -- so with the machine's rule of one ask out taken away.)
        ask' 2 "onb"
        advance clock Editor.elsewhereDelay
        settleHolding view both
        lettersHeld <$> held gates.lookups `shouldReturn` ["on"]
        length <$> held gates.checks `shouldReturn` 0
        -- The first lookup answers: its answer is no reply to the second
        -- question, which is checked and looked up now.
        traverse_ (\h -> answer gates.lookups h (Right [])) first'
        plainNow'
        later <- waitHeld gates.lookups ((== ["onb"]) . lettersHeld)
        map (\h -> h.asked.except) later `shouldBe` [Just input.space.id]
        traverse_ (\h -> answer gates.lookups h (Right [])) later
        settleHolding view both
        held gates.lookups >>= (`shouldBe` []) . lettersHeld
      Router.stop router

  -- Round 4 of #389's review: Escape, or letters typed over, while the
  -- check of the space is out. The check's yes comes after the question
  -- is gone, and no lookup carries its letters. (Shown failing with the
  -- machine keeping a question given up once its check is out.)
  it "sends no lookup when the question is given up while the check of the space is out" $
    withTestClock $ \clock -> do
      wiki <- newMemWiki ((demoState demoNow) {current = Just (UserId "user-editor")})
      gates <- Gates <$> newGate <*> newGate
      let run :: Looked (MemWiki MemDOM) a -> MemDOM a
          run = runMemWiki wiki . runLooked gates
      (router, input) <- editorInput (runMemWiki wiki)
      withView run Editor.component input $ \view -> do
        ask' <- questions view clock
        host <- find view ".editor-cm" >>= maybe (panic "the editor has no element for CodeMirror") pure
        ask' (1 :: Int) "onb"
        advance clock Editor.elsewhereDelay
        checking <- waitHeld gates.checks ((== 1) . length)
        N.dispatch "wiki-complete-elsewhere-abandoned" (object ["detail" .= object ["id" .= (1 :: Int)]]) host
        advance clock 0.001
        traverse_ (\h -> answer gates.checks h (Right ())) checking
        settleHolding view (holds gates.checks <> holds gates.lookups)
        lettersHeld <$> held gates.lookups `shouldReturn` []
      Router.stop router

  -- #389 over the HTTP backend: the server says no to the page's text
  -- being plain now ('plainNow', a 409 for an encrypted space). The
  -- letters typed are not sent: no lookup of pages by title is asked.
  -- (Shown failing with the check skipped, its answer taken as yes.)
  it "sends no letters typed when the server says the page's text is not plain now" $
    withTestClock $ \clock -> do
      wiki <- newMemWiki ((demoState demoNow) {current = Just (UserId "user-editor")})
      let orPanic = either (panic . show) identity
      router <- runMemDOM (runMemWiki wiki (Router.newRouter "/" Unknown route))
      (me, spaces, blog, ps, hello) <- runMemDOM . runMemWiki wiki $ do
        me <- session
        spaces <- listSpaces
        blog <- orPanic <$> getSpace (slugs ["blog"])
        ps <- orPanic <$> listPages blog.id
        hello <- orPanic <$> getPage blog.id (slugs ["hello-world"])
        pure (me, spaces, blog, ps, hello)
      sent <- newIORef []
      let json :: (ToJSON a) => Int -> a -> Either Text Response
          json n a = Right (jsonResponse n (encode a))
          serve r
            | methodOf r /= "GET" = Right (jsonResponse 500 "")
            | requestUrl r == endpointUrl (\e -> e.spaces.pageAt blog.id (slugs ["hello-world"])) = json 200 hello
            | requestUrl r == endpointUrl (\e -> e.pages.plain hello.id) = json 409 (ApiError.Stale ApiError.SpaceIsEncrypted "A draft kept on the server is not possible in an encrypted space.")
            | requestUrl r == endpointUrl (\e -> e.pages.draft hello.id) = json 200 (Nothing :: Maybe Draft)
            | requestUrl r == "api/v1/spaces" = json 200 [blog]
            | "api/v1/pages/titled" `T.isPrefixOf` requestUrl r = json 200 ([] :: [TitleHit])
            | otherwise = json 404 ApiError.NotFound
      env <- newHttpEnvWith (Transport (\r -> atomicModifyIORef' sent (\rs -> (r : rs, ())) $> serve r)) (withSpaces spaces (withSession me emptyCache))
      let run :: HttpWiki MemDOM a -> MemDOM a
          run = runHttpWiki env
          input = Editor.Input {context = viewContextAs router me spaces 0, space = blog, pages = ps, path = slugs ["hello-world"], feed = Nothing, home = False}
          asked prefix = (\rs -> [requestUrl r | r <- rs, prefix `T.isPrefixOf` requestUrl r]) <$> readIORef sent
      withView run Editor.component input $ \view -> do
        ask' <- questions view clock
        ask' (1 :: Int) "onb"
        advance clock Editor.elsewhereDelay
        settle view
        asked "api/v1/pages/titled" `shouldReturn` []
        -- The no came from the server.
        asked (endpointUrl (\e -> e.pages.plain hello.id)) >>= (`shouldSatisfy` (not . null))
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

  -- #394's third round: the guard of a plain write says no (the page's
  -- space was encrypted since the editor opened it), over the HTTP
  -- backend. That is no conflict of versions: the editor says why nothing
  -- was sent, and offers no merge with the page as it is cached. (Shown
  -- failing with the guard's refusal given as a 'Conflict'.)
  it "shows why nothing was sent when the guard refuses a save, and offers no merge" $
    withTestClock $ \clock -> do
      wiki <- newMemWiki ((demoState demoNow) {current = Just (UserId "user-editor")})
      let orPanic = either (panic . show) identity
      router <- runMemDOM (runMemWiki wiki (Router.newRouter "/" Unknown route))
      (me, spaces, blog, ps, hello) <- runMemDOM . runMemWiki wiki $ do
        me <- session
        spaces <- listSpaces
        blog <- orPanic <$> getSpace (slugs ["blog"])
        ps <- orPanic <$> listPages blog.id
        hello <- orPanic <$> getPage blog.id (slugs ["hello-world"])
        pure (me, spaces, blog, ps, hello)
      sent <- newIORef []
      let json :: (ToJSON a) => Int -> a -> Either Text Response
          json n a = Right (jsonResponse n (encode a))
          serve r
            | methodOf r /= "GET" = Right (jsonResponse 500 "")
            | requestUrl r == endpointUrl (\e -> e.spaces.pageAt blog.id (slugs ["hello-world"])) = json 200 hello
            | requestUrl r == endpointUrl (\e -> e.pages.plain hello.id) = json 409 (ApiError.Stale ApiError.SpaceIsEncrypted "A draft kept on the server is not possible in an encrypted space.")
            | requestUrl r == endpointUrl (\e -> e.pages.draft hello.id) = json 200 (Nothing :: Maybe Draft)
            | requestUrl r == "api/v1/spaces" = json 200 [blog]
            | otherwise = json 404 ApiError.NotFound
      env <- newHttpEnvWith (Transport (\r -> atomicModifyIORef' sent (\rs -> (r : rs, ())) $> serve r)) (withSpaces spaces (withSession me emptyCache))
      let run :: HttpWiki MemDOM a -> MemDOM a
          run = runHttpWiki env
          input = Editor.Input {context = viewContextAs router me spaces 0, space = blog, pages = ps, path = slugs ["hello-world"], feed = Nothing, home = False}
      withView run Editor.component input $ \view -> do
        settle view
        field <- find view ".editor-text" >>= maybe (panic "the editor has no textarea") pure
        typeIntoNode field "# Hello, world\n\nMy words."
        advance clock 0.001
        click view ".editor-save"
        settle view
        length <$> findAll view ".editor-conflict" `shouldReturn` 0
        textOf view ".error-conflict" >>= (`shouldSatisfy` T.isInfixOf "nothing was sent")
        valueOf field `shouldReturn` "# Hello, world\n\nMy words."
        -- Nothing was written: no save of the page.
        readIORef sent >>= \rs -> [requestUrl r | r <- rs, methodOf r == "PUT"] `shouldBe` []
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
    lettersHeld = map (\h -> h.asked.letters)
    -- Once the editor is loaded: ask CodeMirror's question about the pages
    -- of other spaces, with this number, after "See [[" and these letters,
    -- typed first in the textarea.
    questions view clock = do
      settle view
      field <- find view ".editor-text" >>= maybe (panic "the editor has no textarea") pure
      host <- find view ".editor-cm" >>= maybe (panic "the editor has no element for CodeMirror") pure
      let idle = advance clock 0.001
      pure $ \n typed -> do
        typeIntoNode field ("# Hello, world\n\nSee [[" <> typed)
        idle
        N.dispatch "wiki-complete-elsewhere" (object ["detail" .= object ["id" .= n, "line" .= (3 :: Int), "before" .= ("See [[" <> typed), "after" .= ("" :: Text), "nodes" .= ("Paragraph Document" :: Text)]]) host
        idle
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

-- | The gates of 'Looked': the checks that a page's text is plain now
-- ('plainNow') and the lookups of pages by title.
data Gates = Gates
  { checks :: Gate PageId (Either WikiError ())
  , lookups :: Gate TitleQuery (Either WikiError [TitleHit])
  }

-- | The in-memory backend with the checks of a page's space and the
-- lookups of pages by title held in gates, until the test answers each.
newtype Looked m a = Looked (ReaderT Gates m a)
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadUnliftIO, PrimMonad)

instance MonadTrans Looked where
  lift = Looked . lift

runLooked :: Gates -> Looked m a -> m a
runLooked gates (Looked m) = runReaderT m gates

deriving newtype instance (MonadDOM m) => MonadDOM (Looked m)

deriving newtype instance (MonadAttributes m) => MonadAttributes (Looked m)

deriving newtype instance (MonadBrowserDOM m) => MonadBrowserDOM (Looked m)

instance (MonadUUID m) => MonadUUID (Looked m) where
  generateV4 = lift generateV4

instance (MonadWiki m, MonadIO m) => MonadWiki (Looked m) where
  findPagesByTitle q = Looked (ask >>= \gates -> liftIO (hold gates.lookups q))
  plainNow p = Looked (ask >>= \gates -> liftIO (hold gates.checks p))

instance (MonadWiki m, MonadIO m) => MonadLoad (Looked m) where
  atOnce = fmap Just
  attempt = fmap Right

instance (MonadAdmin m) => MonadAdmin (Looked m)

instance (MonadNotifications m) => MonadNotifications (Looked m)

instance (MonadVault m) => MonadVault (Looked m)

instance (MonadAssistant m) => MonadAssistant (Looked m)

instance (MonadIslands m) => MonadIslands (Looked m)
