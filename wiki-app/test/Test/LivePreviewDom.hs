-- | The preview in a window of its own ("Wiki.App.View.LivePreview"), the
-- real component, on the in-memory DOM ("Test.Harness.MemDom"), over the
-- demo's wiki, at the blog's post "Hello, world". The editor of the page
-- is the test: the window is given a 'Listener' of the test's, which keeps
-- the handler the window listens with, and the test speaks through it as
-- the editor's CodeMirror speaks on the browser's channel.
--
-- What it shows is that the window lays the text out as the page view
-- does ("Wiki.App.View.Reading"): at the width chosen, with the page's
-- switch, which tells the shell what the page's tells it; and with the
-- "Contents" panel for the text shown, worked out again for each text the
-- editor sends, which goes with the text when the editor of a page of an
-- encrypted space goes.
module Test.LivePreviewDom (spec) where

import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Primitive (PrimMonad)
import Control.Monad.Trans.Class (MonadTrans (..))
import Control.Monad.UUID (MonadUUID (..))
import Data.Aeson (Value, encode, object, (.=))
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import Data.Text qualified as T
import Halogen.Router qualified as Router
import Halogen.VDom.DOM.Monad (MemDOM, runMemDOM)
import Halogen.VDom.DOM.Monad.Class (MonadAttributes, MonadBrowserDOM, MonadDOM)
import Halogen.VDom.DOM.Monad.Native qualified as N
import Protolude hiding (find)
import Test.Harness.MemDom
import Test.Hspec
import Wiki.App.Backend (MonadLoad (..), MonadWiki (..))
import Wiki.App.Backend.Mem (MemWiki, newMemWiki, runMemWiki)
import Wiki.App.Backend.Mem.State (MemState (..))
import Wiki.App.Backend.Notifications (MonadNotifications)
import Wiki.App.Demo (demoNow, demoState)
import Wiki.App.Islands (MonadIslands)
import Wiki.App.Route (Route (..), route)
import Wiki.App.View.Common (Context (..), PageWidth (..), ViewOutput (..))
import Wiki.App.View.LivePreview qualified as LivePreview
import Wiki.Core.Id (PageId (..), UserId (..))
import Wiki.Core.Page (Page (..))
import Wiki.Core.Slug (Slug, mkSlug)
import Wiki.Core.Space (Space (..))
import Wiki.Core.Vault (KeyId (..))

-- | The editor of the page, as the window hears it: the handler the window
-- listens with, while it listens.
newtype Editor = Editor (IORef (Maybe (Text -> IO ())))

-- | The window's 'LivePreview.Listener' over the test's editor.
listenerOf :: Editor -> LivePreview.Listener
listenerOf (Editor handler) =
  LivePreview.Listener
    { listen = \_ heard -> do
        atomicModifyIORef' handler (const (Just heard, ()))
        pure (atomicModifyIORef' handler (const (Nothing, ())))
    }

-- | A message of the editor, as JSON, to the window listening.
speak :: Editor -> Value -> IO ()
speak (Editor handler) message =
  readIORef handler >>= \case
    Nothing -> expectationFailure "the window does not listen for the editor"
    Just heard -> heard (decodeUtf8 (toS (encode message)))

-- | The editor sends a text, under a title.
typed :: Editor -> [Text] -> IO ()
typed editor ls = speak editor (object ["text" .= T.unlines ls, "title" .= ("Hello, world" :: Text)])

slugs :: [Text] -> [Slug]
slugs = mapMaybe (rightToMaybe . mkSlug)

data Rig = Rig
  { view :: View (Const Void) LivePreview.Input ViewOutput
  , editor :: Editor
  , heard :: IO [ViewOutput]
  -- ^ What the window told the shell, the oldest first.
  , widthIs :: PageWidth -> IO ()
  -- ^ The window given the width the shell keeps.
  , askedAbout :: IO [PageId]
  -- ^ The pages asked about, whether their text is plain now
  -- ('plainNow'), the latest first.
  , post :: PageId
  -- ^ The post at the window's address, as saved.
  }

-- | The in-memory backend, with the pages asked about whether their text
-- is plain now kept, the latest first.
newtype Recorded m a = Recorded (ReaderT (IORef [PageId]) m a)
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadUnliftIO, PrimMonad)

instance MonadTrans Recorded where
  lift = Recorded . lift

deriving newtype instance (MonadDOM m) => MonadDOM (Recorded m)

deriving newtype instance (MonadAttributes m) => MonadAttributes (Recorded m)

deriving newtype instance (MonadBrowserDOM m) => MonadBrowserDOM (Recorded m)

instance (MonadUUID m) => MonadUUID (Recorded m) where
  generateV4 = lift generateV4

instance (MonadWiki m, MonadIO m) => MonadWiki (Recorded m) where
  plainNow p = Recorded (ask >>= \asked -> liftIO (atomicModifyIORef' asked (\ps -> (p : ps, ())))) >> lift (plainNow p)

instance (MonadWiki m, MonadIO m) => MonadLoad (Recorded m) where
  atOnce = fmap Just
  attempt = fmap Right

instance (MonadNotifications m) => MonadNotifications (Recorded m)

instance (MonadIslands m) => MonadIslands (Recorded m)

-- | The window at the blog's post "Hello, world", for the demo's
-- administrator, at the compact width; the space marked encrypted when
-- asked, and the post saved first with the text given, if any. The body
-- runs with it, once it is quiet.
withWindow :: Bool -> Maybe [Text] -> (Rig -> IO a) -> IO a
withWindow encrypted saved body = do
  wiki <- newMemWiki ((demoState demoNow) {current = Just (UserId "user-admin")})
  asked <- newIORef []
  let run :: Recorded (MemWiki MemDOM) a -> MemDOM a
      run (Recorded m) = runMemWiki wiki (runReaderT m asked)
      orPanic = either (panic . show) identity
  router <- runMemDOM (run (Router.newRouter "/" Unknown route))
  (me, spaces, blog, ps, post) <- runMemDOM . run $ do
    me <- session
    spaces <- listSpaces
    blog <- orPanic <$> getSpace (slugs ["blog"])
    ps <- orPanic <$> listPages blog.id
    hello <- orPanic <$> getPage blog.id (slugs ["hello-world"])
    for_ saved $ \ls -> void (orPanic <$> savePage hello.id hello.updatedAt hello.title (T.unlines ls))
    pure (me, spaces, blog, ps, hello.id)
  editor <- Editor <$> newIORef Nothing
  let space = if encrypted then blog {encryption = Just (KeyId "key-1")} else blog
      inputAt width = LivePreview.Input {context = (viewContextAs router me spaces 0) {pageWidth = width}, space, pages = ps, path = slugs ["hello-world"], listener = listenerOf editor}
  outcome <- withView run LivePreview.component (inputAt Compact) $ \view -> do
    settle view
    body Rig {view, editor, heard = outputs view, widthIs = \w -> setInput view (inputAt w) >> settle view, askedAbout = readIORef asked, post}
  Router.stop router
  pure outcome

-- | Whether the view has an element the selector finds.
present :: View q i o -> Text -> IO Bool
present view selector = isJust <$> find view selector

-- | The texts of the entries of the "Contents" panel, in order.
panelEntries :: View q i o -> IO [Text]
panelEntries view = findAll view ".page-side > details.page-contents nav .toc-list a" >>= traverse N.textContent

-- | Whether every entry of the lists of headings given links to an element
-- of the text, by its id.
linksResolve :: View q i o -> Text -> IO Bool
linksResolve view selector = do
  hrefs <- findAll view selector >>= traverse (N.attribute "href")
  ids <- findAll view ".page-body [id]" >>= traverse (N.attribute "id")
  pure (not (null hrefs) && all (\h -> maybe False (\x -> Just x `elem` ids) (T.stripPrefix "#" =<< h)) hrefs)

spec :: Spec
spec = describe "Wiki.App.View.LivePreview, mounted on the in-memory DOM" $ do
  it "lays the text out at the width chosen, with the page's switch, and tells the shell of a choice" $
    withWindow False Nothing $ \rig -> do
      present rig.view ".live-preview.page-view.page-width-compact" `shouldReturn` True
      present rig.view ".page-view.page-width-full" `shouldReturn` False
      textOf rig.view ".live-preview-bar .page-width-switch [aria-checked=true]" >>= (`shouldSatisfy` (T.isInfixOf "Compact"))
      -- The text sits in the page's column; the post as saved starts with
      -- its own title, so there is no row of the title.
      textOf rig.view ".page-view > .page-main > .page-text.live-preview-body .page-body h1" `shouldReturn` "Hello, world"
      present rig.view ".page-title-row" `shouldReturn` False
      click rig.view ".page-width-option-full"
      settle rig.view
      rig.heard >>= \os -> [w | ChoseWidth w <- os] `shouldBe` [FullWidth]
      -- The shell keeps the choice and gives it back.
      rig.widthIs FullWidth
      present rig.view ".live-preview.page-view.page-width-full" `shouldReturn` True
      textOf rig.view ".live-preview-bar .page-width-switch [aria-checked=true]" >>= (`shouldSatisfy` (T.isInfixOf "Full width"))
      -- A text with no title of its own: the title the editor sends, in
      -- the page's row above the text, once.
      typed rig.editor ["Some text."]
      settle rig.view
      textOf rig.view ".page-view > .page-title-row > .page-title" `shouldReturn` "Hello, world"
      length <$> findAll rig.view ".page-title" `shouldReturn` 1

  -- #394's first round: the editor's page moved to another space, and
  -- another page took its address. The text the editor sends is still of
  -- its own page, which it names: the people the text mentions are looked
  -- up only once the server says that page's text is plain now, never on
  -- the word of the page at the address. A text sent without a page's id
  -- is of no page: nobody is looked up. (Shown failing with the window
  -- asking about the page it read at the address.)
  it "asks about the page the editor names with its text, and about none for a text without one" $
    withWindow False (Just ["# Hello, world", "", "Saved, and mentioning @ada."]) $ \rig -> do
      -- The page as saved is the page read here.
      rig.askedAbout `shouldReturn` [rig.post]
      speak rig.editor (object ["page" .= ("page-moved" :: Text), "text" .= ("Typed, mentioning @bob." :: Text), "title" .= ("Hello, world" :: Text)])
      settle rig.view
      rig.askedAbout `shouldReturn` [PageId "page-moved", rig.post]
      speak rig.editor (object ["text" .= ("Typed, mentioning @cyd." :: Text), "title" .= ("Hello, world" :: Text)])
      settle rig.view
      rig.askedAbout `shouldReturn` [PageId "page-moved", rig.post]

  it "shows the Contents panel for the text the editor sends, unless it has too few headings or places its own" $
    withWindow False Nothing $ \rig -> do
      typed rig.editor ["## Setup", "", "One.", "", "## Use", "", "### Setup", "", "Two.", "", "## Setup", "", "Three."]
      settle rig.view
      textOf rig.view ".live-preview-state" `shouldReturn` "Following the editor."
      panelEntries rig.view `shouldReturn` ["Setup", "Use", "Setup", "Setup"]
      present rig.view ".page-view > .page-main.with-side.with-contents" `shouldReturn` True
      -- The panel's links, worked out from the text, lead to the headings
      -- the preview rendered, the same heading text twice included.
      linksResolve rig.view ".page-contents .toc-list a" `shouldReturn` True
      -- Too few headings: no panel.
      typed rig.editor ["## Setup", "", "One.", "", "## Use"]
      settle rig.view
      panelEntries rig.view `shouldReturn` []
      present rig.view ".page-main.with-side" `shouldReturn` False
      -- The text places the list itself: in the text, and no panel.
      typed rig.editor ["[[!toc]]", "", "## Setup", "", "## Use", "", "## Again"]
      settle rig.view
      panelEntries rig.view `shouldReturn` []
      findAll rig.view ".page-body nav.toc .toc-list a" >>= traverse N.textContent >>= (`shouldBe` ["Setup", "Use", "Again"])
      linksResolve rig.view ".page-body nav.toc .toc-list a" `shouldReturn` True
      -- Enough headings again: the panel follows.
      typed rig.editor ["## A", "", "## B", "", "## C"]
      settle rig.view
      panelEntries rig.view `shouldReturn` ["A", "B", "C"]

  it "takes the panel away with the text when the editor of a page of an encrypted space goes" $
    withWindow True Nothing $ \rig -> do
      typed rig.editor ["## A", "", "## B", "", "## C"]
      settle rig.view
      panelEntries rig.view `shouldReturn` ["A", "B", "C"]
      present rig.view ".page-title" `shouldReturn` True
      speak rig.editor (object ["closed" .= True])
      settle rig.view
      panelEntries rig.view `shouldReturn` []
      present rig.view ".page-title" `shouldReturn` False
      textOf rig.view ".live-preview-body" `shouldReturn` ""
      textOf rig.view ".live-preview-state" >>= (`shouldSatisfy` (T.isInfixOf "nothing of the page is kept here"))

  it "shows the Contents panel of the page as saved, before the editor speaks" $
    withWindow False (Just ["## One", "", "Text.", "", "## Two", "", "## Three"]) $ \rig -> do
      textOf rig.view ".live-preview-state" >>= (`shouldSatisfy` (T.isInfixOf "The page as saved"))
      panelEntries rig.view `shouldReturn` ["One", "Two", "Three"]
      linksResolve rig.view ".page-contents .toc-list a" `shouldReturn` True
