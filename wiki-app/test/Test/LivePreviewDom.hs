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

import Data.Aeson (Value, encode, object, (.=))
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
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
import Wiki.App.View.Common (Context (..), PageWidth (..), ViewOutput (..))
import Wiki.App.View.LivePreview qualified as LivePreview
import Wiki.Core.Id (UserId (..))
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
  }

-- | The window at the blog's post "Hello, world", for the demo's
-- administrator, at the compact width; the space marked encrypted when
-- asked, and the post saved first with the text given, if any. The body
-- runs with it, once it is quiet.
withWindow :: Bool -> Maybe [Text] -> (Rig -> IO a) -> IO a
withWindow encrypted saved body = do
  wiki <- newMemWiki ((demoState demoNow) {current = Just (UserId "user-admin")})
  let run :: MemWiki MemDOM a -> MemDOM a
      run = runMemWiki wiki
      orPanic = either (panic . show) identity
  router <- runMemDOM (run (Router.newRouter "/" Unknown route))
  (me, spaces, blog, ps) <- runMemDOM . run $ do
    me <- session
    spaces <- listSpaces
    blog <- orPanic <$> getSpace (slugs ["blog"])
    ps <- orPanic <$> listPages blog.id
    for_ saved $ \ls -> do
      post <- orPanic <$> getPage blog.id (slugs ["hello-world"])
      void (orPanic <$> savePage post.id post.updatedAt post.title (T.unlines ls))
    pure (me, spaces, blog, ps)
  editor <- Editor <$> newIORef Nothing
  let space = if encrypted then blog {encryption = Just (KeyId "key-1")} else blog
      inputAt width = LivePreview.Input {context = (viewContextAs router me spaces 0) {pageWidth = width}, space, pages = ps, path = slugs ["hello-world"], listener = listenerOf editor}
  outcome <- withView run LivePreview.component (inputAt Compact) $ \view -> do
    settle view
    body Rig {view, editor, heard = outputs view, widthIs = \w -> setInput view (inputAt w) >> settle view}
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
