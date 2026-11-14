-- | The viewer of a presentation in a page ("Wiki.App.View.DeckViewer"),
-- the real component, on the in-memory DOM ("Test.Harness.MemDom"), over
-- the demo's wiki, its asks held ('withHeldWaits'), its input changed as
-- the shell changes it ('driven'). Its link comes near the window when the
-- test says so: the viewer is given a 'Watcher' of the test's, which keeps
-- the watches it runs and tells them on 'nearNow', in place of the
-- browser's @IntersectionObserver@.
--
-- What it shows is that the shell carries out the machine
-- ("Wiki.App.View.DeckViewer.Machine") with no wait between its effects:
-- #275's two races, which came from waits in the shell, and the rules the
-- machine's generated runs check ("Test.Deck"), on the real component.
module Test.DeckDom (spec) where

import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Primitive (PrimMonad)
import Control.Monad.Trans.Class (MonadTrans (..))
import Control.Monad.UUID (MonadUUID (..))
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import Data.List qualified as List
import Data.Map.Strict qualified as Map
import Halogen.Router qualified as Router
import Halogen.VDom.DOM.Monad (MemDOM, runMemDOM)
import Halogen.VDom.DOM.Monad.Class (MonadAttributes, MonadBrowserDOM, MonadDOM)
import Protolude hiding (find)
import Test.Harness.MemDom
import Test.Hspec hiding (pending)
import Wiki.App.Backend (MonadLoad (..), MonadWiki (..), Session, UrlOwner, anonymousIn)
import Wiki.App.Backend.Mem (MemWiki, newMemWiki, runMemWiki)
import Wiki.App.Backend.Mem.State (MemState (..))
import Wiki.App.Backend.Notifications (MonadNotifications)
import Wiki.App.Demo (demoNow, demoState)
import Wiki.App.Islands (MonadIslands)
import Wiki.App.Route (PageLink (..), Route (..), route)
import Wiki.App.View.Body (Place (..))
import Wiki.App.View.Common (Context (..))
import Wiki.App.View.DeckViewer qualified as DeckViewer
import Wiki.Core.Access (Access (Open))
import Wiki.Core.Id (UserId (..))
import Wiki.Core.Slug (Slug, mkSlug)
import Wiki.Core.Space (Space (..))

-- | What the test sees of the backend: the owners of addresses made and
-- let go, in order.
data Control = Control
  { made :: IORef [UrlOwner]
  , released :: IORef [UrlOwner]
  }

newtype Decking m a = Decking (ReaderT Control m a)
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadUnliftIO, PrimMonad)

instance MonadTrans Decking where
  lift = Decking . lift

deriving newtype instance (MonadDOM m) => MonadDOM (Decking m)

deriving newtype instance (MonadAttributes m) => MonadAttributes (Decking m)

deriving newtype instance (MonadBrowserDOM m) => MonadBrowserDOM (Decking m)

deriving newtype instance (MonadIslands m) => MonadIslands (Decking m)

instance (MonadUUID m) => MonadUUID (Decking m) where
  generateV4 = lift generateV4

instance (MonadWiki m, MonadIO m) => MonadWiki (Decking m) where
  newUrlOwner = do
    o <- lift newUrlOwner
    Decking (asks (\c -> c.made)) >>= \r -> liftIO (atomicModifyIORef' r (\os -> (os <> [o], ())))
    pure o
  releaseUrls o = do
    Decking (asks (\c -> c.released)) >>= \r -> liftIO (atomicModifyIORef' r (\os -> (os <> [o], ())))
    lift (releaseUrls o)

instance (MonadWiki m, MonadIO m) => MonadLoad (Decking m) where
  atOnce = fmap Just
  attempt = fmap Right

instance (MonadNotifications m, MonadIO m) => MonadNotifications (Decking m)

-- | The test's watches of the link: the ones running, by number, with what
-- each tells.
data Watches = Watches
  { running :: IORef (Map Int (IO ()))
  , next :: IORef Int
  }

newWatches :: IO Watches
newWatches = Watches <$> newIORef mempty <*> newIORef 0

-- | The viewer's 'DeckViewer.Watcher' over the test's watches.
watcherOf :: Watches -> DeckViewer.Watcher
watcherOf ws =
  DeckViewer.Watcher
    { start = \_ tell -> do
        i <- atomicModifyIORef' ws.next (\n -> (n + 1, n + 1))
        atomicModifyIORef' ws.running (\m -> (Map.insert i tell m, ()))
        pure (i, pass)
    , stop = \i -> atomicModifyIORef' ws.running (\m -> (Map.delete i m, ()))
    }

-- | The link near the window: every watch running tells once, and stops,
-- as the browser's does.
nearNow :: Watches -> IO ()
nearNow ws = atomicModifyIORef' ws.running (mempty :: Map Int (IO ()),) >>= traverse_ identity . Map.elems

watching :: Watches -> IO Int
watching ws = Map.size <$> readIORef ws.running

slugs :: [Text] -> [Slug]
slugs = mapMaybe (rightToMaybe . mkSlug)

-- | A reader's side as the shell gives it: the session, its number, and
-- the shell's revision.
data Reading = Reading {session :: Session, number :: Int, revision :: Int}

data Rig = Rig
  { view :: View (Drive DeckViewer.Input)
  , control :: Control
  , watches :: Watches
  , readAs :: Reading -> IO ()
  -- ^ The viewer given its input for this reader.
  , admin :: Session
  , visitor :: Session
  }

-- | The viewer of the guide's presentation, embedded in a page of the
-- docs, for the demo's administrator, mounted; the body runs with it.
withDeck :: (Rig -> IO a) -> IO a
withDeck body = do
  wiki <- newMemWiki ((demoState demoNow) {current = Just (UserId "user-admin")})
  control <- Control <$> newIORef [] <*> newIORef []
  watches <- newWatches
  let run :: Decking (MemWiki MemDOM) a -> IO a
      run (Decking p) = runMemDOM (runMemWiki wiki (runReaderT p control))
      orPanic = either (panic . show) identity
  router <- run (Router.newRouter "/" Unknown route)
  (me, spaces, docs, ps) <- run $ do
    me <- session
    spaces <- listSpaces
    docs <- orPanic <$> getSpace (slugs ["docs"])
    ps <- orPanic <$> listPages docs.id
    pure (me, spaces, docs, ps)
  let contextOf r = (viewContextAs router r.session spaces r.revision) {sessionNumber = r.number}
      inputOf r =
        DeckViewer.Input
          { context = contextOf r
          , target = DeckViewer.Linked (PageLink (Page (slugs ["docs"]) (slugs ["markdown-guide", "presentations"])) "") Place {context = contextOf r, space = docs, pages = ps, path = slugs ["markdown-guide"], attachments = [], urls = mempty, downloads = mempty, frames = mempty, people = mempty}
          , caption = "The presentations"
          , ids = "deck-"
          , watcher = watcherOf watches
          }
      opening = Reading {session = me, number = 0, revision = 0}
  view <- inTurn (mount run (driven DeckViewer.component (inputOf opening)) ())
  outcome <- body Rig {view, control, watches, readAs = setInput view . inputOf, admin = me, visitor = anonymousIn Open}
  Router.stop router
  pure outcome

-- | Every wait held let go, until none is.
drain :: HeldWaits -> IO ()
drain waits = pending waits >>= \held -> unless (null held) (traverse_ release held >> drain waits)

-- | The slide shown, out of the deck's, or nothing for the link.
position :: View q -> IO (Maybe Text)
position view = textOf view ".deck-embed-position"

made, released :: Rig -> IO [UrlOwner]
made rig = readIORef rig.control.made
released rig = readIORef rig.control.released

spec :: Spec
spec = describe "Wiki.App.View.DeckViewer, mounted on the in-memory DOM" $ do
  it "keeps no watch once closed, however its waits come" $
    -- #275: the watch made on a worker and kept a step later; a close in
    -- between left it watching.
    withHeldWaits $ \waits -> withDeck $ \rig -> do
      held <- pending waits
      traverse_ release held
      rig.view.dispose
      drain waits
      watching rig.watches `shouldReturn` 0

  it "starts one watch and one read for changes of reader close together" $
    withHeldWaits $ \waits -> withDeck $ \rig -> do
      drain waits
      watching rig.watches `shouldReturn` 1
      rig.readAs Reading {session = rig.admin, number = 0, revision = 1}
      rig.readAs Reading {session = rig.visitor, number = 1, revision = 1}
      rig.readAs Reading {session = rig.admin, number = 2, revision = 2}
      drain waits
      watching rig.watches `shouldReturn` 1
      nearNow rig.watches
      drain waits
      position rig.view `shouldReturn` Just "1 / 9"
      length <$> made rig `shouldReturn` 1
      watching rig.watches `shouldReturn` 0
      rig.view.dispose

  it "hears the watch kept when the reader changes as it starts" $
    -- #275: the channel made in a wait; a change of reader in it left a
    -- watch telling for the reading before, never heard.
    withHeldWaits $ \waits -> withDeck $ \rig -> do
      rig.readAs Reading {session = rig.admin, number = 0, revision = 1}
      drain waits
      nearNow rig.watches
      drain waits
      position rig.view `shouldReturn` Just "1 / 9"
      rig.view.dispose

  it "reads again for the same user signed out and in, given in one input" $
    withHeldWaits $ \waits -> withDeck $ \rig -> do
      drain waits
      nearNow rig.watches
      drain waits
      position rig.view `shouldReturn` Just "1 / 9"
      [o] <- made rig
      rig.readAs Reading {session = rig.admin, number = 2, revision = 0}
      drain waits
      position rig.view `shouldReturn` Nothing
      released rig `shouldReturn` [o]
      nearNow rig.watches
      drain waits
      position rig.view `shouldReturn` Just "1 / 9"
      length <$> made rig `shouldReturn` 2
      rig.view.dispose

  it "shows no answer read for an earlier reading, and lets each read's addresses go once" $
    withHeldWaits $ \waits -> withDeck $ \rig -> do
      drain waits
      nearNow rig.watches
      -- The read held; the reader changes; then its answer comes.
      _ <- pending waits
      rig.readAs Reading {session = rig.admin, number = 0, revision = 1}
      drain waits
      position rig.view `shouldReturn` Nothing
      watching rig.watches `shouldReturn` 1
      nearNow rig.watches
      drain waits
      position rig.view `shouldReturn` Just "1 / 9"
      rig.view.dispose
      drain waits
      ms <- made rig
      rs <- released rig
      length ms `shouldBe` 2
      List.sort rs `shouldBe` List.sort ms
