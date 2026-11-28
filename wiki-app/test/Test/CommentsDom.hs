-- | A page's comments ("Wiki.App.View.Comments", their threads stepped by
-- "Wiki.App.View.Comments.Machine"), the real page view mounted on the
-- in-memory DOM ("Test.Harness.MemDom") under a parent that changes its
-- input as the shell does, over the demo's wiki in memory, as Rita Reader.
-- Each example is a case #284's rounds found, and shows that the page view
-- carries out what the machine asks, in the order it asks.
--
-- The asks are held in one of two ways:
--
-- * with the library's held waits ('withHeldWaits'), where the in-memory
--   backend's own answer is wanted at a moment the test chooses: a write
--   held across a visit of another page, the panel closed and opened
--   again, the people lookup held while a later effect runs, a read that
--   fails;
-- * in a gate ('Gate'), where what the server told and what it answered
--   must come in the test's order: the write is made in the wiki at once,
--   and its answer waits in the gate, so that the live channel can tell
--   its echo before the answer or after it.
--
-- A quote is chosen from the reader's selection in the page, which only a
-- browser has ("Wiki.App.Browser"'s @readSelection@ reads none natively):
-- the cases about quoting (a quote stopped, the same text quoted again,
-- giving it up) stay with the machine's properties ("Test.Threads") and
-- the browser example that comments on selected text.
module Test.CommentsDom (spec) where

import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Primitive (PrimMonad)
import Control.Monad.Trans.Class (MonadTrans (..))
import Control.Monad.UUID (MonadUUID (..))
import Data.Aeson (object)
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import Data.List qualified as List
import Data.Text qualified as T
import Halogen.Router qualified as Router
import Halogen.VDom.DOM.Monad (MemDOM, runMemDOM)
import Halogen.VDom.DOM.Monad.Class (MonadAttributes, MonadBrowserDOM, MonadDOM)
import Halogen.VDom.DOM.Monad.Native qualified as N
import Protolude hiding (find)
import Test.Harness.MemDom hiding (held)
import Test.Harness.MemDom qualified as MemDom
import Test.Hspec hiding (pending)
import Wiki.Api.Events (PageEvent (CommentAdded, CommentDeleted, CommentEdited, ThreadDeleted, ThreadOpened, ThreadResolved, ThreadsChanged))
import Wiki.App.Backend (MonadLoad (..), MonadWiki (..), WikiError (..))
import Wiki.App.Backend.Comments (MonadComments (..))
import Wiki.App.Backend.Encrypted (MonadEncryption)
import Wiki.App.Backend.Mem (MemWiki, newMemWiki, runMemWiki)
import Wiki.App.Backend.Mem.State (MemState (..))
import Wiki.App.Backend.Notifications (MonadNotifications)
import Wiki.App.Backend.Stars (MonadStars)
import Wiki.App.Demo (demoNow, demoState)
import Wiki.App.Islands (MonadIslands)
import Wiki.App.Live (Heard (..))
import Wiki.App.Route (Route (..), route)
import Wiki.App.View.Common (ViewOutput, ViewQuery)
import Wiki.App.View.Page qualified as Page
import Wiki.Core.Comment (Comment (..), Thread (..))
import Wiki.Core.Id (CommentId (..), CommentThreadId (..), PageId, UserId (..))
import Wiki.Core.Page qualified as P
import Wiki.Core.Slug (Slug, mkSlug)
import Wiki.Core.Space qualified as S

-- | What the test controls of the backend: whether the next read of a
-- page's threads fails, and the gate a write's answer waits in, if any.
data Control = Control
  { failNext :: IORef Bool
  , reads :: IORef Int
  -- ^ How many reads of a page's threads were made.
  , gate :: Maybe (Gate (Maybe PageEvent) ())
  }

-- | The in-memory backend, with the reads of a page's threads failing when
-- the test says, and each write's answer, once the write is made, held in
-- the gate if there is one.
newtype Commenting m a = Commenting (ReaderT Control m a)
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadUnliftIO, PrimMonad)

instance MonadTrans Commenting where
  lift = Commenting . lift

deriving newtype instance (MonadDOM m) => MonadDOM (Commenting m)

deriving newtype instance (MonadAttributes m) => MonadAttributes (Commenting m)

deriving newtype instance (MonadBrowserDOM m) => MonadBrowserDOM (Commenting m)

deriving newtype instance (MonadIslands m) => MonadIslands (Commenting m)

instance (MonadUUID m) => MonadUUID (Commenting m) where
  generateV4 = lift generateV4

instance (MonadWiki m) => MonadWiki (Commenting m)

instance (MonadWiki m) => MonadLoad (Commenting m) where
  atOnce = fmap Just
  attempt = fmap Right

instance (MonadNotifications m) => MonadNotifications (Commenting m)

instance (MonadStars m) => MonadStars (Commenting m)

instance (MonadEncryption m) => MonadEncryption (Commenting m)

instance (MonadComments m, MonadIO m) => MonadComments (Commenting m) where
  listThreads page =
    Commenting (asks (\c -> c)) >>= \c ->
      liftIO (atomicModifyIORef' c.reads (\n -> (n + 1, ()))) >> pure c.failNext >>= \flag ->
        liftIO (atomicModifyIORef' flag (False,)) >>= \case
          True -> pure (Left (Unreachable "the network"))
          False -> lift (listThreads page)
  openThread p a b = made (fmap ThreadOpened) (openThread p a b)
  reply p t b = made (fmap CommentAdded) (reply p t b)
  editComment p t c b = made (fmap CommentEdited) (editComment p t c b)
  deleteComment p t c = made (fmap (const (CommentDeleted t c))) (deleteComment p t c)
  deleteThread p t = made (fmap (const (ThreadDeleted t))) (deleteThread p t)
  setResolved p t r = made (fmap (\x -> ThreadResolved x.id x.resolved)) (setResolved p t r)

-- | A write made in the wiki, and its answer held in the gate, with the
-- event it made, until the test lets it go.
made :: (MonadIO m) => (Either WikiError a -> Either WikiError PageEvent) -> m (Either WikiError a) -> Commenting m (Either WikiError a)
made event write = do
  answer' <- lift write
  Commenting (asks (\c -> c.gate)) >>= traverse_ (\g -> liftIO (hold g (rightToMaybe (event answer'))))
  pure answer'

slugs :: [Text] -> [Slug]
slugs = mapMaybe (rightToMaybe . mkSlug)

-- | The page view mounted at the blog's post "Hello, world" for Rita
-- Reader: the view, the live channel, the control of the backend, a way to
-- go to another post of the blog by its path, and the post's id and how
-- to read its threads in the wiki.
data Rig = Rig
  { view :: View ViewQuery Page.Input ViewOutput
  , live :: TestFeed
  , control :: Control
  , goTo :: [Text] -> IO ()
  , hello :: PageId
  , server :: IO [Thread]
  , elsewhere :: Text -> IO Thread
  -- ^ A thread about the whole post opened in the wiki by another tab of
  -- the reader's, which nothing tells this one of.
  , deletedElsewhere :: CommentThreadId -> IO ()
  }

-- | Mount the page view for the body, its asks held by the library, or
-- with the gate given.
withPage :: Maybe (Gate (Maybe PageEvent) ()) -> (Rig -> IO a) -> IO a
withPage gate body = do
  wiki <- newMemWiki ((demoState demoNow) {current = Just (UserId "user-reader")})
  failNext <- newIORef False
  readsMade <- newIORef 0
  live <- newTestFeed
  let control = Control {failNext, reads = readsMade, gate}
      run :: Commenting (MemWiki MemDOM) a -> MemDOM a
      run (Commenting p) = runMemWiki wiki (runReaderT p control)
      orPanic = either (panic . show) identity
  router <- runMemDOM (run (Router.newRouter "/" Unknown route))
  (me, spaces, blog, ps, hello) <- runMemDOM . run $ do
    me <- session
    spaces <- listSpaces
    blog <- orPanic <$> getSpace (slugs ["blog"])
    ps <- orPanic <$> listPages blog.id
    post <- orPanic <$> getPage blog.id (slugs ["hello-world"])
    pure (me, spaces, blog, ps, post.id :: PageId)
  let inputAt path = Page.Input {context = viewContextAs router me spaces 0, space = blog, pages = ps, path = slugs path, version = Nothing, feed = Just live.feed, clock = Nothing}
  outcome <- withView run Page.component (inputAt ["hello-world"]) $ \view ->
    body
      Rig
        { view
        , live
        , control
        , goTo = setInput view . inputAt
        , hello
        , server = orPanic <$> runMemDOM (run (listThreads hello))
        , elsewhere = \text -> orPanic <$> runMemDOM (run (lift (openThread hello Nothing text)))
        , deletedElsewhere = \tid -> orPanic <$> runMemDOM (run (lift (deleteThread hello tid)))
        }
  Router.stop router
  pure outcome

-- | A thread of the page, by its id.
thread :: Text -> Text
thread tid = "#comment-thread-" <> tid

-- | Write in a form's field and send the form.
send :: View q i o -> Text -> Text -> IO ()
send view form text = do
  typeInto view (form <> " textarea") text
  find view form >>= traverse_ (N.dispatch "submit" (object []))

-- | What a field holds, as the reader sees it.
valueIn :: View q i o -> Text -> IO (Maybe Text)
valueIn view selector = find view selector >>= fmap join . traverse (\n -> List.lookup "value" <$> N.propertyList n)

-- | The bodies of a thread's comments, in order.
bodies :: View q i o -> Text -> IO [Text]
bodies view tid = findAll view (thread tid <> " .comment-body") >>= traverse (fmap T.strip . N.textContent)

-- | Let every wait held go but those given, until only they are held.
drainBut :: HeldWaits -> [HeldWait] -> IO ()
drainBut waits kept = do
  held <- pending waits
  let others = [w | w <- held, w.number `notElem` map (\k -> k.number) kept]
  unless (null others) (traverse_ release others >> drainBut waits kept)

-- | Let every wait held go, until none is held.
drain :: HeldWaits -> IO ()
drain waits = drainBut waits []

-- | The one wait held now that is not among those given.
newOne :: HeldWaits -> [HeldWait] -> IO HeldWait
newOne waits known =
  pending waits >>= \held -> case [w | w <- held, w.number `notElem` map (\k -> k.number) known] of
    [w] -> pure w
    other -> panic ("expected one new wait, held: " <> show (map (\w -> w.number) held) <> ", new: " <> show (length other))

spec :: Spec
spec = describe "Wiki.App.View.Page's comments, mounted on the in-memory DOM" $ do
  -- #284's second round: the page view carries out a step's effects
  -- without waiting between them. A reply's answer looks up the people it
  -- names (forked) and tells the comments view under the page that asked
  -- (Answered, to that placement): with the lookup held, the form is
  -- emptied already, and the panel's draft is left alone.
  it "tells the form that asked of its answer while the people lookup is still held" $
    withHeldWaits $ \waits -> withPage Nothing $ \rig -> do
      drain waits
      click rig.view ".comments-toggle"
      drain waits
      typeInto rig.view (thread "thread-hello-1" <> " .comment-reply textarea") "Beside, not sent."
      send rig.view (thread "thread-hello-3" <> " .comment-reply") "Thanks @editor."
      w <- newOne waits []
      release w
      -- Held: the lookup of the people, that of the authors' avatars
      -- (the threads' machine asks both at once, where the page view
      -- looked the avatars up after the people), and the read that follows
      -- the write.
      held <- pending waits
      length held `shouldBe` 3
      valueIn rig.view (thread "thread-hello-3" <> " .comment-reply textarea") `shouldReturn` Just ""
      valueIn rig.view (thread "thread-hello-1" <> " .comment-reply textarea") `shouldReturn` Just "Beside, not sent."
      bodies rig.view "thread-hello-3" >>= (`shouldSatisfy` elem "Thanks @editor.")
      drain waits

  -- #284's first round: a write's answer is taken only on the visit it
  -- was sent on. Sent on the post, answered on another post, and answered
  -- on the post visited again: no form emptied, no thread chosen.
  it "leaves the forms of a later visit alone when a write sent on an earlier one is answered" $
    withHeldWaits $ \waits -> withPage Nothing $ \rig -> do
      drain waits
      let draft = ".page-comments .comment-new"
          laterVisit path = do
            -- Sent on the post.
            rig.goTo ["hello-world"]
            drain waits
            send rig.view draft "Sent on the first visit."
            w <- newOne waits []
            traverse_ (\p -> rig.goTo p >> drainBut waits [w]) path
            typeInto rig.view (draft <> " textarea") "A draft of the later visit."
            release w
            drain waits
            valueIn rig.view (draft <> " textarea") `shouldReturn` Just "A draft of the later visit."
            findAll rig.view ".comment-thread.focused" >>= (`shouldBe` 0) . length
      laterVisit [["notes-on-haskell"]]
      laterVisit [["notes-on-haskell"], ["hello-world"]]

  -- #284's third round: a write's answer names the mount of the comments
  -- view that asked. The panel closed and opened again is a new mount,
  -- whose draft the old answer leaves alone.
  it "leaves the draft of a panel opened again alone when the old panel's write is answered" $
    withHeldWaits $ \waits -> withPage Nothing $ \rig -> do
      drain waits
      let replyHere = thread "thread-hello-1" <> " .comment-reply"
      click rig.view ".comments-toggle"
      drain waits
      send rig.view replyHere "From the first panel."
      w <- newOne waits []
      click rig.view ".comments-close"
      drainBut waits [w]
      click rig.view ".comments-toggle"
      drainBut waits [w]
      typeInto rig.view (replyHere <> " textarea") "In the panel opened again."
      release w
      drain waits
      valueIn rig.view (replyHere <> " textarea") `shouldReturn` Just "In the panel opened again."

  -- The live channel tells the reader's own reply before its answer, or
  -- after it: shown once either way, and where the server has it once the
  -- read after the answer is taken.
  it "shows the reader's reply once, whether its echo comes before its answer or after, in the server's place" $ do
    gate <- newGate
    withPage (Just gate) $ \rig -> do
      let echoed = settleHolding rig.view (holds gate)
          echo h = for_ h.asked (rig.live.tell . Heard rig.hello)
      echoed
      send rig.view (thread "thread-hello-3" <> " .comment-reply") "Echoed first."
      echoed
      [first'] <- MemDom.held gate
      echo first'
      echoed
      answer gate first' ()
      settle rig.view
      send rig.view (thread "thread-hello-3" <> " .comment-reply") "Echoed after."
      echoed
      [second'] <- MemDom.held gate
      answer gate second' ()
      settle rig.view
      echo second'
      settle rig.view
      shown <- bodies rig.view "thread-hello-3"
      length (filter (== "Echoed first.") shown) `shouldBe` 1
      length (filter (== "Echoed after.") shown) `shouldBe` 1
      threads <- rig.server
      shown `shouldBe` concat [map (\c -> T.strip c.body) t.comments | t <- threads, t.id == CommentThreadId "thread-hello-3"]

  -- A read the view needed fails with threads shown: they stay, and the
  -- view says they are behind, until a read is taken.
  it "says its comments could not be refreshed, with Try again, until a read is taken" $
    withHeldWaits $ \waits -> withPage Nothing $ \rig -> do
      drain waits
      rig.live.tell (Heard rig.hello ThreadsChanged)
      r <- newOne waits []
      atomicModifyIORef' rig.control.failNext (const (True, ()))
      release r
      drain waits
      textOf rig.view ".comments-behind" >>= (`shouldSatisfy` (T.isInfixOf "Comments could not be refreshed."))
      textOf rig.view ".comments-behind .retry" `shouldReturn` "Try again"
      bodies rig.view "thread-hello-3" >>= (`shouldSatisfy` (not . null))
      click rig.view ".comments-behind .retry"
      drain waits
      findAll rig.view ".comments-behind" >>= (`shouldBe` 0) . length

  -- The reader's own changes reach the machine from the real page view: a
  -- thread chosen by its quote in the panel or its mark in the page, and
  -- the panel closed.
  it "chooses a thread from its quote or its mark, and closes the panel" $
    withHeldWaits $ \waits -> withPage Nothing $ \rig -> do
      drain waits
      click rig.view "mark.comment-mark[data-thread='thread-hello-1']"
      drain waits
      findAll rig.view (".comments-panel " <> thread "thread-hello-1" <> ".focused") >>= (`shouldBe` 1) . length
      click rig.view (".comments-panel " <> thread "thread-hello-2" <> " .comment-quote")
      drain waits
      findAll rig.view (".comments-panel " <> thread "thread-hello-2" <> ".focused") >>= (`shouldBe` 1) . length
      findAll rig.view "mark.comment-mark.comment-mark-focused" >>= (`shouldBe` 0) . length
      click rig.view ".comments-close"
      drain waits
      findAll rig.view ".comments-panel" >>= (`shouldBe` 0) . length

  -- What the browser examples of the post's threads showed, natively.
  describe "the post's threads" $ do
    it "replies to a thread, and resolves it, which unmarks its quote, and reopens it" $
      withHeldWaits $ \waits -> withPage Nothing $ \rig -> do
        drain waits
        let marked = findAll rig.view "mark.comment-mark[data-thread='thread-hello-1']"
        (traverse N.textContent =<< marked) `shouldReturn` ["a blog that is also a wiki"]
        textOf rig.view ".comments-toggle" `shouldReturn` "Comments (2)"
        click rig.view ".comments-toggle"
        drain waits
        send rig.view (thread "thread-hello-1" <> " .comment-reply") "There is now."
        drain waits
        bodies rig.view "thread-hello-1" `shouldReturn` ["Is there a list of every post somewhere, newest first?", "The blog's space lists its pages; its administrator can order them newest first.", "There is now."]
        click rig.view (thread "thread-hello-1" <> " .thread-resolve")
        drain waits
        length <$> marked `shouldReturn` 0
        textOf rig.view (thread "thread-hello-1" <> " .comment-resolved-note") `shouldReturn` "Resolved."
        textOf rig.view ".comments-toggle" `shouldReturn` "Comments (1)"
        click rig.view (thread "thread-hello-1" <> " .thread-reopen")
        drain waits
        length <$> marked `shouldReturn` 1

    it "edits and deletes one's own comment, and offers neither on another's" $
      withHeldWaits $ \waits -> withPage Nothing $ \rig -> do
        drain waits
        click rig.view ".comments-toggle"
        drain waits
        -- Rita's first comment may be edited; Edgar's reply may not.
        (traverse N.textContent =<< findAll rig.view (thread "thread-hello-1" <> " .comment-actions button")) `shouldReturn` ["Edit", "Delete"]
        click rig.view (thread "thread-hello-1" <> " .comment-edit-button")
        drain waits
        send rig.view (thread "thread-hello-1" <> " .comment-edit") "Is there a list of every post?"
        drain waits
        textOf rig.view (thread "thread-hello-1" <> " .comment-edited") `shouldReturn` "(edited)"
        take 1 <$> bodies rig.view "thread-hello-1" `shouldReturn` ["Is there a list of every post?"]
        -- The page thread's reply is hers too: deleted, after asking.
        click rig.view (thread "thread-hello-3" <> " .comment-delete")
        drain waits
        click rig.view (thread "thread-hello-3" <> " .comment-confirm .danger")
        drain waits
        length <$> findAll rig.view (thread "thread-hello-3" <> " .comment") `shouldReturn` 1

    it "shows a thread about the whole post under it, and takes a new one" $
      withHeldWaits $ \waits -> withPage Nothing $ \rig -> do
        drain waits
        textOf rig.view (".page-comments " <> thread "thread-hello-3" <> " .comment-author") `shouldReturn` "Ada Admin"
        send rig.view ".page-comments .comment-new" "A **fine** post."
        drain waits
        length <$> findAll rig.view ".page-comments .comment-thread" `shouldReturn` 2
        (traverse N.textContent =<< findAll rig.view ".page-comments .comment-thread strong") `shouldReturn` ["fine"]
        -- And not among the threads about the text.
        textOf rig.view ".comments-toggle" `shouldReturn` "Comments (2)"

    it "lists a thread whose quote the post no longer has last, saying so" $
      withHeldWaits $ \waits -> withPage Nothing $ \rig -> do
        drain waits
        click rig.view ".comments-toggle"
        drain waits
        (traverse (N.attribute "data-thread") =<< findAll rig.view ".comments-panel .comment-thread") `shouldReturn` [Just "thread-hello-1", Just "thread-hello-2"]
        textOf rig.view ".comments-panel .comment-thread.orphaned .comment-quote" `shouldReturn` "with the preview always beside it"
        textOf rig.view ".comments-panel .comment-thread.orphaned .comment-orphaned" `shouldReturn` "The page no longer has this text."

    it "hears another tab's reply, and takes the subscription's answer in place of what it shows, asking nothing" $
      withHeldWaits $ \waits -> withPage Nothing $ \rig -> do
        drain waits
        threads <- rig.server
        let comment' = case [c | t <- threads, t.id == CommentThreadId "thread-hello-3", c <- take 1 t.comments] of
              c : _ -> c {id = CommentId "comment-elsewhere", body = "From another tab."}
              [] -> panic "no comment"
        rig.live.tell (Heard rig.hello (CommentAdded comment'))
        drain waits
        bodies rig.view "thread-hello-3" >>= (`shouldSatisfy` (\bs -> lastMay bs == Just "From another tab."))
        -- Made between the post's read and its subscription: the answer
        -- has it, and it is shown, with nothing read.
        meanwhile <- rig.elsewhere "Made meanwhile."
        readsBefore <- readIORef rig.control.reads
        rig.live.tell (Replaced rig.hello [meanwhile])
        drain waits
        readIORef rig.control.reads `shouldReturn` readsBefore
        length <$> findAll rig.view ".page-comments .comment-thread" `shouldReturn` 1
        bodies rig.view (unThread meanwhile.id) `shouldReturn` ["Made meanwhile."]
        length <$> findAll rig.view "mark.comment-mark" `shouldReturn` 0

    it "asks one read of the threads at a time, and the one wanted meanwhile once the first answers" $
      -- The machine keeps one read of the page out at a time, so that the
      -- server handles reads in the order they were asked: a read wanted
      -- while one is out is asked when that one answers.
      withHeldWaits $ \waits -> withPage Nothing $ \rig -> do
        drain waits
        readsBefore <- readIORef rig.control.reads
        rig.live.tell (Heard rig.hello ThreadsChanged)
        first' <- newOne waits []
        -- Told again while that read is out: no second read is asked.
        rig.live.tell (Heard rig.hello ThreadsChanged)
        map (\h -> h.number) <$> pending waits `shouldReturn` [first'.number]
        release first'
        -- Its answer came: the read wanted meanwhile is asked now (held
        -- with the lookup of the people the answer names), and reads what
        -- was made after the first was read.
        made' <- rig.elsewhere "Made meanwhile."
        drain waits
        readIORef rig.control.reads `shouldReturn` readsBefore + 2
        bodies rig.view (unThread made'.id) `shouldReturn` ["Made meanwhile."]
  where
    unThread (CommentThreadId t) = t
