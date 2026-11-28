-- | A day's page in the page view ("Wiki.App.View.Page"), on the in-memory
-- DOM ("Test.Harness.MemDom") over the demo's wiki: the tasks missed are
-- marked once the view has read the reader's clock, and each at its
-- deadline, by one timer to the next deadline and nothing between; a box
-- ticked takes the mark off and unticking puts it back; a page whose title
-- is no date marks nothing and reads no clock; a page of a past date sets
-- no timer; and the first render marks nothing, as the server's prerender
-- does not. The reader's clock is the test's: a moment of 27 November 2026
-- plus the time on the test's clock ('withTestClock'), so that one
-- 'advance' moves both the view's timer and the time it reads; the clock
-- counts its reads. Which entries are missed, and when the next one is,
-- is the pure functions' ("Test.Markdown.Day").
module Test.DayPageDom (spec) where

import Data.IORef (IORef, atomicModifyIORef', modifyIORef', newIORef, readIORef)
import Data.Set qualified as Set
import Data.Text qualified as T
import Data.Time (LocalTime (..), NominalDiffTime, TimeOfDay (..), addLocalTime, diffLocalTime, fromGregorian)
import Halogen.Router qualified as Router
import Halogen.Trace qualified as Trace
import Halogen.VDom.DOM.Monad (MemDOM, runMemDOM)
import Halogen.VDom.DOM.Monad.Native qualified as N
import Protolude hiding (find)
import Test.Harness.MemDom
import Test.Hspec
import Wiki.App.Backend (MonadWiki (..), Session, session)
import Wiki.App.Backend.Mem (MemWiki, newMemWiki, runMemWiki)
import Wiki.App.Backend.Mem.State (MemState (..))
import Wiki.App.Browser (ReaderClock (..))
import Wiki.App.Demo (demoNow, demoState)
import Wiki.App.Route (Route (..), route)
import Wiki.App.View.Page qualified as Page
import Wiki.Core.Id (UserId (..))
import Wiki.Core.Page (PageSummary)
import Wiki.Core.Slug (Slug, mkSlug)
import Wiki.Core.Space (Space (..))

slugs :: [Text] -> [Slug]
slugs = mapMaybe (rightToMaybe . mkSlug)

orPanic :: (Show e) => Either e a -> a
orPanic = either (panic . show) identity

-- | A day's list with a task at two in the afternoon, the task numbered
-- 0, one at half past four, numbered 1, and an entry without a box at
-- nine.
afternoon :: Text
afternoon = "{.day}\n- 09:00 Standup\n- [ ] 14:00 Call the bank\n- [ ] 16:30 Send the report\n"

-- | A day's list with one task at nine in the morning.
morning :: Text
morning = "{.day}\n- [ ] 09:00 Write the notes\n"

-- | The demo's team space, as its administrator sees it, with four pages
-- at its top: today's, 27 November 2026 ('afternoon'), yesterday's
-- ('morning'), the page of 6 January 2027, forty days later, with the
-- same list as yesterday's, and "Notes", a page whose title is no date,
-- with yesterday's list.
data Wiki = Wiki
  { wiki :: IORef MemState
  , input :: [Text] -> Maybe ReaderClock -> Page.Input
  }

run :: IORef MemState -> MemWiki MemDOM a -> MemDOM a
run = runMemWiki

withWiki :: (Wiki -> IO a) -> IO a
withWiki body = do
  wiki <- newMemWiki ((demoState demoNow) {current = Just (UserId "user-admin")})
  router <- runMemDOM (run wiki (Router.newRouter "/" Unknown route))
  (admin, spaces, team, ps) <- runMemDOM . run wiki $ do
    admin <- session
    spaces <- listSpaces
    team <- orPanic <$> getSpace (slugs ["team"])
    for_ [("2026-11-27", afternoon), ("2026-11-26", morning), ("2027-01-06", morning), ("Notes", morning)] $ \(title, text) ->
      orPanic <$> createPage team.id Nothing title text
    ps <- orPanic <$> listPages team.id
    pure (admin, spaces, team, ps)
  outcome <- body Wiki {wiki, input = pageInput router admin spaces team ps}
  Router.stop router
  pure outcome
  where
    pageInput router (admin :: Session) spaces team (ps :: [PageSummary]) path clock =
      Page.Input {context = viewContextAs router admin spaces 0, space = team, pages = ps, path = slugs path, version = Nothing, feed = Nothing, clock}

-- | Half past one in the afternoon of 27 November 2026, when the test's
-- clock is at zero.
start :: LocalTime
start = LocalTime (fromGregorian 2026 11 27) (TimeOfDay 13 30 0)

-- | The reader's clock: 'start' plus the time on the test's clock.
clockOf :: TestClock -> IO LocalTime
clockOf clock = (`addLocalTime` start) <$> now clock

minutes :: NominalDiffTime -> NominalDiffTime
minutes = (* 60)

-- | The reader's clock over a read of the present time: on the test's
-- clock, which has no summer time, how long until a moment is the
-- difference of the two local times.
readerOf :: TestClock -> IO LocalTime -> ReaderClock
readerOf clock reading = ReaderClock {readNow = reading, waitUntil = \at -> diffLocalTime at <$> clockOf clock}

-- | The reader's clock, counting its reads of the present time.
countedClock :: TestClock -> IO (ReaderClock, IO Int)
countedClock clock = do
  reads' <- newIORef (0 :: Int)
  pure (readerOf clock (modifyIORef' reads' (+ 1) >> clockOf clock), readIORef reads')

-- | The program's trace while the body runs ("Halogen.Trace"): every
-- tree's events, the view's among them.
withTrace :: (IORef [Trace.Event] -> IO a) -> IO a
withTrace body = do
  events <- newIORef []
  bracket (Trace.subscribe (\e -> atomicModifyIORef' events (\es -> (e : es, ())))) identity (\_ -> body events)

-- | How many forks of the view's components are alive, made and not
-- ended: the page view's timer, when nothing else of it waits. The view's
-- components are those whose root on the page is the root of the view.
liveForks :: IORef [Trace.Event] -> View q i o -> IO Int
liveForks events view = do
  es <- reverse <$> readIORef events
  root <- headMay <$> N.childNodes view.container
  let made = ordNub [e.tag.id | e <- es, Trace.Mounted <- [e.what]]
  mine <- filterM (\i -> (\n -> isJust root && (N.toNative <$> n) == root) <$> Trace.rootNode i) made
  let forked = Set.fromList [(e.tag.id, f) | e <- es, Trace.Forked f <- [e.what], e.tag.id `elem` mine]
      ended = Set.fromList [(e.tag.id, f) | e <- es, Trace.ForkEnded f <- [e.what]]
  pure (Set.size (forked `Set.difference` ended))

-- | Move the test's clock on, and let the view do what that wakes.
passing :: TestClock -> View q i o -> NominalDiffTime -> IO ()
passing clock view d = advance clock d >> settle view

-- | The texts of the items marked missed.
missedItems :: View q i o -> IO [Text]
missedItems view = findAll view ".day-missed" >>= traverse (fmap T.strip . N.textContent)

spec :: Spec
spec = describe "a day's page in the page view, with the reader's clock" $ do
  it "reads the clock when the page is shown and at each deadline, and marks each task there" $
    withTestClock $ \clock -> withWiki $ \w -> do
      (reading, reads') <- countedClock clock
      withView (run w.wiki) Page.component (w.input ["2026-11-27"] (Just reading)) $ \view -> do
        settle view
        -- Shown at half past one: nothing is missed yet, and the entry
        -- at nine has no box.
        findAll view ".task-list-item" >>= (`shouldBe` 2) . length
        missedItems view `shouldReturn` []
        reads' `shouldReturn` 1
        -- 13:59: nothing read, nothing marked.
        passing clock view (minutes 29)
        reads' `shouldReturn` 1
        missedItems view `shouldReturn` []
        -- 14:00: the timer reads the clock, and the first task is missed.
        passing clock view (minutes 1)
        reads' `shouldReturn` 2
        missedItems view `shouldReturn` ["14:00 Call the bank"]
        -- Nothing more until half past four.
        passing clock view (minutes 149)
        reads' `shouldReturn` 2
        passing clock view (minutes 1)
        reads' `shouldReturn` 3
        missedItems view `shouldReturn` ["14:00 Call the bank", "16:30 Send the report"]
        -- After the last deadline no timer is left.
        passing clock view (minutes (12 * 60))
        reads' `shouldReturn` 3

  it "takes the mark off a task ticked, and leaves no timer for a task ticked before its time" $
    withTrace $ \events -> withTestClock $ \clock -> withWiki $ \w -> do
      (reading, reads') <- countedClock clock
      withView (run w.wiki) Page.component (w.input ["2026-11-27"] (Just reading)) $ \view -> do
        settle view
        liveForks events view `shouldReturn` 1
        passing clock view (minutes 30)
        liveForks events view `shouldReturn` 1
        missedItems view `shouldReturn` ["14:00 Call the bank"]
        reads' `shouldReturn` 2
        -- Ticked, not saved: done as the page shows it. Each change of
        -- the text shown reads the clock once.
        click view "input[data-task=\"0\"]"
        settle view
        isJust <$> find view ".task-done" `shouldReturn` True
        missedItems view `shouldReturn` []
        click view "input[data-task=\"0\"]"
        settle view
        isJust <$> find view ".task-done" `shouldReturn` False
        missedItems view `shouldReturn` ["14:00 Call the bank"]
        -- Each tick set the timer anew and stopped the one before.
        liveForks events view `shouldReturn` 1
        -- The task at half past four ticked before its time: no deadline
        -- is left, and no timer.
        click view "input[data-task=\"1\"]"
        settle view
        reads' `shouldReturn` 5
        liveForks events view `shouldReturn` 0
        passing clock view (minutes (3 * 60))
        reads' `shouldReturn` 5
        missedItems view `shouldReturn` ["14:00 Call the bank"]

  it "sets a timer again for a task unticked before its time" $
    withTrace $ \events -> withTestClock $ \clock -> withWiki $ \w -> do
      (reading, reads') <- countedClock clock
      withView (run w.wiki) Page.component (w.input ["2026-11-27"] (Just reading)) $ \view -> do
        settle view
        -- At half past one the task at half past four is ticked, at three
        -- it is unticked again.
        click view "input[data-task=\"1\"]"
        settle view
        passing clock view (minutes 30)
        reads' `shouldReturn` 3
        passing clock view (minutes 60)
        click view "input[data-task=\"1\"]"
        settle view
        reads' `shouldReturn` 4
        liveForks events view `shouldReturn` 1
        -- Its timer wakes at half past four, once.
        passing clock view (minutes 89)
        reads' `shouldReturn` 4
        missedItems view `shouldReturn` ["14:00 Call the bank"]
        passing clock view (minutes 1)
        reads' `shouldReturn` 5
        missedItems view `shouldReturn` ["14:00 Call the bank", "16:30 Send the report"]
        passing clock view (minutes (12 * 60))
        reads' `shouldReturn` 5

  it "sets no timer on a page of a past date, and reads no clock on a page whose title is no date" $
    withTestClock $ \clock -> withWiki $ \w -> do
      (reading, reads') <- countedClock clock
      withView (run w.wiki) Page.component (w.input ["notes"] (Just reading)) $ \view -> do
        settle view
        textOf view ".page-text" >>= (`shouldSatisfy` T.isInfixOf "09:00 Write the notes")
        missedItems view `shouldReturn` []
        passing clock view (minutes (12 * 60))
        reads' `shouldReturn` 0
        missedItems view `shouldReturn` []
        -- Yesterday's page, in the same view: read once when it is
        -- fetched, its task missed, and no timer.
        setInput view (w.input ["2026-11-26"] (Just reading))
        settle view
        missedItems view `shouldReturn` ["09:00 Write the notes"]
        reads' `shouldReturn` 1
        passing clock view (minutes (24 * 60))
        reads' `shouldReturn` 1

  it "waits for a change forty days away in pauses of twelve hours, and marks the task when it comes" $
    withTestClock $ \clock -> withWiki $ \w -> do
      (reading, reads') <- countedClock clock
      withView (run w.wiki) Page.component (w.input ["2027-01-06"] (Just reading)) $ \view -> do
        settle view
        reads' `shouldReturn` 1
        -- A day and a half: three pauses of twelve hours, each a read.
        passing clock view (minutes (36 * 60))
        reads' `shouldReturn` 4
        missedItems view `shouldReturn` []
        -- Its day and hour: the task is missed.
        elapsed <- now clock
        passing clock view (diffLocalTime (LocalTime (fromGregorian 2027 1 6) (TimeOfDay 9 0 0)) start - elapsed)
        missedItems view `shouldReturn` ["09:00 Write the notes"]

  it "waits what the reader's clock says is left until a deadline, across a jump of the clock" $
    withTestClock $ \clock -> withWiki $ \w -> do
      -- The local time jumps an hour forward ten minutes after the start,
      -- from 13:40 to 14:40, as it does in the night the clocks go
      -- forward: 14:00 comes ten minutes after the start, not thirty.
      let jumpAt = minutes 10
          wall t = addLocalTime (t + (if t >= jumpAt then minutes 60 else 0)) start
          left t at
            | at <= wall t = diffLocalTime at (wall t)
            | t < jumpAt && at > wall jumpAt = diffLocalTime at (wall jumpAt) + (jumpAt - t)
            | t < jumpAt && at > addLocalTime jumpAt start = jumpAt - t
            | otherwise = diffLocalTime at (wall t)
          jumping = ReaderClock {readNow = wall <$> now clock, waitUntil = \at -> (`left` at) <$> now clock}
      withView (run w.wiki) Page.component (w.input ["2026-11-27"] (Just jumping)) $ \view -> do
        settle view
        missedItems view `shouldReturn` []
        passing clock view (minutes 10)
        missedItems view `shouldReturn` ["14:00 Call the bank"]
        -- 16:30 is 110 minutes after 14:40.
        passing clock view (minutes 109)
        missedItems view `shouldReturn` ["14:00 Call the bank"]
        passing clock view (minutes 1)
        missedItems view `shouldReturn` ["14:00 Call the bank", "16:30 Send the report"]

  it "reads the clock again when a day's page is fetched in the same view" $
    withTestClock $ \clock -> withWiki $ \w ->
      withView (run w.wiki) Page.component (w.input ["notes"] (Just (readerOf clock (clockOf clock)))) $ \view -> do
        settle view
        passing clock view (minutes 60)
        -- Today's page at half past two: its task at two is missed only
        -- if the view read the clock when it fetched the page.
        setInput view (w.input ["2026-11-27"] (Just (readerOf clock (clockOf clock))))
        settle view
        missedItems view `shouldReturn` ["14:00 Call the bank"]

  it "marks nothing without a clock, as in the server's prerender" $
    withWiki $ \w ->
      withView (run w.wiki) Page.component (w.input ["2026-11-26"] Nothing) $ \view -> do
        settle view
        findAll view ".task-list-item" >>= (`shouldBe` 1) . length
        missedItems view `shouldReturn` []

  it "marks nothing in its first render: the view reads the clock after it" $
    withTestClock $ \clock -> withWiki $ \w -> do
      page <- N.newPage
      -- What the page held each time the view read the clock: how many
      -- tasks it showed, and how many of them were marked missed. The
      -- first read that finds the page drawn finds the view's first render,
      -- whatever read the clock before it.
      seen <- newIORef []
      let reading = do
            let document = N.pageDocument page
            tasks <- length <$> N.querySelectorAll ".task-list-item" document
            marked <- length <$> N.querySelectorAll ".day-missed" document
            modifyIORef' seen (<> [(tasks, marked)])
            clockOf clock
      view <- mountIn page (run w.wiki) Page.component (w.input ["2026-11-26"] (Just (readerOf clock reading)))
      settle view
      -- Yesterday's task is missed at any hour of today, and the first
      -- render drew it unmarked.
      headMay . filter ((> 0) . fst) <$> readIORef seen `shouldReturn` Just (1, 0)
      missedItems view `shouldReturn` ["09:00 Write the notes"]
      dispose view
