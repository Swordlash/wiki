-- | A day's page in the page view ("Wiki.App.View.Page"), on the in-memory
-- DOM ("Test.Harness.MemDom") over the demo's wiki: the tasks missed are
-- marked once the view has read the reader's clock, and as the clock
-- passes their hour; a box ticked takes the mark off and unticking puts it
-- back; a page whose title is no date marks nothing; and the first render
-- marks nothing, as the server's prerender does not; a page that is no
-- day's page reads no clock. The reader's clock is
-- the test's: a moment of 27 November 2026 plus the time on the test's
-- clock ('withTestClock'), so that one 'advance' moves both the view's
-- ticker and the time it reads. Which entries are missed is the pure
-- function's ("Test.Markdown.Day").
module Test.DayPageDom (spec) where

import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import Data.Text qualified as T
import Data.Time (LocalTime (..), NominalDiffTime, TimeOfDay (..), addLocalTime, fromGregorian)
import Halogen.Router qualified as Router
import Halogen.VDom.DOM.Monad (MemDOM, runMemDOM)
import Halogen.VDom.DOM.Monad.Native qualified as N
import Protolude hiding (find)
import Test.Harness.MemDom
import Test.Hspec
import Wiki.App.Backend (MonadWiki (..), Session, session)
import Wiki.App.Backend.Mem (MemWiki, newMemWiki, runMemWiki)
import Wiki.App.Backend.Mem.State (MemState (..))
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

-- | A day's list with one task at two in the afternoon and an entry
-- without a box at nine.
afternoon :: Text
afternoon = "{.day}\n- 09:00 Standup\n- [ ] 14:00 Call the bank\n"

-- | A day's list with one task at nine in the morning.
morning :: Text
morning = "{.day}\n- [ ] 09:00 Write the notes\n"

-- | The demo's team space, as its administrator sees it, with three pages
-- at its top: today's, 27 November 2026 ('afternoon'), yesterday's
-- ('morning'), and "Notes", a page whose title is no date, with
-- yesterday's list.
data Wiki = Wiki
  { wiki :: IORef MemState
  , input :: [Text] -> Maybe (IO LocalTime) -> Page.Input
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
    for_ [("2026-11-27", afternoon), ("2026-11-26", morning), ("Notes", morning)] $ \(title, text) ->
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

-- | The texts of the items marked missed.
missedItems :: View q i o -> IO [Text]
missedItems view = findAll view ".day-missed" >>= traverse (fmap T.strip . N.textContent)

spec :: Spec
spec = describe "a day's page in the page view, with the reader's clock" $ do
  it "marks a task once its hour has passed, on the ticker, and a tick takes the mark off until it is unticked" $
    withTestClock $ \clock -> withWiki $ \w ->
      withView (run w.wiki) Page.component (w.input ["2026-11-27"] (Just (clockOf clock))) $ \view -> do
        settle view
        -- Shown at half past one: the task at two is not missed yet, and
        -- the entry at nine has no box.
        findAll view ".task-list-item" >>= (`shouldBe` 1) . length
        missedItems view `shouldReturn` []
        -- 13:59, then 14:00 exactly: not missed.
        advance clock (minutes 29)
        settle view
        missedItems view `shouldReturn` []
        advance clock (minutes 1)
        settle view
        missedItems view `shouldReturn` []
        -- 14:01: the ticker reads the clock, and the task is missed.
        advance clock (minutes 1)
        settle view
        missedItems view `shouldReturn` ["14:00 Call the bank"]
        -- Ticked, not saved: done as the page shows it.
        click view "input[data-task=\"0\"]"
        settle view
        isJust <$> find view ".task-done" `shouldReturn` True
        missedItems view `shouldReturn` []
        -- Unticked: missed again.
        click view "input[data-task=\"0\"]"
        settle view
        isJust <$> find view ".task-done" `shouldReturn` False
        missedItems view `shouldReturn` ["14:00 Call the bank"]

  it "marks nothing on a page whose title is no date, and reads the clock again when a day's page is fetched" $
    withTestClock $ \clock -> withWiki $ \w ->
      withView (run w.wiki) Page.component (w.input ["notes"] (Just (clockOf clock))) $ \view -> do
        settle view
        textOf view ".page-text" >>= (`shouldSatisfy` T.isInfixOf "09:00 Write the notes")
        missedItems view `shouldReturn` []
        -- An hour later, at half past two: the ticker reads no clock for a
        -- page that is not a day's page.
        advance clock (minutes 60)
        settle view
        missedItems view `shouldReturn` []
        -- Today's page, shown in the same view: its task at two is missed
        -- only if the view read the clock again when it fetched the page.
        setInput view (w.input ["2026-11-27"] (Just (clockOf clock)))
        settle view
        missedItems view `shouldReturn` ["14:00 Call the bank"]
        -- The check of absence on "Notes" can fail: yesterday's page has
        -- the same list, and its task is missed.
        setInput view (w.input ["2026-11-26"] (Just (clockOf clock)))
        settle view
        missedItems view `shouldReturn` ["09:00 Write the notes"]

  it "reads no clock on a page that is no day's page, so that it renders no more often" $
    withTestClock $ \clock -> withWiki $ \w -> do
      -- Every read of the clock is followed by a write of the view's
      -- state, and so by a render and a parse of the page: a page that
      -- reads no clock renders as often as it did before there was one.
      reads' <- newIORef (0 :: Int)
      let counted = modifyIORef' reads' (+ 1) >> clockOf clock
      withView (run w.wiki) Page.component (w.input ["notes"] (Just counted)) $ \view -> do
        settle view
        advance clock (minutes 3)
        settle view
        readIORef reads' `shouldReturn` 0
        -- The count can grow: a day's page reads the clock when it is
        -- fetched, and on the ticker.
        setInput view (w.input ["2026-11-27"] (Just counted))
        settle view
        advance clock (minutes 3)
        settle view
        readIORef reads' `shouldReturn` 4

  it "reads the clock once on a page of a past date, where a later time changes nothing" $
    withTestClock $ \clock -> withWiki $ \w -> do
      reads' <- newIORef (0 :: Int)
      let counted = modifyIORef' reads' (+ 1) >> clockOf clock
      withView (run w.wiki) Page.component (w.input ["2026-11-26"] (Just counted)) $ \view -> do
        settle view
        missedItems view `shouldReturn` ["09:00 Write the notes"]
        advance clock (minutes 5)
        settle view
        readIORef reads' `shouldReturn` 1
        -- Today's page, in the same view, keeps reading: once when it is
        -- fetched, then on each of the five minutes.
        setInput view (w.input ["2026-11-27"] (Just counted))
        settle view
        advance clock (minutes 5)
        settle view
        readIORef reads' `shouldReturn` 7

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
      view <- mountIn page (run w.wiki) Page.component (w.input ["2026-11-26"] (Just reading))
      settle view
      -- Yesterday's task is missed at any hour of today, and the first
      -- render drew it unmarked.
      headMay . filter ((> 0) . fst) <$> readIORef seen `shouldReturn` Just (1, 0)
      missedItems view `shouldReturn` ["09:00 Write the notes"]
      dispose view
