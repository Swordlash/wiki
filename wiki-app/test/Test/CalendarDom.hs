-- | The calendar ("Wiki.App.Plugin.Calendar"), a kind of page a plugin
-- brings, in the real views on the in-memory DOM ("Test.Harness.MemDom"),
-- over the demo's wiki: "New page" ("Wiki.App.View.NewPage") offers the
-- kind while the plugin is on and not while it is off, and the page view
-- ("Wiki.App.View.Page") shows a calendar's days after its text while the
-- plugin is on, and the calendar as a document while it is off. Which days
-- are listed, and in which order, is the plugin's pure function's
-- ("Test.Plugins").
module Test.CalendarDom (spec) where

import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as T
import Halogen.Router (Router)
import Halogen.Router qualified as Router
import Halogen.Subscription qualified as HS
import Halogen.VDom.DOM.Monad (MemDOM, runMemDOM)
import Protolude hiding (find)
import Test.Harness.MemDom
import Test.Hspec
import Wiki.Api.Plugins (PluginChoice (..), PluginId (..), PluginState (..), Plugins (..), noPlugins)
import Wiki.App.Backend (MonadWiki (..), Session, session)
import Wiki.App.Backend.Mem (MemWiki, newMemWiki, runMemWiki, switchUser)
import Wiki.App.Backend.Mem.State (MemState (..))
import Wiki.App.Demo (demoNow, demoState)
import Wiki.App.Route (Route (..), route)
import Wiki.App.Route qualified as R
import Wiki.App.View.Common (Context (..))
import Wiki.App.View.NewPage qualified as NewPage
import Wiki.App.View.Page qualified as Page
import Wiki.Core.Id (UserId (..))
import Wiki.Core.Page (PageSummary)
import Wiki.Core.Page qualified as P
import Wiki.Core.PageKind (PageKind (..), calendarKind)
import Wiki.Core.Slug (Slug, mkSlug)
import Wiki.Core.Space (Space (..))

slugs :: [Text] -> [Slug]
slugs = mapMaybe (rightToMaybe . mkSlug)

orPanic :: (Show e) => Either e a -> a
orPanic = either (panic . show) identity

-- | The team's space of the demo, with a calendar "Days" under its home
-- page holding two days, a page whose title is no date and a template
-- titled with a date, and an empty calendar "Plans" in its projects.
data Wiki = Wiki
  { wiki :: IORef MemState
  , router :: Router Route
  , went :: IORef [Route]
  -- ^ Where the router went, in order.
  , spaces :: [Space]
  , team :: Space
  , projects :: Space
  , days :: P.Page
  , admin :: Session
  }

run :: IORef MemState -> MemWiki MemDOM a -> MemDOM a
run = runMemWiki

withWiki :: (Wiki -> IO a) -> IO a
withWiki body = do
  wiki <- newMemWiki ((demoState demoNow) {current = Just (UserId "user-admin")})
  router <- runMemDOM (run wiki (Router.newRouter "/" Unknown route))
  went <- newIORef []
  _ <- HS.subscribe (Router.changes router) (\r -> modifyIORef' went (<> [r]))
  (admin, spaces, team, projects, days) <- runMemDOM . run wiki $ do
    admin <- session
    spaces <- listSpaces
    team <- orPanic <$> getSpace (slugs ["team"])
    projects <- orPanic <$> getSpace (slugs ["team", "projects"])
    days <- orPanic <$> createPageOf calendarKind team.id Nothing "Days" "What the team does, day by day."
    for_ ["2026-11-25", "2026-11-27", "Notes"] $ \t -> orPanic <$> createPage team.id (Just days.id) t ("About " <> t <> ".")
    template <- orPanic <$> createPage team.id (Just days.id) "2026-11-26" "A template."
    _ <- orPanic <$> setPageTemplate template.id True
    _ <- orPanic <$> createPageOf calendarKind projects.id Nothing "Plans" ""
    pure (admin, spaces, team, projects, days)
  outcome <- body Wiki {wiki, router, went, spaces, team, projects, days, admin}
  Router.stop router
  pure outcome

-- | The pages of a space, as the shell gives them to its views.
pagesOf :: Wiki -> Space -> IO [PageSummary]
pagesOf w s = orPanic <$> runMemDOM (run w.wiki (listPages s.id))

-- | The plugins as the server keeps them, with the calendar off for the
-- reader alone, or for the whole wiki.
offForMe, offForWiki' :: Plugins
offForMe = noPlugins {mine = Map.fromList [(PluginId "calendar", PluginChoice {state = PluginOff, options = Nothing})]}
offForWiki' = noPlugins {offForWiki = Set.fromList [PluginId "calendar"]}

-- | The page view's input for a page of a space, for this reader, with
-- the plugins as given.
pageInput :: Wiki -> Session -> Plugins -> Space -> [PageSummary] -> [Text] -> Page.Input
pageInput w who known s ps path =
  Page.Input {context = (viewContextAs w.router who w.spaces 0) {plugins = known}, space = s, pages = ps, path = slugs path, version = Nothing, feed = Nothing, clock = Nothing}

-- | The texts of the days the view lists, in order.
listedDays :: View q i o -> IO [Text]
listedDays view = findAll view ".calendar-day" >>= traverse (fmap T.strip . textContent)

-- | The kinds "New page" offers, by their labels' first word.
kindsOffered :: View q i o -> IO [Text]
kindsOffered view = findAll view ".new-page-kind-choice" >>= traverse (fmap (T.takeWhile (/= ' ') . T.strip) . textContent)

spec :: Spec
spec = describe "the calendar, a plugin's kind of page" $ do
  describe "New page" $ do
    let newPage w known = NewPage.Input {context = (viewContextAs w.router w.admin w.spaces 0) {plugins = known}, space = w.team, pages = [], parent = Nothing}
    it "offers a calendar while the plugin is on, and not while it is off" $ withWiki $ \w -> do
      ps <- pagesOf w w.team
      withView (run w.wiki) NewPage.component (newPage w noPlugins) {NewPage.pages = ps} $ \view -> do
        settle view
        kindsOffered view `shouldReturn` ["Document", "Presentation", "Calendar"]
        setInput view (newPage w offForMe) {NewPage.pages = ps}
        settle view
        kindsOffered view `shouldReturn` ["Document", "Presentation"]
        setInput view (newPage w offForWiki') {NewPage.pages = ps}
        settle view
        kindsOffered view `shouldReturn` ["Document", "Presentation"]

    it "makes a calendar, and a document when the plugin goes off while a calendar is chosen" $ withWiki $ \w -> do
      ps <- pagesOf w w.team
      withView (run w.wiki) NewPage.component (newPage w noPlugins) {NewPage.pages = ps} $ \view -> do
        settle view
        typeInto view ".new-page-title" "Week"
        choices <- findAll view ".new-page-kind-choice input"
        maybe (expectationFailure "no third kind offered") clickNode (atMay choices 2)
        settle view
        setInput view (newPage w offForMe) {NewPage.pages = ps}
        settle view
        click view ".new-page-form button[type=submit]"
        settle view
      made <- runMemDOM (run w.wiki (getPage w.team.id (slugs ["week"])))
      (\p -> p.kind) <$> made `shouldBe` Right Document
      withView (run w.wiki) NewPage.component (newPage w noPlugins) {NewPage.pages = ps} $ \view -> do
        settle view
        typeInto view ".new-page-title" "Month"
        choices <- findAll view ".new-page-kind-choice input"
        maybe (expectationFailure "no third kind offered") clickNode (atMay choices 2)
        settle view
        click view ".new-page-form button[type=submit]"
        settle view
      again <- runMemDOM (run w.wiki (getPage w.team.id (slugs ["month"])))
      (\p -> p.kind) <$> again `shouldBe` Right calendarKind

  describe "the page view" $ do
    it "shows a calendar's text, then its days newest first, and opens a day" $ withWiki $ \w -> do
      ps <- pagesOf w w.team
      withView (run w.wiki) Page.component (pageInput w w.admin noPlugins w.team ps ["days"]) $ \view -> do
        settle view
        textOf view ".page-text" >>= (`shouldSatisfy` T.isInfixOf "What the team does, day by day.")
        -- Not the template titled with a date, nor the page whose title
        -- is no date.
        listedDays view `shouldReturn` ["Friday, 27 November 2026", "Wednesday, 25 November 2026"]
        -- After the text.
        drawn <- html view
        T.breakOn "calendar-days" drawn `shouldSatisfy` (T.isInfixOf "page-text" . fst)
        -- A calendar is not made a presentation.
        isJust <$> find view ".page-kind" `shouldReturn` False
        click view ".calendar-day a"
        settle view
      readIORef w.went `shouldReturn` [R.Page (slugs ["team"]) (slugs ["days", "2026-11-27"])]

    it "shows a calendar as a document while the plugin is off, for the reader or for the wiki" $ withWiki $ \w -> do
      ps <- pagesOf w w.team
      for_ [offForMe, offForWiki'] $ \known ->
        withView (run w.wiki) Page.component (pageInput w w.admin known w.team ps ["days"]) $ \view -> do
          settle view
          textOf view ".page-text" >>= (`shouldSatisfy` T.isInfixOf "What the team does, day by day.")
          isJust <$> find view ".calendar-days" `shouldReturn` False
          isJust <$> find view ".page-kind" `shouldReturn` False
      -- The checks of absence can fail: a document has the toggle, and the
      -- calendar its list while the plugin is on.
      withView (run w.wiki) Page.component (pageInput w w.admin offForMe w.team ps ["days", "notes"]) $ \view -> do
        settle view
        isJust <$> find view ".page-kind" `shouldReturn` True

    it "shows no days on a version of a calendar" $ withWiki $ \w -> do
      ps <- pagesOf w w.team
      let current = pageInput w w.admin noPlugins w.team ps ["days"]
      withView (run w.wiki) Page.component current $ \view -> do
        settle view
        isJust <$> find view ".calendar-days" `shouldReturn` True
        setInput view current {Page.version = Just w.days.updatedAt}
        settle view
        textOf view ".page-text" >>= (`shouldSatisfy` T.isInfixOf "What the team does, day by day.")
        isJust <$> find view ".version-note" `shouldReturn` True
        isJust <$> find view ".calendar-days" `shouldReturn` False

    it "says how to add a day to an empty calendar to whoever may edit it, and only that it is empty to a reader" $ withWiki $ \w -> do
      ps <- pagesOf w w.projects
      withView (run w.wiki) Page.component (pageInput w w.admin noPlugins w.projects ps ["plans"]) $ \view -> do
        settle view
        listedDays view `shouldReturn` []
        textOf view ".calendar-empty" `shouldReturn` "No days yet. To add one, make a page under this calendar titled with its date, such as 2026-11-27, from the template “Day”."
      rita <- runMemDOM (run w.wiki (switchUser (Just (UserId "user-reader")) >> session))
      withView (run w.wiki) Page.component (pageInput w rita noPlugins w.projects ps ["plans"]) $ \view -> do
        settle view
        textOf view ".calendar-empty" `shouldReturn` "No days yet."
