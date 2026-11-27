{-# LANGUAGE TemplateHaskell #-}

-- | Tables of contents and templates (WP4.15 of docs/PLAN.md) in a browser,
-- over the demo's wiki: the "Contents" panel beside a long page, which
-- marks the section being read as the page scrolls and stays folded once
-- folded; @[[!toc]]@ placing the list in the page instead; a heading's link
-- to itself; a page made a template, listed apart from the tree; and "New
-- page" from a template, the space's and a built-in one, with its
-- placeholders filled in. And a footnote's number, titled with its note,
-- which scrolls to the note when followed. And a calendar, a kind of page:
-- its month, drawn by FullCalendar, whose days' numbers open the days'
-- pages, and a page left while the month's library loads.
module Test.Browser.Contents (spec) where

import Control.Monad.UUID (MonadUUID)
import Data.Aeson (decodeStrict)
import Data.IORef (IORef)
import Data.Row (type (.==))
import Data.Text qualified as T
import Data.Time (Day, defaultTimeLocale, formatTime, fromGregorian, getCurrentTime, utctDay)
import Data.Time.Format.ISO8601 (iso8601Show)
import Halogen qualified as H
import Halogen.HTML qualified as HH
import Halogen.JSBits (Inert (..), JSText, Safety (..), browserJS, fromJSText, inBrowser, toJSText)
import Halogen.JSBits.Promise (Promise (..), wait)
import Halogen.Router (Router)
import Halogen.VDom.DOM.Monad.Class (MonadBrowserDOM)
import Protolude hiding (find, wait)
import Test.Browser.Mount (slugs, start, startWith, startWritten, testRouter)
import Test.Browser.Outside (untilAttribute, untilFound)
import Test.Hspec.Halogen
import Wiki.App.Assistant (MonadAssistant)
import Wiki.App.Backend (MonadWiki (..))
import Wiki.App.Backend.Mem (newMemWiki, runMemWiki)
import Wiki.App.Backend.Mem.State (MemState (..))
import Wiki.App.Demo (demoNow, demoState)
import Wiki.App.Islands (MonadIslands)
import Wiki.App.Plugin (Child (..), KindInput (..), KindView (..), Plugin (..))
import Wiki.App.Plugin qualified as Plugin
import Wiki.App.Plugin.Calendar (calendar)
import Wiki.App.Route (Route (..))
import Wiki.Core.Id (UserId (..))
import Wiki.Core.Page (Page (body, id, kind, space, template), PageSummary (parent, slug))
import Wiki.Core.PageKind qualified as Kind
import Wiki.Core.Space (Space (id))

-- Where things are in the window (test/jsbits/test-layout.js); a
-- calendar's month held and read (test/jsbits/test-calendar.js); and the
-- browser's Back (test/jsbits/test-live.js). One splice for the module:
-- on wasm each splice declares the same loader.
$( browserJS
     ["jsbits/test-layout.js", "jsbits/test-calendar.js", "jsbits/test-live.js"]
     [ ("js_scroll_toPromise", "wiki_test_scroll_to", Safe, [t|Int -> IO (Promise ())|])
     , ("js_scroll_into_viewPromise", "wiki_test_scroll_into_view", Safe, [t|JSText -> IO (Promise ())|])
     , ("js_box", "wiki_test_box", Safe, [t|JSText -> IO JSText|])
     , ("js_calendar_hold", "wiki_test_calendar_hold", Safe, [t|IO ()|])
     , ("js_calendar_held", "wiki_test_calendar_held", Safe, [t|IO Int|])
     , ("js_calendar_releasePromise", "wiki_test_calendar_release", Safe, [t|IO (Promise ())|])
     , ("js_calendar_hosts", "wiki_test_calendar_hosts", Safe, [t|IO JSText|])
     , ("js_calendar_errors", "wiki_test_calendar_errors", Safe, [t|IO JSText|])
     , ("js_calendar_months", "wiki_test_calendar_months", Safe, [t|IO Int|])
     , ("js_calendar_today", "wiki_test_calendar_today", Safe, [t|JSText -> IO ()|])
     , ("js_calendar_focused", "wiki_test_calendar_focused", Safe, [t|IO JSText|])
     , ("js_history_back_heardPromise", "wiki_test_history_back_heard", Safe, [t|IO (Promise ())|])
     , ("js_calendar_click", "wiki_test_calendar_click", Safe, [t|JSText -> Bool -> IO Bool|])
     , ("js_calendar_fail_first", "wiki_test_calendar_fail_first", Safe, [t|IO ()|])
     , ("js_calendar_unfail", "wiki_test_calendar_unfail", Safe, [t|IO JSText|])
     , ("js_calendar_focused_date", "wiki_test_calendar_focused_date", Safe, [t|IO JSText|])
     ]
 )

js_calendar_release :: IO ()
js_calendar_release = if inBrowser then wait =<< js_calendar_releasePromise else pure inert

js_history_back_heard :: IO ()
js_history_back_heard = if inBrowser then wait =<< js_history_back_heardPromise else pure inert

js_scroll_to :: Int -> IO ()
js_scroll_to a1 = if inBrowser then wait =<< js_scroll_toPromise a1 else pure inert

js_scroll_into_view :: JSText -> IO ()
js_scroll_into_view a1 = if inBrowser then wait =<< js_scroll_into_viewPromise a1 else pure inert

spec :: forall m -> (Typeable m, MonadBrowserTest m, MonadBrowserDOM m, MonadIslands m, MonadAssistant m, MonadUUID m) => Spec
spec m = do
  it "lists a long page's headings beside it, and marks the section being read as it scrolls" $ runPage $ do
    ui <- start m Nothing (Page (slugs ["docs"]) (slugs ["markdown-guide"]))
    panel <- find ui ".page-side .page-contents"
    getAttribute panel "open" `shouldReturn` Just ""
    titles <- traverse textContent =<< findAll ui ".page-contents nav > .toc-list > li > a"
    titles `shouldBe` ["Markdown guide"]
    nested <- traverse textContent =<< findAll ui ".page-contents nav > .toc-list > li > .toc-list > li > a"
    take 3 nested `shouldBe` ["Text", "Code", "Mathematics"]
    deeper <- traverse textContent =<< findAll ui ".page-contents a[href='#text'] + .toc-list > li > a"
    deeper `shouldBe` ["Lists", "Tasks", "Tables", "Emoji"]
    -- Nothing of the panel's in the page's text.
    length <$> findAll ui ".page-text .toc" `shouldReturn` 0
    -- At the top, the first section is the one read. jsbits/toc.js marks
    -- the section read when the browser's IntersectionObserver calls it,
    -- outside the program: each wait is for its mark on the section's
    -- link, and then what is marked is read once.
    marked "#markdown-guide"
    current ui `shouldReturn` ["#markdown-guide"]
    scrollTo "#tables"
    marked "#tables"
    current ui `shouldReturn` ["#tables"]
    scrollTo "#mathematics"
    marked "#mathematics"
    current ui `shouldReturn` ["#mathematics"]
    -- A section's link scrolls to it: the last, at the end of the page,
    -- which cannot be scrolled to the top of the window.
    find ui ".page-contents a[href='#images-video-and-attachments']" >>= click
    marked "#images-video-and-attachments"
    current ui `shouldReturn` ["#images-video-and-attachments"]
    scrollTop 0
    marked "#markdown-guide"
    current ui `shouldReturn` ["#markdown-guide"]

  it "folds the panel, and keeps it folded on the next long page" $ runPage $ do
    ui <- start m Nothing (Page (slugs ["docs"]) (slugs ["markdown-guide"]))
    find ui ".page-contents-title" >>= click
    eventually $ find ui ".page-contents" >>= (`getAttribute` "open") >>= (`shouldBe` Nothing)
    find ui ".page-contents .toc-list" >>= shouldBeHidden
    unmount ui
    again <- start m Nothing (Page (slugs ["docs"]) (slugs ["markdown-guide"]))
    eventually $ find again ".page-contents" >>= (`getAttribute` "open") >>= (`shouldBe` Nothing)
    -- Unfolded again, for whatever comes next in this browser.
    find again ".page-contents-title" >>= click
    eventually $ find again ".page-contents .toc-list" >>= shouldBeVisible

  it "shows no panel for a page with few headings, and the list where [[!toc]] is" $ runPage $ do
    ui <- start m (Just "user-admin") (Page (slugs ["team"]) (slugs ["team-handbook", "on-call"]))
    find ui ".page-view .page-body h1" >>= (`shouldHaveText` "On-call")
    length <$> findAll ui ".page-contents" `shouldReturn` 0
    unmount ui
    handbook <- start m (Just "user-admin") (Page (slugs ["team"]) (slugs ["team-handbook"]))
    _ <- find handbook ".page-body nav.toc"
    inline <- traverse textContent =<< findAll handbook ".page-body nav.toc .toc-list a"
    inline `shouldBe` ["Team handbook", "The week", "Before your first day", "Meetings"]
    length <$> findAll handbook ".page-contents" `shouldReturn` 0
    -- Its links are followed in the page, and mark the section too.
    find handbook ".page-body nav.toc a[href='#meetings']" >>= click
    eventually $ current handbook `shouldReturn` ["#meetings"]

  it "ends each heading with a link to itself" $ runPage $ do
    ui <- start m Nothing (Page (slugs ["docs"]) (slugs ["markdown-guide"]))
    anchor <- find ui ".page-body h2#code > a.heading-anchor"
    getAttribute anchor "href" `shouldReturn` Just "#code"
    -- It adds nothing to the heading's text.
    find ui ".page-body h2#code" >>= (`shouldHaveText` "Code")
    -- Shown on hover only, by the stylesheet.
    getAttribute anchor "aria-label" `shouldReturn` Just "Link to this section"
    -- A click copies the section's address rather than following it
    -- (WP5.53), and the page stays as it is: the browser may refuse the
    -- clipboard here, and the notice shows the address either way.
    click anchor
    eventually $ find ui ".link-copied-address" >>= textContent >>= (`shouldSatisfy` T.isSuffixOf "/#code")
    find ui ".link-copied-label" >>= textContent >>= (`shouldSatisfy` (`elem` ["Link copied:", "The link could not be copied. It is selected, to copy by hand:"]))
    getAttribute anchor "href" `shouldReturn` Just "#code"
    find ui ".page-body h2#code" >>= (`shouldHaveText` "Code")

  -- Back to the reference is the browser's, under the application's
  -- router (Wiki.App.Browser.followArticleLink); the test router keeps the
  -- route in the fragment, where no entry for the note can go.
  it "titles a footnote's number with its note, and scrolls to the note when the number is followed" $ runPage $ do
    ui <- start m Nothing (Page (slugs ["docs"]) (slugs ["markdown-guide"]))
    reference <- find ui ".page-body sup.footnote-ref a[href='#fn-1']"
    getAttribute reference "id" `shouldReturn` Just "fnref-1"
    getAttribute reference "title" `shouldReturn` Just "A path without a leading slash is read from this page's own address, as a browser reads a relative link."
    -- The note is at the end of a long page, out of sight from the top.
    scrollTop 0
    noteBox >>= (`shouldSatisfy` \(top, _, window) -> top > window)
    click reference
    eventually $ noteBox >>= (`shouldSatisfy` \(top, _, window) -> top >= 0 && top < window)

  it "makes a page a template, listed apart from the tree, and an ordinary page again" $ runPage $ do
    (ui, wiki) <- startWith m (Just "user-admin") (Page (slugs ["docs"]) (slugs ["markdown-guide", "diagrams"]))
    _ <- find ui ".page-actions .use-as-template"
    tree <- traverse textContent =<< findAll ui ".wiki-sidebar .page-tree a"
    tree `shouldBe` ["Markdown guide", "Diagrams", "Mathematics", "Presentations"]
    length <$> findAll ui ".sidebar-templates" `shouldReturn` 0
    toggle <- find ui ".page-actions .use-as-template"
    getAttribute toggle "aria-pressed" `shouldReturn` Just "false"
    click toggle
    find ui ".template-note" >>= textContent >>= (`shouldSatisfy` T.isInfixOf "{{title}}")
    find ui ".page-actions .use-as-template" >>= (`getAttribute` "aria-pressed") >>= (`shouldBe` Just "true")
    eventually $ (traverse textContent =<< findAll ui ".wiki-sidebar .sidebar-templates a") `shouldReturn` ["Diagrams"]
    (traverse textContent =<< findAll ui ".wiki-sidebar .page-tree a") `shouldReturn` ["Markdown guide", "Mathematics", "Presentations"]
    (\p -> p.template) <$> readPage wiki ["docs"] ["markdown-guide", "diagrams"] `shouldReturn` True
    find ui ".page-actions .use-as-template" >>= click
    eventually $ length <$> findAll ui ".sidebar-templates" `shouldReturn` 0
    length <$> findAll ui ".template-note" `shouldReturn` 0
    (\p -> p.template) <$> readPage wiki ["docs"] ["markdown-guide", "diagrams"] `shouldReturn` False

  it "makes a new page from the space's template, its placeholders filled in" $ runPage $ do
    ui <- start m (Just "user-admin") (Space (slugs ["team"]))
    _ <- find ui ".wiki-sidebar .sidebar-templates a"
    (traverse textContent =<< findAll ui ".wiki-sidebar .sidebar-templates a") `shouldReturn` ["Weekly sync"]
    unmount ui
    (form, wiki) <- startWith m (Just "user-admin") (NewPage (slugs ["team"]) Nothing)
    _ <- find form ".new-page-choice"
    choices <- traverse textContent =<< findAll form ".new-page-choice"
    map (T.takeWhile (/= ' ') . T.strip) choices `shouldBe` ["Blank", "Weekly", "Meeting", "Decision", "How-to", "Day"]
    find form ".new-page-title" >>= (`typeText` "Sync of the 26th")
    chooseStart form "Weekly sync"
    find form ".new-page-view button.primary" >>= click
    find form ".editor-heading" >>= (`shouldHaveText` "Editing Sync of the 26th")
    today <- unsafeIOToPageM (toS . iso8601Show . utctDay <$> getCurrentTime)
    made <- readPage wiki ["team"] ["sync-of-the-26th"]
    T.lines made.body
      `shouldSatisfy` (\ls -> take 3 ls == ["# Sync of the 26th", "", "Weekly sync of " <> today <> ", notes by Ada Admin."])
    -- The template keeps its placeholders.
    (\p -> T.take 11 p.body) <$> readPage wiki ["team"] ["weekly-sync"] `shouldReturn` "# {{title}}"

  it "makes a new page from a built-in template, and a blank one" $ runPage $ do
    (ui, wiki) <- startWith m (Just "user-editor") (NewPage (slugs ["blog"]) Nothing)
    find ui ".new-page-title" >>= (`typeText` "Pick a database")
    chooseStart ui "Decision record"
    find ui ".new-page-view button.primary" >>= click
    find ui ".editor-heading" >>= (`shouldHaveText` "Editing Pick a database")
    made <- readPage wiki ["blog"] ["pick-a-database"]
    T.take 18 made.body `shouldBe` "# Pick a database\n"
    made.body `shouldSatisfy` T.isInfixOf "| **Written by** | Edgar Editor |"
    made.body `shouldSatisfy` (not . T.isInfixOf "{{")

  it "makes a presentation, offering a presentation's templates only, and starts a document again blank" $ runPage $ do
    (ui, wiki) <- startWith m (Just "user-editor") (NewPage (slugs ["blog"]) Nothing)
    kinds <- traverse textContent =<< findAll ui ".new-page-kind-choice"
    map (T.takeWhile (/= ' ') . T.strip) kinds `shouldBe` ["Document", "Presentation", "Calendar"]
    chooseKind ui "Presentation"
    eventually $ (map (T.takeWhile (/= ' ') . T.strip) <$> (traverse textContent =<< findAll ui ".new-page-choice")) `shouldReturn` ["Blank", "Starter"]
    chooseStart ui "Starter deck"
    -- Back to a document: its templates, and blank again.
    chooseKind ui "Document"
    eventually $ (map (T.takeWhile (/= ' ') . T.strip) <$> (traverse textContent =<< findAll ui ".new-page-choice")) `shouldReturn` ["Blank", "Meeting", "Decision", "How-to", "Day"]
    checked <- filterM (\c -> (== "true") <$> (findIn c "input" >>= (`getProperty` "checked"))) =<< findAll ui ".new-page-choice"
    traverse (fmap (T.takeWhile (/= ' ') . T.strip) . textContent) checked `shouldReturn` ["Blank"]
    chooseKind ui "Presentation"
    find ui ".new-page-title" >>= (`typeText` "Quarterly review")
    chooseStart ui "Starter deck"
    find ui ".new-page-view button.primary" >>= click
    find ui ".editor-heading" >>= (`shouldHaveText` "Editing Quarterly review")
    made <- readPage wiki ["blog"] ["quarterly-review"]
    made.kind `shouldBe` Kind.Presentation
    T.take 19 made.body `shouldBe` "# Quarterly review\n"
    made.body `shouldSatisfy` (not . T.isInfixOf "{{")

  it "makes a calendar, adds a day under it from the template, lists the day and opens it" $ runPage $ do
    (ui, wiki) <- startWith m (Just "user-admin") (NewPage (slugs ["team"]) Nothing)
    chooseKind ui "Calendar"
    -- A calendar has no templates of its own. Choosing a kind asks
    -- nothing: the click's settle is the form drawn again, read once.
    (map (T.takeWhile (/= ' ') . T.strip) <$> (traverse textContent =<< findAll ui ".new-page-choice")) `shouldReturn` ["Blank"]
    find ui ".new-page-title" >>= (`typeText` "Days")
    find ui ".new-page-view button.primary" >>= click
    find ui ".editor-heading" >>= (`shouldHaveText` "Editing Days")
    (\p -> p.kind) <$> readPage wiki ["team"] ["days"] `shouldReturn` Kind.calendarKind
    treeLink ui "Days" >>= click
    find ui ".calendar-empty" >>= (`shouldHaveText` "No days yet. To add one, make a page under this calendar titled with its date, such as 2026-11-27, from the template “Day”.")
    length <$> findAll ui ".page-actions .page-kind" `shouldReturn` 0
    -- A day: a page under the calendar, titled with its date, from the
    -- template "Day".
    find ui ".page-actions a[href*='/new-page/']" >>= click
    find ui ".new-page-title" >>= (`typeText` "2026-11-27")
    chooseStart ui "Day"
    find ui ".new-page-view button.primary" >>= click
    find ui ".editor-heading" >>= (`shouldHaveText` "Editing 2026-11-27")
    day <- readPage wiki ["team"] ["days", "2026-11-27"]
    day.kind `shouldBe` Kind.Document
    day.body `shouldSatisfy` T.isInfixOf "{.day}"
    treeLink ui "Days" >>= click
    -- The month's island says it drew the month, and the list of days
    -- folds; unfolded, it is the keyboard's short way to a day.
    untilAttribute ".calendar-all-days" "open" Nothing
    find ui ".calendar-all-days summary" >>= click
    find ui ".calendar-day a" >>= (`shouldHaveText` "Friday, 27 November 2026")
    length <$> findAll ui ".calendar-empty" `shouldReturn` 0
    find ui ".calendar-day a" >>= click
    find ui ".page-view h1" >>= (`shouldHaveText` "2026-11-27")
    length <$> findAll ui ".calendar-days" `shouldReturn` 0

  it "shows a calendar's month with the numbers of its days as links, opens a day, and marks a day of the month before" $ runPage $ do
    -- The month's today is fixed (its hook of the time now, at noon of the
    -- day), and the days written are taken from the same day: the walk
    -- crosses no midnight and no month's end.
    let today = fromGregorian 2026 11 27
        here = fromGregorian 2026 11 15
        before = fromGregorian 2026 10 15
        linkTo d = ".calendar-day-link[data-date='" <> iso d <> "']"
    unsafeIOToPageM (js_calendar_today (toJSText (iso today)))
    (ui, _) <- startWritten m (writeCalendar [here, before]) (Just "user-admin") (Page (slugs ["team"]) (slugs ["days"]))
    -- The month is FullCalendar's, drawn once its file has loaded; its
    -- island says so, which folds the list of days. Then it is read once.
    untilAttribute ".calendar-all-days" "open" Nothing
    unsafeIOToPageM js_calendar_months `shouldReturn` 1
    shown <- findAll ui ".calendar-day-link"
    traverse (`getAttribute` "data-date") shown `shouldReturn` [Just (iso here)]
    traverse textContent shown `shouldReturn` ["15"]
    -- Named for a screen reader with the whole date.
    traverse (`getAttribute` "aria-label") shown `shouldReturn` [Just (dayWords here)]
    -- The month opens on today's, which is marked.
    length <$> findAll ui (".calendar-month [aria-current=date][data-date='" <> iso today <> "']") `shouldReturn` 1
    -- The day's number is a link to the day's page: the address the list
    -- of days links it with.
    listed <- find ui ".calendar-all-days .calendar-day a" >>= (`getAttribute` "href")
    (traverse (`getAttribute` "href") shown) `shouldReturn` [listed]
    listed `shouldSatisfy` maybe False (T.isInfixOf (iso here))
    -- A click with Ctrl held is the browser's, which opens the address in
    -- another tab: the page does not take it, and stays. The same click
    -- without Ctrl is taken, and opens the day, which shows the check can
    -- fail. (The test keeps the browser from following either.)
    unsafeIOToPageM (js_calendar_click (toJSText (linkTo here)) True) `shouldReturn` False
    waitQuiet
    length <$> findAll ui ".calendar-month" `shouldReturn` 1
    unsafeIOToPageM (js_calendar_click (toJSText (linkTo here)) False) `shouldReturn` True
    waitQuiet
    find ui ".page-view h1" >>= textContent >>= (`shouldBe` iso here)
    unsafeIOToPageM js_history_back_heard
    waitQuiet
    untilAttribute ".calendar-all-days" "open" Nothing
    -- A reader's own click on the number opens the day's page in this tab.
    find ui (linkTo here) >>= click
    -- The day's page is read from the wiki once the click has gone to it.
    waitQuiet
    (find ui ".page-view h1" >>= textContent) `shouldReturn` iso here
    length <$> findAll ui ".calendar-month" `shouldReturn` 0
    -- Back on the calendar, a new month is drawn: paged to the month
    -- before, it marks that month's day.
    unsafeIOToPageM js_history_back_heard
    waitQuiet
    untilAttribute ".calendar-all-days" "open" Nothing
    find ui ".calendar-month [aria-label^='Previous']" >>= click
    untilFound (linkTo before)
    (findAll ui ".calendar-day-link" >>= traverse (`getAttribute` "data-date")) `shouldReturn` [Just (iso before)]
    -- The list, folded, holds every day, newest first.
    find ui ".calendar-all-days" >>= (`getAttribute` "open") >>= (`shouldBe` Nothing)
    (findAll ui ".calendar-all-days .calendar-day time" >>= traverse textContent) `shouldReturn` map dayWords [here, before]
    unsafeIOToPageM (js_calendar_today (toJSText ""))

  it "puts no month in a calendar's page left while the month's library loads, and logs no error" $ runPage $ do
    let day = fromGregorian 2026 11 27
    -- The load of the month's files is held once the page has asked for
    -- the month, as if the files were slow to come.
    unsafeIOToPageM js_calendar_hold
    (ui, _) <- startWritten m (writeCalendar [day]) (Just "user-admin") (Page (slugs ["team"]) (slugs ["days"]))
    find ui ".calendar-all-days" >>= (`getAttribute` "open") >>= (`shouldBe` Just "")
    unsafeIOToPageM js_calendar_held `shouldReturn` 1
    -- The list is open while the month is not drawn: a day of it is
    -- opened, and the calendar's page left.
    find ui ".calendar-day a" >>= click
    waitQuiet
    (find ui ".page-view h1" >>= textContent) `shouldReturn` iso day
    -- The files come now, for an element out of the page and dropped:
    -- nothing is put in it, nor anywhere else.
    unsafeIOToPageM js_calendar_release
    hosts `shouldReturn` [(False, 0)]
    unsafeIOToPageM js_calendar_months `shouldReturn` 0
    errors `shouldReturn` []

  it "draws the month when it is asked for again after its module failed to load" $ runPage $ do
    let day = fromGregorian 2026 11 27
    -- The first import of the month's module fails, as on a network that
    -- is down; the next imports go to the network.
    unsafeIOToPageM js_calendar_fail_first
    (ui, _) <- startWritten m (writeCalendar [day]) (Just "user-admin") (Page (slugs ["team"]) (slugs ["days"]))
    -- The island tells the page, which says so and keeps the list open.
    untilFound ".calendar-month-failed"
    find ui ".calendar-month-failed" >>= textContent >>= (`shouldBe` "The month could not be shown.")
    find ui ".calendar-all-days" >>= (`getAttribute` "open") >>= (`shouldBe` Just "")
    unsafeIOToPageM js_calendar_months `shouldReturn` 0
    -- The reader goes to a day and back: the month is asked for again,
    -- imported at another address, and drawn.
    find ui ".calendar-day a" >>= click
    waitQuiet
    (find ui ".page-view h1" >>= textContent) `shouldReturn` iso day
    unsafeIOToPageM js_history_back_heard
    waitQuiet
    untilAttribute ".calendar-all-days" "open" Nothing
    unsafeIOToPageM js_calendar_months `shouldReturn` 1
    length <$> findAll ui ".calendar-month-failed" `shouldReturn` 0
    imported <- fromMaybe [] . decodeStrict . encodeUtf8 . fromJSText <$> unsafeIOToPageM js_calendar_unfail
    imported `shouldBe` ["islands/calendar.js", "islands/calendar.js?attempt=2" :: Text]

  it "keeps the focus on a day's link in the month when the days change" $ runPage $ do
    let linkOf d = ".calendar-day-link[data-date='" <> iso d <> "']"
        (d12, d20, d27) = (fromGregorian 2026 11 12, fromGregorian 2026 11 20, fromGregorian 2026 11 27)
    unsafeIOToPageM (js_calendar_today (toJSText (iso d27)))
    wiki <- unsafeIOToPageM (newMemWiki ((demoState demoNow) {current = Just (UserId "user-admin")}))
    router <- unsafeIOToPageM testRouter
    unsafeIOToPageM (writeCalendar [d27, d12] wiki)
    before <- unsafeIOToPageM (calendarInput router wiki)
    ui <- mount m calendarSurface before
    untilAttribute ".calendar-all-days" "open" Nothing
    find ui (linkOf d27) >>= focus
    (fromJSText <$> unsafeIOToPageM js_calendar_focused_date) `shouldReturn` iso d27
    -- A day is added under the calendar, as in another tab: the month is
    -- told its new days, and draws its cells again.
    unsafeIOToPageM . runMemWiki wiki $ either (panic . show) (const ()) <$> createPage before.page.space (Just before.page.id) (iso d20) "A day."
    after <- unsafeIOToPageM (calendarInput router wiki)
    void (query ui (H.mkTell (SetInput after)))
    untilFound (linkOf d20)
    (fromJSText <$> unsafeIOToPageM js_calendar_focused_date) `shouldReturn` iso d27
    unsafeIOToPageM (js_calendar_today (toJSText ""))

  it "keeps the list of days open when the month is drawn while the focus is in it" $ runPage $ do
    let day = fromGregorian 2026 11 27
    unsafeIOToPageM js_calendar_hold
    (ui, _) <- startWritten m (writeCalendar [day]) (Just "user-admin") (Page (slugs ["team"]) (slugs ["days"]))
    -- While the month loads, the reader comes to a day in the open list.
    unsafeIOToPageM js_calendar_held `shouldReturn` 1
    find ui ".calendar-day a" >>= focus
    -- The month is drawn: the list stays open, the focus where it was.
    -- (Without the focus in it, the list folds: the walk above.)
    unsafeIOToPageM js_calendar_release
    waitQuiet
    unsafeIOToPageM js_calendar_months `shouldReturn` 1
    find ui ".calendar-all-days" >>= (`getAttribute` "open") >>= (`shouldBe` Just "")
    (fromJSText <$> unsafeIOToPageM js_calendar_focused) `shouldReturn` dayWords day

  it "makes a page a presentation, which is offered to be presented, and a document again" $ runPage $ do
    (ui, wiki) <- startWith m (Just "user-admin") (Page (slugs ["docs"]) (slugs ["markdown-guide", "diagrams"]))
    toggle <- find ui ".page-actions .page-kind"
    getAttribute toggle "aria-pressed" `shouldReturn` Just "false"
    length <$> findAll ui ".page-actions a.page-present" `shouldReturn` 0
    click toggle
    _ <- find ui ".page-actions a.page-present"
    find ui ".page-actions .page-kind" >>= (`getAttribute` "aria-pressed") >>= (`shouldBe` Just "true")
    (\p -> p.kind) <$> readPage wiki ["docs"] ["markdown-guide", "diagrams"] `shouldReturn` Kind.Presentation
    find ui ".page-actions .page-kind" >>= click
    eventually $ length <$> findAll ui ".page-actions a.page-present" `shouldReturn` 0
    (\p -> p.kind) <$> readPage wiki ["docs"] ["markdown-guide", "diagrams"] `shouldReturn` Kind.Document

  it "keeps a space's home page a document: it has no choice of kind" $ runPage $ do
    ui <- start m (Just "user-admin") (Space (slugs ["docs"]))
    _ <- find ui ".page-actions"
    length <$> findAll ui ".page-actions .page-kind" `shouldReturn` 0
  where
    -- The links marked as the section being read, in every list of the
    -- page's headings.
    current ui = do
      links <- findAll ui ".toc-list a[data-current]"
      ordNub . catMaybes <$> traverse (`getAttribute` "href") links
    -- Until jsbits/toc.js has marked the panel's link to a section as the
    -- one read.
    marked section = untilAttribute (".page-contents a[href='" <> section <> "']") "data-current" (Just "")
    scrollTo selector = unsafeIOToPageM (evaluate =<< js_scroll_into_view (toJSText selector))
    scrollTop y = unsafeIOToPageM (evaluate =<< js_scroll_to y)
    -- Where the first footnote is in the window: its top, its bottom and
    -- the window's height.
    noteBox = do
      found <- unsafeIOToPageM (fromJSText <$> js_box (toJSText ("#fn-1" :: Text)))
      case mapMaybe (readMaybe . T.unpack) (T.words found) of
        [top, bottom, window] -> pure (top, bottom, window :: Double)
        _ -> expectationFailure "no footnote" >> pure (0, 0, 0)
    chooseKind ui label = do
      choices <- findAll ui ".new-page-kind-choice"
      labelled <- filterM (fmap (T.isPrefixOf label . T.strip) . textContent) choices
      case labelled of
        c : _ -> findIn c "input" >>= click
        [] -> expectationFailure ("no kind says " <> toS label)
    -- The link to a page of the space in the sidebar's tree, by its title.
    treeLink ui title = do
      links <- findAll ui ".wiki-sidebar .page-tree a"
      named <- filterM (fmap ((== title) . T.strip) . textContent) links
      case named of
        l : _ -> pure l
        [] -> expectationFailure ("no page in the tree is called " <> toS title)
    chooseStart ui label = do
      choices <- findAll ui ".new-page-choice"
      labelled <- filterM (fmap (T.isPrefixOf label . T.strip) . textContent) choices
      case labelled of
        c : _ -> findIn c "input" >>= click
        [] -> expectationFailure ("no choice says " <> toS label)

readPage :: IORef MemState -> [Text] -> [Text] -> PageM s Page
readPage wiki space path =
  unsafeIOToPageM . runMemWiki wiki $ do
    s <- either (panic . show) identity <$> getSpace (slugs space)
    either (panic . show) identity <$> getPage s.id (slugs path)

-- | A calendar "Days" in the demo's team space, with a page under it for
-- each of these days, titled with its date.
writeCalendar :: [Day] -> IORef MemState -> IO ()
writeCalendar ds wiki = runMemWiki wiki $ do
  team <- either (panic . show) identity <$> getSpace (slugs ["team"])
  made <- either (panic . show) identity <$> createPageOf Kind.calendarKind team.id Nothing "Days" ""
  for_ ds $ \d -> either (panic . show) (const ()) <$> createPage team.id (Just made.id) (iso d) "A day."

-- | What the test tells the calendar's surface on its own
-- ('calendarSurface'): its input from now on.
data SurfaceQuery a = SetInput KindInput a

type SurfaceSlots = ("surface" .== H.Slot (Const Void) Void ())

-- | The calendar's surface on its own, with an input the test changes
-- ('SetInput'): a change of the pages under the calendar without the page
-- view around it, as a day added in another tab brings one.
calendarSurface :: forall m. (MonadUUID m, MonadIO m) => H.Component SurfaceQuery KindInput Void m
calendarSurface = case (calendar :: Plugin m).pageKind of
  Nothing -> panic "the calendar brings no kind of page"
  Just kindView ->
    H.mkComponent
      H.ComponentSpec
        { initialState = pure
        , render = \input -> HH.div_ [HH.slot_ "surface" () kindView.surface input]
        , eval = H.mkEval H.defaultEval {H.handleQuery = handleQuery}
        }
  where
    handleQuery :: SurfaceQuery a -> H.HalogenM KindInput Void SurfaceSlots Void m (Maybe a)
    handleQuery (SetInput input next) = put input $> Just next

-- | The surface's input for the calendar "Days" of the team's space, as the
-- page view gives it: the calendar, and the pages under it with links.
calendarInput :: Router Route -> IORef MemState -> IO KindInput
calendarInput router wiki = runMemWiki wiki $ do
  team <- either (panic . show) identity <$> getSpace (slugs ["team"])
  shown <- either (panic . show) identity <$> getPage team.id (slugs ["days"])
  listed <- either (panic . show) identity <$> listPages team.id
  pure
    KindInput
      { page = shown
      , under = [Child {page = c, link = Plugin.linkTo router (Page (slugs ["team"]) (slugs ["days"] <> [c.slug]))} | c <- listed, c.parent == Just shown.id]
      , options = Nothing
      , mayEdit = True
      }

-- | A day as the wiki writes a date: 2026-11-27.
iso :: Day -> Text
iso = toS . iso8601Show

-- | A day as the calendar's list names it: "Friday, 27 November 2026".
dayWords :: Day -> Text
dayWords = toS . formatTime defaultTimeLocale "%A, %-d %B %Y"

-- | The elements the held asks for a month were for: whether each is in
-- the page, and how many children it has.
hosts :: PageM s [(Bool, Int)]
hosts = fromMaybe [] . decodeStrict . encodeUtf8 . fromJSText <$> unsafeIOToPageM js_calendar_hosts

-- | The console's errors since the asks were held.
errors :: PageM s [Text]
errors = fromMaybe ["unreadable"] . decodeStrict . encodeUtf8 . fromJSText <$> unsafeIOToPageM js_calendar_errors
