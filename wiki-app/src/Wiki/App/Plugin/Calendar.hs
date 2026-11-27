{-# LANGUAGE QuasiQuotes #-}

-- | The calendar, a built-in plugin ("Wiki.App.Plugin"; WP5.93 of
-- docs/PLAN.md, issue #204): a kind of page, "calendar", whose page shows
-- its days. A day is a page directly under the calendar whose title is a
-- date ("Wiki.Core.Markdown.Day"'s 'dayOfTitle'), such as a page made
-- from the template "Day". The page view shows the calendar's own text as
-- a document's, and the calendar's surface after it.
--
-- The surface is a month, drawn by FullCalendar in the browser
-- ("Wiki.App.Plugin.Calendar.Island"), with the list of the days under it.
-- In the month, the number of each day that has a page is a link to the
-- day's page: a plain click opens it in this tab, through the router, and
-- any other is the browser's. The list names every day, newest first, each a
-- link to its page. It is folded in a @\<details>@ once the month is drawn,
-- unless the reader has the focus in it then, and open before: on the
-- page the server rendered, while the month's library loads, and when the
-- library cannot be loaded. It is also the keyboard's short way to a day.
--
-- Both read only the page view's list of the pages under the calendar, so
-- they work in an encrypted space as in a plain one: the titles there are
-- decrypted in this browser. The entries of each day come later.
--
-- With the plugin off, "New page" offers no calendar, and a calendar's
-- page is shown as a document.
module Wiki.App.Plugin.Calendar
  ( calendar
  , calendarId
  , days
  , dayName
  , dayLink
  , marked
  )
where

import Control.Monad.UUID (MonadUUID)
import Data.List (lookup)
import Data.List.NonEmpty qualified as NE
import Data.Row (Empty)
import Data.Time (Day, defaultTimeLocale, formatTime)
import Data.Time.Format.ISO8601 (iso8601Show)
import Halogen qualified as H
import Halogen.HTML.Events qualified as HE
import Halogen.TH (hh)
import Protolude hiding (State)
import Web.DOM.Internal.Types (Element)
import Web.Event.Event (EventType (..))
import Wiki.App.Plugin (Child (..), KindInput (..), KindView (..), Link, LinkClick, Manifest (..), Plugin (..), PluginId, attr, builtin, followLink, linkAddress, linkProps, openLink)
import Wiki.App.Plugin.Calendar.Island (Marked (..), clickedDay, drawnProblem, dropMonth, foldList, showMonth)
import Wiki.Core.Markdown.Day (dayOfTitle)
import Wiki.Core.Page (PageSummary (..))
import Wiki.Core.PageKind (calendarKind)

-- | The plugin.
calendar :: (MonadUUID m, MonadIO m) => Plugin m
calendar =
  Plugin
    { manifest
    , diagnostics = Nothing
    , agent = Nothing
    , pageKind = Just KindView {kind = calendarKind, label = "Calendar", hint = "A month with its days, the pages under it titled with a date.", surface}
    , options = Nothing
    }

-- | The plugin's id.
calendarId :: PluginId
calendarId = manifest.id

manifest :: Manifest
manifest = builtin "calendar" "Calendar" "A kind of page that shows a month with its days: the pages under it titled with a date, as the template “Day” makes them."

-- | The days among the pages under a calendar, newest first: each page
-- whose title is a date ('dayOfTitle'), with its date. A template is not a
-- day, whatever its title. Two pages of one date keep the space's order.
days :: [Child] -> [(Day, Child)]
days under = sortOn (Down . fst) [(d, c) | c <- under, not c.page.template, Just d <- [dayOfTitle c.page.title]]

-- | The link to a day's page, among the pages under a calendar: of the
-- first page of that date, in the order of 'days'. 'Nothing' for a day
-- that has no page.
dayLink :: [Child] -> Day -> Maybe Link
dayLink under d = (\c -> c.link) <$> lookup d (days under)

-- | A day as the list names it: "Friday, 27 November 2026".
dayName :: Day -> Text
dayName = toS . formatTime defaultTimeLocale "%A, %-d %B %Y"

-- | The days the month marks: each date that has a page, once, with the
-- address of its first page in the order of 'days' (the page 'dayLink'
-- opens) and its name. One pass over the sorted days, in which the pages
-- of one date are next to each other.
marked :: [Child] -> [Marked]
marked under = [Marked {day = d, address = linkAddress c.link, name = dayName d} | (d, c) NE.:| _ <- NE.groupWith fst (days under)]

data State = State
  { input :: KindInput
  , host :: H.RefLabel
  -- ^ The element the month is drawn in, which no render looks inside.
  , allDays :: H.RefLabel
  -- ^ The list of days, a @\<details>@, which the month folds once drawn
  -- ('foldList').
  , shown :: Maybe Element
  -- ^ The element the month was asked into ('showMonth'), until the
  -- surface goes: 'Finalize' takes the month out of it.
  , marks :: [Marked]
  -- ^ The days the month was last told ('marked'), so that a change of
  -- input that changes none of them tells it nothing.
  , month :: Month
  }

-- | Where the month is: not drawn yet (the page the server rendered, the
-- library loading), drawn, or not to be drawn, its library having failed
-- to load.
data Month = Waiting | Drawn | Failed
  deriving stock (Eq)

data Action
  = Initialize
  | Receive KindInput
  | MonthDrawn (Maybe Text)
  | DayClicked Day
  | Followed Link LinkClick
  | Finalize

-- | The month, and the list of the calendar's days under it. It asks
-- nothing, so it is a plain handler of its input and of the month's
-- events; it becomes a machine when it reads the days' pages for their
-- entries (WP5.93's next package, 4b).
surface :: forall m. (MonadUUID m, MonadIO m) => H.Component (Const Void) KindInput Void m
surface =
  H.mkComponent
    H.ComponentSpec
      { initialState = \input -> do
          host <- H.newRefLabel "calendar-month"
          allDays <- H.newRefLabel "calendar-all-days"
          pure State {input, host, allDays, shown = Nothing, marks = marked input.under, month = Waiting}
      , render
      , eval =
          H.mkEval
            H.defaultEval
              { H.handleAction = handleAction
              , H.receive = Just . Receive
              , H.initialize = Just Initialize
              , H.finalize = Just Finalize
              }
      }
  where
    render :: State -> H.ComponentHTML Action Empty m
    render st =
      [hh|
        <section class="calendar-days" aria-label="Days">
          <div class="calendar-month" ref={{ st.host }} {{ HE.handler' (EventType "wiki-island-drawn") (Just . MonthDrawn . drawnProblem) }} {{ HE.handler' (EventType "wiki-calendar-day") (fmap DayClicked . clickedDay) }}></div>
          {% if st.month == Failed %}
            <p class="calendar-month-failed">The month could not be shown.</p>
          {% end %}
          {{ listed }}
        </section>
      |]
      where
        listed = case days st.input.under of
          []
            | st.input.mayEdit -> [hh|<p class="calendar-empty">No days yet. To add one, make a page under this calendar titled with its date, such as 2026-11-27, from the template “Day”.</p>|]
            | otherwise -> [hh|<p class="calendar-empty">No days yet.</p>|]
          ds ->
            [hh|
              <details class="calendar-all-days" open ref={{ st.allDays }}>
                <summary>All days ({{ show (length ds) :: Text }})</summary>
                <ul class="calendar-day-list">
                  {% for (d, c) in ds %}
                    <li class="calendar-day"><a {{ linkProps c.link (Followed c.link) }}><time {{ [attr "datetime" (toS (iso8601Show d))] }}>{{ dayName d }}</time></a></li>
                  {% end %}
                </ul>
              </details>
            |]

    handleAction :: Action -> H.HalogenM State Action Empty Void m ()
    handleAction = \case
      -- The month is asked for in the browser once the element is there,
      -- and the element is kept in the same step, so that 'Finalize' takes
      -- out whatever the ask puts in. Natively the ask does nothing.
      Initialize -> do
        st <- get
        H.getRef st.host >>= traverse_ (\el -> H.sync (showMonth el st.marks) >> modify (\s -> s {shown = Just el}))
      -- The page view gives its input at each of its renders: taken, and
      -- the list drawn again, only when it changed, since a calendar of
      -- years of days is a long list. A change of the pages under the
      -- calendar (a day added, in this tab or another) that changes the
      -- days the month marks is told to the month, which is not put in
      -- again.
      Receive input -> do
        st <- get
        unless (input == st.input) $ do
          let marks = marked input.under
          put st {input, marks}
          when (marks /= st.marks) $
            for_ st.shown (\el -> H.sync (showMonth el marks))
      -- Drawn, the month folds the list, unless the reader has the focus
      -- in it: the check and the fold are one call of the browser's, made
      -- in this step. The list is open in every render, so no render
      -- opens or folds it again.
      MonthDrawn Nothing -> do
        modify (\st -> st {month = Drawn})
        gets (\st -> st.allDays) >>= H.getRef >>= traverse_ (H.sync . foldList)
      MonthDrawn (Just _) -> modify (\st -> st {month = Failed})
      DayClicked d -> gets (\st -> dayLink st.input.under d) >>= traverse_ openLink
      Followed l click -> followLink l click
      Finalize -> gets (\st -> st.shown) >>= traverse_ (H.sync . dropMonth)
