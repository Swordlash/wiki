{-# LANGUAGE QuasiQuotes #-}

-- | The calendar, a built-in plugin ("Wiki.App.Plugin"; WP5.93 of
-- docs/PLAN.md, issue #204): a kind of page, "calendar", whose page lists
-- its days. A day is a page directly under the calendar whose title is a
-- date ("Wiki.Core.Markdown.Day"'s 'dayOfTitle'), such as a page made
-- from the template "Day". The page view shows the calendar's own text as
-- a document's, and the list of days after it ('days').
--
-- The list is the calendar's first view: the days newest first, each a
-- link to its page. It reads only the page view's list of the pages under
-- the calendar, so it works in an encrypted space as in a plain one: the
-- titles there are decrypted in this browser. The views by month, week and
-- day, which read each day's entries, come later.
--
-- With the plugin off, "New page" offers no calendar, and a calendar's
-- page is shown as a document.
module Wiki.App.Plugin.Calendar
  ( calendar
  , calendarId
  , days
  , dayName
  )
where

import Data.Row (Empty)
import Data.Time (Day, defaultTimeLocale, formatTime)
import Data.Time.Format.ISO8601 (iso8601Show)
import Halogen qualified as H
import Halogen.TH (hh)
import Protolude
import Wiki.App.Plugin (Child (..), KindInput (..), KindView (..), Link, LinkClick, Manifest (..), Plugin (..), PluginId, attr, builtin, followLink, linkProps)
import Wiki.Core.Markdown.Day (dayOfTitle)
import Wiki.Core.Page (PageSummary (..))
import Wiki.Core.PageKind (calendarKind)

-- | The plugin.
calendar :: (MonadIO m) => Plugin m
calendar =
  Plugin
    { manifest
    , diagnostics = Nothing
    , agent = Nothing
    , pageKind = Just KindView {kind = calendarKind, label = "Calendar", hint = "Its days, the pages under it titled with a date.", surface}
    , options = Nothing
    }

-- | The plugin's id.
calendarId :: PluginId
calendarId = manifest.id

manifest :: Manifest
manifest = builtin "calendar" "Calendar" "A kind of page that lists its days: the pages under it titled with a date, as the template “Day” makes them."

-- | The days among the pages under a calendar, newest first: each page
-- whose title is a date ('dayOfTitle'), with its date. A template is not a
-- day, whatever its title. Two pages of one date keep the space's order.
days :: [Child] -> [(Day, Child)]
days under = sortOn (Down . fst) [(d, c) | c <- under, not c.page.template, Just d <- [dayOfTitle c.page.title]]

-- | A day as the list names it: "Friday, 27 November 2026".
dayName :: Day -> Text
dayName = toS . formatTime defaultTimeLocale "%A, %-d %B %Y"

data Action = Receive KindInput | Followed Link LinkClick

-- | The list of the calendar's days, after the calendar's text.
surface :: forall m. (MonadIO m) => H.Component (Const Void) KindInput Void m
surface =
  H.mkComponent
    H.ComponentSpec
      { initialState = pure
      , render
      , eval = H.mkEval H.defaultEval {H.handleAction = handleAction, H.receive = Just . Receive}
      }
  where
    render :: KindInput -> H.ComponentHTML Action Empty m
    render input =
      [hh|
        <section class="calendar-days" aria-label="Days">
          <h2 class="calendar-days-heading">Days</h2>
          {{ listed }}
        </section>
      |]
      where
        listed = case days input.under of
          []
            | input.mayEdit -> [hh|<p class="calendar-empty">No days yet. To add one, make a page under this calendar titled with its date, such as 2026-11-27, from the template “Day”.</p>|]
            | otherwise -> [hh|<p class="calendar-empty">No days yet.</p>|]
          ds ->
            [hh|
              <ul class="calendar-day-list">
                {% for (d, c) in ds %}
                  <li class="calendar-day"><a {{ linkProps c.link (Followed c.link) }}><time {{ [attr "datetime" (toS (iso8601Show d))] }}>{{ dayName d }}</time></a></li>
                {% end %}
              </ul>
            |]

    handleAction :: Action -> H.HalogenM KindInput Action Empty Void m ()
    handleAction = \case
      Receive input -> put input
      Followed l click -> followLink l click
