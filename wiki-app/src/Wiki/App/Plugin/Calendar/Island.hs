{-# LANGUAGE TemplateHaskell #-}

-- | The calendar's month, an island: FullCalendar's month view, which the
-- page's islands put in an element the calendar's surface holds by ref
-- and never looks inside (@web/src/calendar.js@, loaded the first time a
-- calendar's page is shown). The bindings here only forward to the
-- page's islands (@jsbits/calendar.js@).
--
-- The island tells the element two things, in events: that the month is
-- drawn, or that its library could not be loaded (@wiki-island-drawn@, as
-- a diagram's element is told; 'drawnProblem'), and that the reader
-- clicked the number of a day that has a page, a link, with a plain click
-- (@wiki-calendar-day@; 'clickedDay'). The island decides which clicks
-- are plain as the application's own links do: the main button and no
-- modifier key. It prevents the default of those only, and leaves any
-- other click to the browser, which opens the address in a new tab or
-- window.
--
-- Natively there is no browser: 'showMonth' and 'dropMonth' do nothing,
-- and a native test dispatches the events itself, made from JSON.
module Wiki.App.Plugin.Calendar.Island
  ( showMonth
  , dropMonth
  , foldList
  , Marked (..)
  , drawnProblem
  , clickedDay
  )
where

import Data.Aeson (encode)
import Data.ByteString.Lazy qualified as BSL
import Data.Foreign (foreignToString, readProp)
import Data.Text qualified as T
import Data.Time (Day)
import Data.Time.Format.ISO8601 (iso8601ParseM, iso8601Show)
import Halogen.JSBits (JSText, Safety (..), browserJS, inBrowser, isNull, jsValText, toJSText)
import Protolude
-- With its constructor: the FFI passes the value inside the newtype,
-- which it can only see through a constructor in scope.
import Web.DOM.Internal.Types (Element (..))
import Web.Event.CustomEvent (CustomEvent (..))
import Web.Event.Event (Event (..))

$( browserJS
     ["jsbits/calendar.js"]
     [ ("js_show", "wiki_calendar_show", Safe, [t|Element -> JSText -> IO ()|])
     , ("js_drop", "wiki_calendar_drop", Safe, [t|Element -> IO ()|])
     , ("js_fold", "wiki_calendar_fold", Safe, [t|Element -> IO ()|])
     ]
 )

-- | Show the month in the element, with these days marked as having a
-- page, each a link to its address, named with its name. The first call for an element starts the load of the month's
-- library and puts the month in once it has loaded, if the element is
-- still in the page and nobody dropped the month meanwhile. A later call
-- changes the days marked, and puts no second month in. It returns at
-- once: call it in @H.sync@.
showMonth :: (MonadIO m) => Element -> [Marked] -> m ()
showMonth host days = liftIO (js_show host (toJSText (decodeUtf8 (BSL.toStrict (encode [(iso8601Show m.day, m.address, m.name) | m <- days])))))

-- | 'Marked' is a day the month marks as having a page: its date, the
-- address of its page ('Wiki.App.Plugin.linkAddress'), and its name in
-- words, which names the link for a screen reader.
data Marked = Marked
  { day :: Day
  , address :: Text
  , name :: Text
  }
  deriving stock (Eq, Show)

-- | Fold the list of days, unless the focus is inside it: in the browser,
-- one call that checks and folds. Natively it does nothing. Call it in
-- @H.sync@.
foldList :: (MonadIO m) => Element -> m ()
foldList listed = liftIO (js_fold listed)

-- | Take the month out of the element, or forget that it was wanted
-- there while its library loads. Call it in @H.sync@.
dropMonth :: (MonadIO m) => Element -> m ()
dropMonth host = liftIO (js_drop host)

-- | What a @wiki-island-drawn@ event says: the library's error when the
-- month could not be shown, or nothing when it was drawn (the event's
-- detail is then empty).
drawnProblem :: Event -> Maybe Text
drawnProblem event = let message = detailText event in if T.null message then Nothing else Just message

-- | The day a @wiki-calendar-day@ event names: the day whose number the
-- reader clicked.
clickedDay :: Event -> Maybe Day
clickedDay = iso8601ParseM . toS . detailText

-- | The text an event of the island's carries as its detail; empty when it
-- carries none. Natively the event is made from JSON by a test, and its
-- detail is read from that.
detailText :: Event -> Text
detailText event@(Event value)
  | inBrowser = let detail = (CustomEvent event).detail in if isNull detail then "" else jsValText detail
  | otherwise = fromMaybe "" (readProp "detail" (Just . foreignToString) value)
