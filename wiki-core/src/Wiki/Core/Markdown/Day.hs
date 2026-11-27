-- | A day's entries: the items of a list on a day's page, each read as an
-- entry at a time, an entry for the whole day, or either one highlighted
-- (WP5.92 of docs/PLAN.md).
--
-- A day's page is an ordinary page whose title is the date
-- ('dayOfTitle'). Its list looks like this:
--
-- > - 09:00 Standup
-- > - 9:30-10:30 Review of the plan
-- > - ! 14:00 Demo for the client
-- > - ! Ania's birthday
-- > - [ ] 16:00 Call the bank
-- > - [x] ! Pay the invoice
-- > - Buy milk
--
-- The parser makes three lists of this one, cut where the items with a
-- task box start and end; 'dayEntries' says how to read them as one.
--
-- Each item is one entry ('dayEntries'). An item is read from its start,
-- in this order: the task box, which the parser has already taken off
-- (@[ ]@ or @[x]@, as in "Wiki.Core.Markdown.Tasks"); then @!@ and a
-- space (or the end of the item's text), which highlights the entry; then
-- a time and a space (or the end of the item's text): @09:00@ is an entry
-- at nine, @9:30-10:30@ an entry from half past nine to half past ten.
-- What is left is the entry's label. An item without a time is an entry
-- for the whole day.
--
-- The item's text is the text of its first paragraph. Display math
-- written in an item ends that paragraph: the parser makes the math a
-- block of its own. So in @- 09:00$$x$$@ the paragraph is @09:00@ alone,
-- and the item is an entry at nine, as @- 09:00 $$x$$@ is. In the same
-- way @- ! $$x$$@ and @- !$$x$$@ are highlighted entries.
--
-- The rule is narrow on purpose. Something at the start that is not
-- exactly a mark or a time (@25:00@, @9:5@, @10:30-9:30@, @!09:00@, a time
-- in inline code or in a link's text) is not read at all: the item is an
-- entry for the whole day, with its text as written for a label. An
-- unusual item is shown as the author wrote it, never at a wrong time.
--
-- The source is not changed: the entries are a reading of the list. The
-- page view shows them in the order of their times, under the parts of
-- the day ('DayPart').
module Wiki.Core.Markdown.Day
  ( -- * Entries
    DayEntry (..)
  , EntryTime (..)
  , dayEntries

    -- * Parts of the day
  , DayPart (..)
  , dayPart
  , entryPart

    -- * The date of a day's page
  , dayOfTitle
  )
where

import Data.Text qualified as T
import Data.Time (Day, TimeOfDay (..), makeTimeOfDayValid)
import Data.Time.Format.ISO8601 (iso8601ParseM)
import Protolude
import Wiki.Core.Markdown.Ast (Block (..), Inline (..), ListItem (..), plainText)

-- | 'DayEntry' is one item of a day's list, read as an entry of the day.
data DayEntry = DayEntry
  { place :: Int
  -- ^ The item's place among the items 'dayEntries' was given, from 0,
  -- in the order of the source.
  , task :: Maybe Bool
  -- ^ The item's task box as the parser gives it: none, to do ('False')
  -- or done ('True').
  , highlight :: Bool
  -- ^ Whether the item starts with @!@, after its box, and the @!@ is
  -- followed by a space or is the end of the item's first paragraph.
  , time :: EntryTime
  , label :: Text
  -- ^ The plain text of the rest of the item's first paragraph, after
  -- the mark and the time, without white space at either end. An item
  -- whose first block is not a paragraph (a code block, a list) has an
  -- empty label.
  }
  deriving stock (Eq, Show)

-- | 'EntryTime' is when an entry is: the whole day, a start, or a start
-- and an end. An end is always after its start, on the same day.
data EntryTime
  = AllDay
  | At TimeOfDay
  | Span TimeOfDay TimeOfDay
  deriving stock (Eq, Show)

-- | The entries of a list's items: one for each item at the top level of
-- the list, in the order of the source. An item's nested lists are part
-- of the item, not entries of their own. Which lists of a page are a
-- day's lists is for the caller to decide.
--
-- The parser cuts a list in two where an item with a task box and an item
-- without one meet, so a day's list with tasks in it is several lists in
-- the tree, one after another. A caller that reads such a list as one
-- gives this function the items of those lists together.
dayEntries :: [ListItem] -> [DayEntry]
dayEntries = zipWith entryOf [0 ..]

-- | The entry of one item, at its place in the list.
entryOf :: Int -> ListItem -> DayEntry
entryOf place (ListItem task blocks) = case blocks of
  Para xs : _ -> fromInlines xs
  Plain xs : _ -> fromInlines xs
  _ -> DayEntry {place, task, highlight = False, time = AllDay, label = ""}
  where
    fromInlines xs =
      let (highlight, afterMark) = markOf xs
          (time, rest) = timeOf afterMark
       in DayEntry {place, task, highlight, time, label = T.strip (plainText rest)}

-- | Whether the inlines start with the mark, and the inlines after it.
-- The mark is @!@ at the start of the first inline, which must be text: a
-- @!@ in code or emphasis is no mark. The @!@ must be followed by a space
-- or be the end of the item's first paragraph, as a time must. So the
-- item @- !@ is a highlighted entry for the whole day with an empty label.
-- A @!@ followed directly by other text (@!x@), by code or by an emoji is
-- no mark.
markOf :: [Inline] -> (Bool, [Inline])
markOf = \case
  Str t : rest | Just after <- T.stripPrefix "! " t -> (True, Str after : rest)
  [Str "!"] -> (True, [])
  xs -> (False, xs)

-- | The time at the start of the inlines, and the inlines after it. The
-- time is the first word of the first inline, which must be text, and the
-- word must be followed by a space or be all that is left of the item's
-- text. So a time followed directly by an emoji, by code or by a line
-- break is no time. Spaces before the word (after the mark) are skipped.
timeOf :: [Inline] -> (EntryTime, [Inline])
timeOf xs = case xs of
  Str t : rest
    | (word, after) <- T.break (== ' ') (T.dropWhile (== ' ') t)
    , not (T.null after) || null rest
    , Just time <- readTime word ->
        (time, Str after : rest)
  _ -> (AllDay, xs)

-- | A word read as a time: @H:MM@ or @HH:MM@, or two of them joined by a
-- hyphen or an en dash, without spaces, the second after the first. The
-- parser leaves both dashes as written: it makes no smart punctuation,
-- and @:00-10:@ is not the name of an emoji. Anything else is no time.
readTime :: Text -> Maybe EntryTime
readTime word = case T.split (\c -> c == '-' || c == '\x2013') word of
  [single] -> At <$> clock single
  [startWord, endWord] -> do
    start <- clock startWord
    end <- clock endWord
    guard (end > start)
    Just (Span start end)
  _ -> Nothing

-- | @H:MM@ or @HH:MM@ as a time of day: hours 0 to 23, minutes 00 to 59,
-- in ASCII digits.
clock :: Text -> Maybe TimeOfDay
clock t = case T.splitOn ":" t of
  [h, m]
    | T.length h `elem` [1, 2]
    , T.length m == 2
    , T.all isDigit h
    , T.all isDigit m -> do
        hours <- readMaybe (T.unpack h)
        minutes <- readMaybe (T.unpack m)
        makeTimeOfDayValid hours minutes 0
  _ -> Nothing

-- | 'DayPart' is one of the parts of the day the page view groups timed
-- entries under (the owner's): Dawn from 6 to 9, Morning from 9 to 12,
-- Noon from 12 to 15, Afternoon from 15 to 18, Evening from 18 to 21, and
-- Night from 21 to 6. A part starts at its first hour and ends where the
-- next one starts: 9:00 is Morning, 8:59 is Dawn. The constructors are in
-- the order the view shows them.
data DayPart = Dawn | Morning | Noon | Afternoon | Evening | Night
  deriving stock (Eq, Ord, Show, Enum, Bounded)

-- | The part of the day a time is in.
dayPart :: TimeOfDay -> DayPart
dayPart t
  | hour < 6 = Night
  | hour < 9 = Dawn
  | hour < 12 = Morning
  | hour < 15 = Noon
  | hour < 18 = Afternoon
  | hour < 21 = Evening
  | otherwise = Night
  where
    hour = t.todHour

-- | The part of the day an entry is in: the part of its start. None for
-- an entry for the whole day.
entryPart :: DayEntry -> Maybe DayPart
entryPart entry = case entry.time of
  AllDay -> Nothing
  At start -> Just (dayPart start)
  Span start _ -> Just (dayPart start)

-- | The date a page's title names, when the title is exactly an ISO date,
-- @2026-11-27@, with white space around it allowed: four digits, a
-- hyphen, two digits, a hyphen, two digits, and a real date (not
-- @2026-02-30@). A title with any other text in it names no date.
dayOfTitle :: Text -> Maybe Day
dayOfTitle title = do
  guard (T.length t == 10 && and (zipWith fits [0 :: Int ..] (T.unpack t)))
  iso8601ParseM (T.unpack t)
  where
    t = T.strip title
    fits i c
      | i == 4 || i == 7 = c == '-'
      | otherwise = isDigit c
