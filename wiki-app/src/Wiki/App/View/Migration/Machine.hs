-- | 'Model' is what the shell knows of the move of the writing
-- assistant's legacy settings for the reader ("Wiki.App.Assistant.Migrate"),
-- and 'step' the one place it changes (decision 82 of docs/PLAN.md).
--
-- = When a run is asked
--
-- A run is asked for the reader when the shell is given a session
-- ('Given'), and each time the secrets change in this browser ('Nudged':
-- the vault unlocked or locked, a secret kept or forgotten, here or in
-- another tab), until a run finds nothing legacy left ('done'). A run
-- that waits (for an unlock, for the server's options) is asked again at
-- the next change. A new page asks again at its start, so that a legacy
-- item an earlier tab added again is moved.
--
-- = One run at a time
--
-- One run is out at a time ('out'). A run wanted while one is out is asked
-- when that one answers ('wanted'), failed or not: two runs never read and
-- write the same places at once in this page.
--
-- = The reader
--
-- The session's number and the reader change in the step that is given
-- them, before any wait. A run asked in an earlier session answers into
-- nothing: its options are not adopted ('Adopt' carries the session it was
-- found in), and a run for the new reader is asked once it answers.
module Wiki.App.View.Migration.Machine
  ( Model (..)
  , Event (..)
  , Effect (..)
  , initial
  , step
  )
where

import Data.Aeson (Value)
import Protolude hiding (reader)
import Wiki.App.Assistant.Migrate (Outcome (..))
import Wiki.Core.Id (UserId)

-- | 'Model' is the move's state, for the shell.
data Model = Model
  { session :: Int
  -- ^ The number of the session last given.
  , reader :: Maybe UserId
  -- ^ Who is signed in, in that session.
  , out :: Maybe (Int, Int)
  -- ^ The run out, by its number, with the session it was asked in.
  , wanted :: Bool
  -- ^ Whether a run is wanted after the one out.
  , next :: Int
  -- ^ The next run's number.
  , done :: Bool
  -- ^ Whether a run of this session found nothing legacy left.
  , problem :: Maybe Text
  -- ^ Why the last run of this session failed, until one does not.
  }
  deriving stock (Eq, Show)

-- | 'Event' is what happens to the move.
data Event
  = -- | The shell was given the session of this number, and its reader.
    Given Int (Maybe UserId)
  | -- | The secrets changed in this browser.
    Nudged
  | -- | The answer of the run of this number.
    Answered Int (Either Text Outcome)
  deriving stock (Eq, Show)

-- | 'Effect' is what a step asks the shell to do.
data Effect
  = -- | Run the move for the reader; the answer comes back as 'Answered'.
    Run Int UserId
  | -- | Give these options of the assistant to the plugins' state, found in
    -- the session of this number.
    AdoptOptions Int Value
  deriving stock (Eq, Show)

-- | Before any session.
initial :: Model
initial = Model {session = -1, reader = Nothing, out = Nothing, wanted = False, next = 0, done = False, problem = Nothing}

-- | The state after an event, and what it asks to be done.
step :: Event -> Model -> (Model, [Effect])
step event m = case event of
  Given session reader
    | session == m.session && reader == m.reader -> (m, [])
    | otherwise -> asking m {session, reader, done = False, problem = Nothing}
  Nudged
    | m.done -> (m, [])
    | otherwise -> asking m
  Answered n answer -> case m.out of
    Just (out, askedIn)
      | out == n ->
          let (m1, effects) = if askedIn == m.session then answered askedIn answer m {out = Nothing} else (m {out = Nothing}, [])
           in if m1.wanted then second (effects <>) (asking m1 {wanted = False}) else (m1, effects)
    _ -> (m, [])

-- | What a run's answer changes, for the session it was asked in.
answered :: Int -> Either Text Outcome -> Model -> (Model, [Effect])
answered session answer m = case answer of
  Right Moved -> (m {done = True, problem = Nothing}, [])
  Right Waiting -> (m {problem = Nothing}, [])
  Right (Adopt v) -> (m {problem = Nothing}, [AdoptOptions session v])
  Left why -> (m {problem = Just why}, [])

-- | A run asked for the reader, or wanted after the one out.
asking :: Model -> (Model, [Effect])
asking m = case m.reader of
  Nothing -> (m, [])
  Just u
    | isJust m.out -> (m {wanted = True}, [])
    | otherwise -> (m {out = Just (m.next, m.session), next = m.next + 1}, [Run m.next u])
