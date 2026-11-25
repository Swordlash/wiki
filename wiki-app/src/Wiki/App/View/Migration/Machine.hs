-- | 'Model' is what the shell knows of the move of the writing
-- assistant's legacy settings for the reader ("Wiki.App.Assistant.Migrate"),
-- and 'step' the one place it changes (decision 82 of docs/PLAN.md).
--
-- = When a run is asked
--
-- A run is asked for the reader when the shell is given a session
-- ('Given'), and each time the secrets change in this browser ('Nudged':
-- the vault unlocked or locked, a secret kept or forgotten, here or in
-- another tab), until a run finds nothing legacy left ('done'). The run
-- at the start of a session looks at this browser only, and asks the
-- server nothing when it keeps nothing legacy; the runs after a change of
-- the secrets look at the vault too ('vaultToo'). A run that waits for an
-- unlock is asked again at the next change. A run that ends with options
-- to adopt ('adopting'), which it does too while the server's options are
-- not known, is asked again when what the plugins' state knows of the
-- assistant's options changes ('OptionsLearnt': the plugins read, the
-- adopted options written, or refused because the server had some): the
-- legacy places are then emptied with nothing else needed, and so is a run
-- out when they change, which read them before. A change of the options
-- asks nothing when no run waits for them. A new page asks again at its start,
-- so that a legacy item an earlier tab added again is moved.
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
  , optionsLearnt
  )
where

import Data.Aeson (Value)
import Protolude hiding (reader)
import Wiki.App.Assistant.Migrate (Outcome (..))
import Wiki.App.Plugin.Registry (assistantId, optionsFor)
import Wiki.App.View.Plugins.Machine qualified as Plugins
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
  , vaultToo :: Bool
  -- ^ Whether runs look at the vault when this browser keeps nothing
  -- legacy: from the first change of the secrets in the session on.
  , problem :: Maybe Text
  -- ^ Why the last run of this session failed, until one does not.
  , adopting :: Bool
  -- ^ Whether the last run of this session ended with options to adopt:
  -- the move waits for the server's options.
  }
  deriving stock (Eq, Show)

-- | 'Event' is what happens to the move.
data Event
  = -- | The shell was given the session of this number, and its reader.
    Given Int (Maybe UserId)
  | -- | The secrets changed in this browser.
    Nudged
  | -- | What the plugins' state knows of the assistant's options changed
    -- ('optionsLearnt').
    OptionsLearnt
  | -- | The answer of the run of this number.
    Answered Int (Either Text Outcome)
  deriving stock (Eq, Show)

-- | 'Effect' is what a step asks the shell to do.
data Effect
  = -- | Run the move for the reader, looking at the vault even when this
    -- browser keeps nothing legacy, or not; the answer comes back as
    -- 'Answered'.
    Run Int UserId Bool
  | -- | Give these options of the assistant to the plugins' state, found in
    -- the session of this number.
    AdoptOptions Int Value
  deriving stock (Eq, Show)

-- | Before any session.
initial :: Model
initial = Model {session = -1, reader = Nothing, out = Nothing, wanted = False, next = 0, done = False, problem = Nothing, vaultToo = False, adopting = False}

-- | The state after an event, and what it asks to be done.
step :: Event -> Model -> (Model, [Effect])
step event m = case event of
  Given session reader
    | session == m.session && reader == m.reader -> (m, [])
    | otherwise -> asking m {session, reader, done = False, problem = Nothing, vaultToo = False, adopting = False}
  Nudged
    | m.done -> (m, [])
    | otherwise -> asking m {vaultToo = True}
  -- Whether the run looks at the vault stays as it was: the options do
  -- not say that this browser keeps anything legacy.
  -- A run out read the options before they changed, and may end with
  -- options to adopt: it is run again once it answers.
  OptionsLearnt
    | m.done -> (m, [])
    | m.adopting || isJust m.out -> asking m
    | otherwise -> (m, [])
  Answered n answer -> case m.out of
    Just (out, askedIn)
      | out == n ->
          let (m1, effects) = if askedIn == m.session then answered askedIn answer m {out = Nothing} else (m {out = Nothing}, [])
           in if m1.wanted then second (effects <>) (asking m1 {wanted = False}) else (m1, effects)
    _ -> (m, [])

-- | What a run's answer changes, for the session it was asked in.
answered :: Int -> Either Text Outcome -> Model -> (Model, [Effect])
answered session answer m = case answer of
  Right Moved -> (m {done = True, problem = Nothing, adopting = False}, [])
  Right NothingHere -> (m {problem = Nothing, adopting = False}, [])
  Right Waiting -> (m {problem = Nothing, adopting = False}, [])
  Right (Adopt v) -> (m {problem = Nothing, adopting = True}, [AdoptOptions session v])
  Left why -> (m {problem = Just why, adopting = False}, [])

-- | Whether a step of the plugins' state changed what it knows of the
-- assistant's options as the server keeps them: the shell then steps
-- 'OptionsLearnt'. Only a change: a write that keeps failing changes
-- nothing, and asks no run.
optionsLearnt :: Plugins.Model -> Plugins.Model -> Bool
optionsLearnt before after = known before /= known after
  where
    known m = optionsFor assistantId <$> Plugins.knownNow m

-- | A run asked for the reader, or wanted after the one out.
asking :: Model -> (Model, [Effect])
asking m = case m.reader of
  Nothing -> (m, [])
  Just u
    | isJust m.out -> (m {wanted = True}, [])
    | otherwise -> (m {out = Just (m.next, m.session), next = m.next + 1}, [Run m.next u m.vaultToo])
