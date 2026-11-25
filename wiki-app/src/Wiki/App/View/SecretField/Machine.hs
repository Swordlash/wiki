-- | 'Model' is what the host's field for one of a plugin's secrets knows
-- ("Wiki.App.View.SecretField"), and 'step' the one place it changes
-- (decision 82 of docs/PLAN.md). The field shows whether the secret is
-- set, takes a new value and keeps it after the plugin's check, forgets
-- it, and unlocks the user's vault where it is locked in this browser.
--
-- = One ask at a time
--
-- Each of these is an ask of the host ('Job'): reading whether the secret
-- is set, the plugin's check of a value, keeping it, forgetting it, and
-- the unlock. One is out at a time ('doing'), so that the vault sees
-- them in the order the reader made them, and a later answer is never an
-- older one. The buttons are off while one is out, and a press that comes
-- all the same is dropped. A read wanted while an ask is out is asked when
-- that ask answers ('wanted').
--
-- A value is kept in two asks: the plugin's check, and, when the check
-- passes, the keep. The check answers the value to keep, as the plugin
-- makes it from the text typed (the assistant trims the spaces around a
-- key), and the keep's ask carries that value: what is kept is what was
-- checked, not the text typed.
--
-- = What the check was for
--
-- A plugin's check can depend on more than the value: the assistant tries
-- an API key against one service, at one address. The plugin names that
-- in its input to the field ('binding'), and the field takes a new input
-- in the step that hears of it ('Rebound'). The check's ask carries the
-- binding it was asked under, and the step that takes its answer keeps the
-- value only if the binding is still that one. Otherwise nothing is kept,
-- and the field says why: a key checked against one service is never kept
-- for another.
--
-- = What is shown
--
-- The model never holds a value kept: only whether the secret is set
-- ('Here'). The value typed stays in the model until it is kept, and is
-- then cleared ('typed'), as the password typed for an unlock is after
-- the unlock ('password').
--
-- = What changes elsewhere
--
-- Whether the vault is locked in this browser, and whether the secret is
-- set, are not the field's alone: another field, a plugin's own call, a
-- sign-out or an unlock in another tab change them. Each such change comes
-- as 'Changed', from the one place that sees them all (the keys this
-- browser keeps, and the host's changes of secrets:
-- 'Wiki.App.Assistant.secretsChanges'), and the field reads again, now or
-- after the ask out. So an unlock through one field shows the others
-- unlocked, and a lock shows them locked.
--
-- = The reader
--
-- A field is made for one reader ('Wiki.App.Plugin.FieldSlot'): a new
-- reader mounts a new field, so the model never changes reader.
--
-- = The keys
--
-- Each ask of the vault runs with the keys it registers in the page under
-- an owner of its own, made with the ask under its number. The step lets
-- the owner go ('Release') when the ask answers, and when the field closes
-- ('Finished') with an ask out.
module Wiki.App.View.SecretField.Machine
  ( Model (..)
  , Shown (..)
  , Event (..)
  , Answer (..)
  , Effect (..)
  , Job (..)
  , Told (..)
  , initial
  , initialBound
  , step
  , busy
  , needsOwner
  , reboundProblem
  )
where

import Data.Text qualified as T
import Protolude
import Text.Show (Show (showsPrec))

-- | 'Shown' is what the field knows of the secret: not read yet; readable,
-- and whether it is set; behind a vault locked in this browser; with
-- nobody signed in; or not readable, and why.
data Shown
  = Loading
  | Here Bool
  | LockedHere
  | NoReader
  | Unreadable Text
  deriving stock (Eq, Show)

-- | 'Job' is an ask of the host.
data Job
  = -- | Whether the secret is set.
    Read
  | -- | The plugin's check of a value, under the binding the field had
    -- when it was asked.
    Check Text Text
  | -- | Keep a value that passed the check: the text typed, and the value
    -- the check answered, which is kept.
    Put Text Text
  | -- | Forget the secret's value.
    Remove
  | -- | Unlock the vault with a passkey.
    UnlockByPasskey
  | -- | Unlock the vault with the user's password.
    UnlockByPassword Text
  deriving stock (Eq)

-- | Shown without the values it holds.
instance Show Job where
  showsPrec _ job rest = named <> rest
    where
      named :: [Char]
      named = case job of
        Read -> "Read"
        Check _ binding -> "Check <value> " <> show binding
        Put _ _ -> "Put <value>"
        Remove -> "Remove"
        UnlockByPasskey -> "UnlockByPasskey"
        UnlockByPassword _ -> "UnlockByPassword <password>"

-- | Whether a job asks the vault, with keys under an owner of its own:
-- every job but the plugin's check.
needsOwner :: Job -> Bool
needsOwner = \case
  Check _ _ -> False
  _ -> True

-- | 'Told' is what the field tells the plugin.
data Told = KeptIt | RemovedIt | UnlockedIt
  deriving stock (Eq, Show)

-- | 'Model' is the field's state.
data Model = Model
  { shown :: Shown
  , typed :: Text
  -- ^ The value typed, not kept yet.
  , password :: Text
  -- ^ The password typed for an unlock.
  , doing :: Maybe (Int, Job)
  -- ^ The ask out, by its number.
  , wanted :: Bool
  -- ^ Whether a read is wanted after the ask out.
  , next :: Int
  -- ^ The next ask's number.
  , problem :: Maybe Text
  -- ^ Why the last ask failed, until the next one is made.
  , binding :: Text
  -- ^ What the plugin's check is for, as the plugin's input last said
  -- ('Wiki.App.Plugin.FieldInput').
  }
  deriving stock (Eq, Show)

-- | 'Event' is what happens to the field.
data Event
  = -- | The field was mounted.
    Opened
  | -- | The reader typed a value.
    Typed Text
  | -- | The reader typed their password, to unlock.
    TypedPassword Text
  | -- | "Keep".
    KeepPressed
  | -- | "Forget".
    ForgetPressed
  | -- | "Unlock with a passkey".
    PasskeyPressed
  | -- | "Unlock with your password".
    PasswordPressed
  | -- | The plugin gave the field a new input, which binds its check to
    -- this ('Wiki.App.Plugin.FieldInput').
    Rebound Text
  | -- | What the secrets read as changed: a key kept or let go in this
    -- browser (the vault unlocked or locked), or a secret of a plugin kept
    -- or forgotten, by this field, another, or another tab.
    Changed
  | -- | The answer to the ask made under this number.
    Answered Int Answer
  | -- | The field closes.
    Finished
  deriving stock (Eq, Show)

-- | 'Answer' is what an ask answers: a read, with what it found; the
-- plugin's check, with the value to keep or why not; or another job, done
-- or not.
data Answer = ReadAs Shown | Checked (Either Text Text) | Done (Either Text ())
  deriving stock (Eq, Show)

-- | 'Effect' is what a step asks the component to do.
data Effect
  = -- | Do a job; its answer comes back as 'Answered' with the number.
    Ask Int Job
  | -- | Let go of the owner of keys of the ask with this number, if it
    -- has one.
    Release Int
  | -- | Tell the plugin.
    Tell Told
  deriving stock (Eq, Show)

-- | The field before anything is read.
initial :: Model
initial = Model {shown = Loading, typed = "", password = "", doing = Nothing, wanted = False, next = 0, problem = Nothing, binding = ""}

-- | The field before anything is read, for a plugin that binds its check
-- to this ('Rebound').
initialBound :: Text -> Model
initialBound binding = initial {binding}

-- | Whether an ask is out.
busy :: Model -> Bool
busy m = isJust m.doing

-- | The state after an event, and what it asks to be done.
step :: Event -> Model -> (Model, [Effect])
step event m = case event of
  Opened -> reading m
  Changed -> reading m
  Rebound binding -> (m {binding}, [])
  Typed t -> (m {typed = t}, [])
  TypedPassword t -> (m {password = t}, [])
  KeepPressed
    | Here _ <- m.shown, not (busy m), not (T.null (T.strip m.typed)) -> asking (Check m.typed m.binding) m
    | otherwise -> (m, [])
  ForgetPressed
    | Here True <- m.shown, not (busy m) -> asking Remove m
    | otherwise -> (m, [])
  PasskeyPressed
    | LockedHere <- m.shown, not (busy m) -> asking UnlockByPasskey m
    | otherwise -> (m, [])
  PasswordPressed
    | LockedHere <- m.shown, not (busy m), not (T.null m.password) -> asking (UnlockByPassword m.password) m
    | otherwise -> (m, [])
  Answered n answer -> case m.doing of
    Just (out, job)
      | out == n ->
          let (m', effects) = answered job answer m {doing = Nothing}
           in landed (Release n : effects) m'
    _ -> (m, [Release n])
  Finished -> (m, [Release n | Just (n, _) <- [m.doing]])

-- | What the answer to a job changes, and what it tells.
answered :: Job -> Answer -> Model -> (Model, [Effect])
answered job answer m = case (job, answer) of
  (Read, ReadAs s) -> (m {shown = s}, [])
  -- A read that threw: what it found is unknown.
  (Read, Done r) -> (m {shown = either Unreadable (const m.shown) r}, [])
  -- The check passed: the keep follows, of the value the check answered,
  -- if the check was for what the plugin binds it to now.
  (Check typed binding, Checked (Right value))
    | binding == m.binding -> asking (Put typed value) m
    | otherwise -> (m {problem = Just reboundProblem}, [])
  (Check _ _, Checked (Left why)) -> (m {problem = Just why}, [])
  (Put typed _, Done (Right ())) -> (m {shown = Here True, typed = if m.typed == typed then "" else m.typed}, [Tell KeptIt])
  (Remove, Done (Right ())) -> (m {shown = Here False}, [Tell RemovedIt])
  (UnlockByPasskey, Done (Right ())) -> unlocked m
  (UnlockByPassword _, Done (Right ())) -> unlocked m
  (_, Done (Left why)) -> (m {problem = Just why}, [])
  -- A read answers with what it found, a check with 'Checked', every other
  -- job with 'Done'.
  (_, ReadAs _) -> (m, [])
  (_, Checked _) -> (m, [])
  (Check _ _, Done (Right ())) -> (m, [])
  where
    -- The vault is open: the plugin is told, and the secret read again.
    unlocked x = (x {password = "", wanted = True}, [Tell UnlockedIt])

-- | What the field says when what the check was for changed while it was
-- out.
reboundProblem :: Text
reboundProblem = "The settings changed while the value was checked, so it was not kept. Keep it again to check it for the settings as they are now."

-- | The state once an ask answered: the read wanted meanwhile is asked,
-- unless the answer made another ask.
landed :: [Effect] -> Model -> (Model, [Effect])
landed effects m
  | busy m = (m, effects)
  | m.wanted = second (effects <>) (reading m)
  | otherwise = (m, effects)

-- | A read asked, or wanted after the ask out.
reading :: Model -> (Model, [Effect])
reading m
  | busy m = (m {wanted = True}, [])
  | otherwise = asking Read m {wanted = False}

-- | An ask made, under the next number. A new ask clears the last
-- problem.
asking :: Job -> Model -> (Model, [Effect])
asking job m = (m {doing = Just (m.next, job), next = m.next + 1, problem = if job == Read then m.problem else Nothing}, [Ask m.next job])
