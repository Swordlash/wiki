-- | 'Model' is what the editor knows of the external plugins it runs, and
-- 'step' is the one place it changes (decision 82 of docs/PLAN.md): which
-- plugins are installed, the frame each plugin that is on runs in, the ask
-- each frame has out, what each plugin last answered, and which plugins
-- were stopped and why. A hidden component of the editor runs it
-- ("Wiki.App.Plugin.External.Frames") and carries out its effects. It
-- imports nothing of Halogen.
--
-- = The input
--
-- The editor gives the machine an 'Input' with each render: the session's
-- number, which plugins are on ("Wiki.Api.Plugins"), the external plugins
-- installed ("Wiki.Api.Plugins.External"), whether both are this
-- session's read of them, the text to diagnose (the editor's text as last
-- checked), and whether the page is in an encrypted space. The shell's
-- plugins' state ("Wiki.App.View.Plugins.Machine") reads the first three,
-- and the editor takes them from the context. Each field changes in the
-- step that takes the input ('Given'): a new text is a new 'version', a
-- new session forgets which plugins were stopped, and the frames follow
-- at once, so that a plugin installed, removed, replaced or turned off in
-- the settings of this tab starts or stops in the editor in that step.
--
-- = Which plugins run
--
-- The plugins that run are the ones that are on, among those installed
-- ("Wiki.App.Plugin.Registry"'s 'externalOn'), and none in an encrypted
-- space. None runs either until this session's read of them has answered
-- ('choicesRead'): until then the shell holds the defaults, under which a
-- plugin the reader turned off is on, and a plugin is sent the page's text
-- only when the reader's own choices say it may be. A read of them that
-- fails keeps every plugin stopped. Each has one frame, made when the plugin starts to run
-- and closed when it stops: turned off, removed, replaced by another
-- version (another bundle), or stopped for misbehaving. A plugin stopped
-- stays stopped while the same version stays on, so its frame is not made
-- again until the editor opens again.
--
-- = One ask at a time
--
-- A frame starts 'Starting': the plugin is asked nothing until it says it
-- is ready. Then it is asked for the problems of the newest text, under a
-- ticket ('Asking'); while that ask is out, a newer text waits, and is
-- sent when the answer comes. An answer is taken only under the ticket of
-- the ask out: one under another number is late or made up, and is
-- dropped. The answer's problems are of the text that ask sent, which
-- 'Heard' keeps with them.
--
-- = A plugin that misbehaves
--
-- A plugin is stopped, its frame closed and its problems dropped, when it
-- does not say it is ready within 'startWithin' seconds, does not answer
-- an ask within 'answerWithin', fails, sends what the protocol does not
-- have, sends a message it was not asked for (a second @ready@, an answer
-- to no ask out or under another ticket, or more messages than it was
-- asked for, which the frame's JavaScript counts), reports problems
-- outside the text, or its frame goes to another page. The wait for each is a pause under the frame's number, cancelled
-- when the answer comes, so it is never taken for a later ask's.
--
-- = Frames, by number
--
-- Each frame has a number of its own, which the frame's messages and its
-- pauses carry. A message from a frame that is no longer the plugin's (it
-- was closed, and its last message came after) names a number no frame
-- has, and changes nothing.
module Wiki.App.Plugin.External.Machine
  ( Input (..)
  , Model (..)
  , Frame (..)
  , Phase (..)
  , Out (..)
  , Stopped (..)
  , Event (..)
  , Effect (..)
  , initial
  , step
  , wanted
  , stoppedPlugins
  , restInEncrypted
  , startWithin
  , answerWithin
  )
where

import Data.Aeson (Value)
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Protolude
import Wiki.Api.Plugins (PluginId, Plugins)
import Wiki.Api.Plugins.External (ExternalManifest (..), ExternalPlugin (..), ExternalPlugins, framePath)
import Wiki.App.Plugin.External (Heard (..))
import Wiki.App.Plugin.External.Frame (FrameSaid (..))
import Wiki.App.Plugin.External.Protocol (FromPlugin (..), answerWithin, diagnose, fromPlugin, maxMessageLength, problemsIn, startWithin)
import Wiki.App.Plugin.Registry (externalOn)

-- | 'Input' is what the editor gives the machine with each render.
data Input = Input
  { session :: Int
  -- ^ The number of the reader's session ("Wiki.App.View.Common"'s
  -- @sessionNumber@).
  , known :: Plugins
  -- ^ Which plugins are on, as the shell last read them.
  , installed :: ExternalPlugins
  -- ^ The external plugins installed, as the shell last read them or
  -- changed them.
  , choicesRead :: Bool
  -- ^ Whether 'known' and 'installed' are this session's read
  -- ("Wiki.App.View.Common"'s @pluginsRead@), and not the defaults the
  -- shell holds until that read answers, or when it failed. No plugin
  -- runs while it is not.
  , text :: Maybe Text
  -- ^ The text to diagnose: the editor's text as last checked, once the
  -- page is loaded.
  , encrypted :: Bool
  -- ^ Whether the page is in an encrypted space, where no external plugin
  -- runs.
  }
  deriving stock (Eq, Show)

-- | 'Model' is the machine's state.
data Model = Model
  { input :: Input
  -- ^ The input last given.
  , version :: Int
  -- ^ The text's version: one more each time the input's text changes.
  , frames :: Map PluginId Frame
  -- ^ The frame of each plugin that runs.
  , stopped :: Map PluginId Stopped
  -- ^ The plugins stopped, and why, while the same version is on.
  , heard :: Map PluginId Heard
  -- ^ What each plugin that runs last answered.
  , next :: Int
  -- ^ The next number for a frame or a ticket.
  }
  deriving stock (Eq, Show)

-- | 'Frame' is a plugin's frame: its number, the plugin it runs, and
-- where its asks are.
data Frame = Frame
  { number :: Int
  , plugin :: ExternalPlugin
  , phase :: Phase
  }
  deriving stock (Eq, Show)

-- | 'Phase' is where a frame's asks are.
data Phase
  = -- | The plugin has not said it is ready; the wait for it is under this
    -- ticket.
    Starting Int
  | -- | Ready, with no ask out; the version it last answered, if any.
    Idle (Maybe Int)
  | -- | An ask out.
    Asking Out
  deriving stock (Eq, Show)

-- | 'Out' is an ask out: its ticket, and the version and text it sent.
data Out = Out
  { ticket :: Int
  , version :: Int
  , text :: Text
  }
  deriving stock (Eq, Show)

-- | 'Stopped' is a plugin stopped: the plugin, as it was installed, and
-- why, for the author, as the end of a sentence without its full stop
-- ("it did not answer within 10 seconds").
data Stopped = Stopped
  { plugin :: ExternalPlugin
  , reason :: Text
  }
  deriving stock (Eq, Show)

-- | 'Event' is everything that happens to the machine.
data Event
  = -- | The editor gave this input.
    Given Input
  | -- | The frame of this number said something.
    Said Int FrameSaid
  | -- | The wait under this frame's number and this ticket ended
    -- ('Right'), or what had to be done before it, making the frame or
    -- sending to it, failed ('Left', with the reason).
    Waited Int Int (Either Text ())
  | -- | The editor closes: close every frame.
    Finished
  deriving stock (Eq, Show)

-- | 'Effect' is what a step asks the component to do.
data Effect
  = -- | Make the frame of this number at this address, then wait
    -- 'startWithin' under the frame's number; the end of the wait comes
    -- back as 'Waited' with the ticket.
    Open Int Int Text
  | -- | Send this message to the frame of this number, then wait
    -- 'answerWithin' under the frame's number, as 'Open' does.
    Send Int Int Value
  | -- | Stop the wait under the frame's number.
    StopWaiting Int
  | -- | Close the frame of this number.
    Close Int
  | -- | Tell the editor what the plugins that run last answered.
    Tell (Map PluginId Heard)
  deriving stock (Eq, Show)

-- | The machine for the first input.
initial :: Input -> Model
initial input =
  Model
    { input
    , version = 0
    , frames = mempty
    , stopped = mempty
    , heard = mempty
    , next = 0
    }

-- | The plugins that are to run now: the ones on, among those installed
-- in this session, and none in an encrypted space.
wanted :: Model -> [ExternalPlugin]
wanted m
  | m.input.encrypted || not m.input.choicesRead = []
  | otherwise = externalOn m.input.known m.input.installed

-- | The plugins stopped, with why, in the order of their ids.
stoppedPlugins :: Model -> [Stopped]
stoppedPlugins m = Map.elems m.stopped

-- | Whether some plugins would run, were the page not in an encrypted
-- space.
restInEncrypted :: Model -> Bool
restInEncrypted m = m.input.encrypted && not (null (wanted m {input = m.input {encrypted = False}}))

-- | The state after an event, and what it asks to be done.
step :: Event -> Model -> (Model, [Effect])
step event m = case event of
  Given input -> settled m (givenInput input m, [])
  Said n said -> case frameNumbered n m of
    Nothing -> (m, [])
    Just (p, f) -> settled m (heardFrom p f said m)
  Waited n ticket ended -> case frameNumbered n m of
    Just (p, f) -> case (f.phase, ended) of
      (Starting t, Left why) | t == ticket -> settled m (stop p ("it could not be started: " <> why) m)
      (Starting t, Right ()) | t == ticket -> settled m (stop p ("it did not start within " <> seconds startWithin <> ".") m)
      (Asking out, Left why) | out.ticket == ticket -> settled m (stop p ("the text could not be sent to it: " <> why) m)
      (Asking out, Right ()) | out.ticket == ticket -> settled m (stop p ("it did not answer within " <> seconds answerWithin <> ".") m)
      _ -> (m, [])
    Nothing -> (m, [])
  Finished ->
    ( m {frames = mempty, heard = mempty}
    , concat [[StopWaiting f.number, Close f.number] | f <- Map.elems m.frames]
    )
  where
    seconds d = show (round d :: Int) <> " seconds"

-- | The model with a new input taken: a new session forgets which plugins
-- were stopped; a new text is a new version.
givenInput :: Input -> Model -> Model
givenInput input m
  | input.session /= m.input.session = m1 {stopped = mempty}
  | otherwise = m1
  where
    m1 = m {input, version = if input.text /= m.input.text then m.version + 1 else m.version}

-- | The frame with this number, and its plugin's id.
frameNumbered :: Int -> Model -> Maybe (PluginId, Frame)
frameNumbered n m = find (\(_, f) -> f.number == n) (Map.toList m.frames)

-- | What a frame said, taken.
heardFrom :: PluginId -> Frame -> FrameSaid -> Model -> (Model, [Effect])
heardFrom p f said m = case said of
  Navigated -> stop p "its frame went to another page." m
  Unreadable -> stop p "it sent a message that is not text of at most 1 MiB, or not JSON." m
  Unasked -> stop p "it sent a message it was not asked for." m
  Message value -> case fromPlugin value of
    Left why -> stop p why m
    Right Ready -> case f.phase of
      Starting _ -> (m {frames = Map.insert p f {phase = Idle Nothing} m.frames}, [StopWaiting f.number])
      _ -> stop p "it said it was ready a second time." m
    Right (Failed why) -> stop p ("it failed: " <> why) m
    Right (Problems re reported) -> case f.phase of
      Asking out
        | out.ticket == re -> case problemsIn f.plugin.manifest.name out.text reported of
            Left why -> stop p why m
            Right problems ->
              ( m {frames = Map.insert p f {phase = Idle (Just out.version)} m.frames, heard = Map.insert p Heard {text = out.text, problems} m.heard}
              , [StopWaiting f.number]
              )
      -- An answer to no ask out, or under another number: the plugin
      -- answers once an ask, and only the ask out.
      _ -> stop p "it sent an answer it was not asked for." m

-- | A plugin stopped: its frame closed, its problems dropped, and why
-- kept while the same version is on.
stop :: PluginId -> Text -> Model -> (Model, [Effect])
stop p why m = case Map.lookup p m.frames of
  Nothing -> (m, [])
  Just f ->
    ( m {frames = Map.delete p m.frames, heard = Map.delete p m.heard, stopped = Map.insert p Stopped {plugin = f.plugin, reason = reasonOf why} m.stopped}
    , [StopWaiting f.number, Close f.number]
    )

-- | Why a plugin was stopped, as the editor shows it: without its full
-- stop, and cut to 'maxMessageLength' characters, since part of it may be
-- the plugin's own words (its @failed@ message).
reasonOf :: Text -> Text
reasonOf why = T.dropWhileEnd (== '.') (if T.length why > maxMessageLength then T.take maxMessageLength why <> "…" else why)

-- | After a step: the frames brought in line with the plugins wanted
-- (frames closed for the plugins no longer wanted, made for the plugins
-- wanted that have none and were not stopped), each ready frame asked for
-- the newest text, and the editor told when what was heard changed since
-- the model before the step.
settled :: Model -> (Model, [Effect]) -> (Model, [Effect])
settled before (m0, fx0) =
  let running = Map.fromList [(p.manifest.id, p) | p <- wanted m0]
      same pid p = Map.lookup pid running == Just p
      (kept, gone) = Map.partitionWithKey (\pid f -> same pid f.plugin) m0.frames
      m1 = m0 {frames = kept, stopped = Map.filterWithKey (\pid s -> same pid s.plugin) m0.stopped, heard = Map.restrictKeys m0.heard (Map.keysSet kept)}
      closing = concat [[StopWaiting f.number, Close f.number] | f <- Map.elems gone]
      starting = [p | p <- Map.elems running, not (Map.member p.manifest.id m1.frames), not (Map.member p.manifest.id m1.stopped)]
      (m2, opening) = foldl' open (m1, []) starting
      (m3, asking) = foldl' askNewest (m2, []) (Map.toList m2.frames)
      telling = [Tell m3.heard | m3.heard /= before.heard]
   in (m3, fx0 <> closing <> opening <> asking <> telling)
  where
    open (m, fx) p =
      let n = m.next
          ticket = m.next + 1
       in (m {next = m.next + 2, frames = Map.insert p.manifest.id Frame {number = n, plugin = p, phase = Starting ticket} m.frames}, fx <> [Open n ticket (framePath p)])
    askNewest (m, fx) (pid, f) = case (f.phase, m.input.text) of
      (Idle answered, Just text)
        | answered /= Just m.version ->
            let ticket = m.next
             in (m {next = m.next + 1, frames = Map.insert pid f {phase = Asking Out {ticket, version = m.version, text}} m.frames}, fx <> [Send f.number ticket (diagnose ticket text)])
      _ -> (m, fx)
