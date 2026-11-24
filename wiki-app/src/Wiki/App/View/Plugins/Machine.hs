-- | 'Model' is what the settings' section "Plugins" ("Wiki.App.View.Plugins")
-- shows, and 'step' is the one place it changes (decision 82 of
-- docs/PLAN.md). Every input is an 'Event': the section opened, a switch
-- flipped, an answer of the backend, a change of session. Every output is
-- an 'Effect' as data: read what is on, write the user's state for a
-- plugin, tell the shell what is on now.
--
-- The shell keeps the model and carries out the effects
-- ("Wiki.App.Shell"), not the section: a write sent goes on after the
-- section is gone (the reader follows the assistant's link to its options,
-- or goes to the editor), its answer is stepped all the same, and the
-- shell hears what it changed. The section draws the model and raises the
-- reader's acts as events. A section opened again shows the model as it
-- is, a write still out with it, and reads only when nothing was read yet
-- in this session ('Opened').
--
-- = One ask at a time
--
-- A read and a write are one flight: while one of them is out, no other
-- is sent ('flight'). A write chosen meanwhile waits its turn, in the
-- order chosen ('queued'); a later choice for the same plugin takes the
-- place of the one that waits. A read wanted meanwhile is sent when the
-- ask out answers, failed or not, after what waits ('wanted'). The server
-- handles an ask sent after another's answer came after that one, so a
-- later answer is never an older one.
--
-- = What is shown
--
-- The model keeps one record of what the server keeps ('known'): the
-- plugins off for the wiki, and the user's choice for each plugin. A read
-- answers with all of it; a write with the user's choice for the one
-- plugin written, which is put in by the plugin's id. An answer is taken
-- only if no answer to a later ask was taken already
-- ("Wiki.App.View.Latest"), so the last answer about a plugin wins. While
-- a write for a plugin is out or waits, its switch shows the state chosen
-- ('writing'). A write refused is shown on its plugin's row ('refused'),
-- and a read follows: a write whose answer was lost may have been done.
--
-- = Who is signed in
--
-- The model is the signed-in user's, by the session's number
-- ("Wiki.App.View.Common"'s @sessionNumber@), which the shell gives it
-- each time it changes. The step that takes a new one forgets what is
-- known, what waits its turn and what was refused, and puts the asks out
-- behind it; the section, if it is open, then asks for a read
-- ('Opened'). An answer to an ask of the earlier session changes nothing
-- ('ofSession'), and a write chosen in it never goes out.
module Wiki.App.View.Plugins.Machine
  ( Model (..)
  , Event (..)
  , Effect (..)
  , initial
  , step
  , stateOf
  , shownState
  , offForWiki
  , askOf
  )
where

import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Protolude
import Wiki.Api.Plugins (PluginChoice (..), PluginId, PluginState (..), Plugins (..))
import Wiki.App.Backend (WikiError)
import Wiki.App.View.Latest (Ask, Asks, madeAfter, newAsk, noAsks, supersede, takes)

-- | 'Model' is the section's state.
data Model = Model
  { known :: Maybe (Either WikiError Plugins)
  -- ^ What the server keeps, as the section holds it, or why it could not
  -- be read; 'Nothing' until the first answer.
  , asks :: Asks
  -- ^ The reads and writes sent, and which one's answer was taken last.
  , flight :: Maybe Ask
  -- ^ The last ask sent, until its answer comes. While its answer can
  -- still be taken, no other is sent.
  , wanted :: Bool
  -- ^ Whether a read was wanted while that ask was out.
  , queued :: [(Int, PluginId, PluginState)]
  -- ^ The writes chosen while an ask was out, in the order chosen, each
  -- with the session it was chosen in.
  , writing :: Map PluginId PluginState
  -- ^ The state last chosen for each plugin whose write is out or waits.
  , refused :: Map PluginId WikiError
  -- ^ Why the last write for a plugin failed.
  , session :: Int
  -- ^ The number of the session the section was last given.
  , sessionFrom :: Asks
  -- ^ The asks as they were when that session began.
  , closed :: Bool
  -- ^ Whether the section has gone away: no event changes anything after.
  }
  deriving stock (Eq, Show)

-- | 'Event' is everything that happens to the section.
data Event
  = -- | Read what is on.
    Started
  | -- | The section was opened, or is open in a new session: a read,
    -- unless what is on was read in this session already.
    Opened
  | -- | The section goes away.
    Finished
  | -- | The section was given the number of the session signed in.
    Given Int
  | -- | The answer to the read made under this ticket.
    Loaded Ask (Either WikiError Plugins)
  | -- | The user chose a state for a plugin: on, off, or its default.
    Chosen PluginId PluginState
  | -- | The answer to the write for this plugin sent under this ticket:
    -- the user's choice for it after the write.
    Written Ask PluginId (Either WikiError PluginChoice)
  deriving stock (Show)

-- | 'Effect' is what a step asks the component to do.
data Effect
  = -- | Read what is on; the answer comes back as 'Loaded'.
    Load Ask
  | -- | Set the user's state for a plugin; the answer comes back as
    -- 'Written'.
    Write Ask PluginId PluginState
  | -- | Tell the shell what is on now, as the model holds it, so that
    -- the rest of the page follows at once.
    Tell Plugins
  deriving stock (Eq, Show)

-- | The section before it is mounted, for a session.
initial :: Int -> Model
initial session =
  Model
    { known = Nothing
    , asks = noAsks
    , flight = Nothing
    , wanted = False
    , queued = []
    , writing = mempty
    , refused = mempty
    , session
    , sessionFrom = noAsks
    , closed = False
    }

-- | The section after an event, and what it asks to be done. An answer
-- taken that changes what the section holds about any plugin is told to
-- the shell ('Tell'), whatever the ask was: a read that follows a write
-- whose answer was lost may be the first to say the write was done.
step :: Event -> Model -> (Model, [Effect])
step event m =
  let (m', fx) = stepping event m
   in (m', fx <> [Tell k | not m.closed, known' m' /= known' m, Just k <- [known' m']])

stepping :: Event -> Model -> (Model, [Effect])
stepping event m
  | m.closed = (m, [])
  | otherwise = case event of
      Started -> reading m
      Opened
        | isNothing (known' m) -> reading m
        | otherwise -> (m, [])
      Finished -> (m {closed = True}, [])
      Given session
        | session /= m.session ->
            let behind = supersede m.asks
             in ((initial session) {asks = behind, sessionFrom = behind}, [])
        | otherwise -> (m, [])
      Loaded asked answer
        | ofSession asked m -> landed asked (taking asked answer m)
        | otherwise -> landed asked m
      Chosen p wanted'
        | maybe False (Set.member p . offForWiki) (known' m) -> (m, [])
        | shownState m p == wanted' -> (m, [])
        | otherwise -> writingNow p wanted' m {writing = Map.insert p wanted' m.writing, refused = Map.delete p m.refused}
      Written asked p answer
        | not (ofSession asked m) -> landed asked m
        | otherwise ->
            let waits = any (\(_, q, _) -> q == p) m.queued
                m1 = m {writing = if waits then m.writing else Map.delete p m.writing}
                (m2, fx1) = case answer of
                  -- A success clears the plugin's refusal: the last answer
                  -- about it is not one.
                  Right choice -> case takes asked m1.asks of
                    Just after -> (m1 {asks = after, known = withChoice p choice <$> m1.known, refused = Map.delete p m1.refused}, [])
                    Nothing -> (m1, [])
                  -- A write that failed may have been done all the same
                  -- (its answer lost on the way): a read follows.
                  Left e -> reading m1 {refused = Map.insert p e m1.refused}
                (m3, fx2) = landed asked m2
             in (m3, fx1 <> fx2)

-- | What the server keeps, as the section holds it, once read.
known' :: Model -> Maybe Plugins
known' m = m.known >>= rightToMaybe

-- | The plugins off for the wiki.
offForWiki :: Plugins -> Set PluginId
offForWiki p = p.offForWiki

-- | The user's state for a plugin, as the server keeps it: the default
-- when they made no choice.
stateOf :: Plugins -> PluginId -> PluginState
stateOf known p = maybe PluginDefault (\c -> c.state) (Map.lookup p known.mine)

-- | The state a plugin's switch shows: the one chosen while its write is
-- out or waits, and otherwise the server's.
shownState :: Model -> PluginId -> PluginState
shownState m p = fromMaybe (maybe PluginDefault (`stateOf` p) (known' m)) (Map.lookup p m.writing)

-- | What is known with the user's choice for a plugin put in, by its id.
-- A choice of the default with no options is none, as the server keeps it:
-- the plugin is taken out. A read that failed holds nothing to put it in.
withChoice :: PluginId -> PluginChoice -> Either WikiError Plugins -> Either WikiError Plugins
withChoice p choice = fmap (\k -> k {mine = if kept then Map.insert p choice k.mine else Map.delete p k.mine})
  where
    kept = choice.state /= PluginDefault || isJust choice.options

-- | The answer to a read taken, if no answer to a later ask was. A failure
-- is shown only where nothing is held: what is held is kept.
taking :: Ask -> Either WikiError Plugins -> Model -> Model
taking asked answer m = case takes asked m.asks of
  Nothing -> m
  Just after -> case answer of
    Right _ -> m {asks = after, known = Just answer}
    Left _ -> case m.known of
      Just (Right _) -> m {asks = after}
      _ -> m {asks = after, known = Just answer}

-- | The section after the answer to an ask came, taken or not, failed or
-- not: when it was the ask out, the next is sent, the first write waiting
-- of this session, or else the read wanted.
landed :: Ask -> Model -> (Model, [Effect])
landed asked m
  | m.flight /= Just asked = (m, [])
  | otherwise = case [(p, s) | (n, p, s) <- m.queued, n == m.session] of
      (p, s) : rest -> send p s m {flight = Nothing, queued = [(m.session, q, t) | (q, t) <- rest]}
      []
        | m.wanted -> sendRead m {flight = Nothing, queued = []}
        | otherwise -> (m {flight = Nothing, queued = []}, [])

-- | Whether an ask was made in the session the section is in.
ofSession :: Ask -> Model -> Bool
ofSession asked m = asked `madeAfter` m.sessionFrom

-- | Whether an ask is out whose answer can still be taken.
inFlight :: Model -> Bool
inFlight m = maybe False (\a -> isJust (takes a m.asks)) m.flight

-- | A read begun, or wanted while an ask is out.
reading :: Model -> (Model, [Effect])
reading m
  | inFlight m = (m {wanted = True}, [])
  | otherwise = sendRead m

-- | A write begun, or put in its turn while an ask is out, in the place of
-- one for the same plugin that waits.
writingNow :: PluginId -> PluginState -> Model -> (Model, [Effect])
writingNow p s m
  | inFlight m = (m {queued = [x | x@(_, q, _) <- m.queued, q /= p] <> [(m.session, p, s)]}, [])
  | otherwise = send p s m

sendRead :: Model -> (Model, [Effect])
sendRead m = let (ticket, after) = newAsk m.asks in (m {asks = after, flight = Just ticket, wanted = False}, [Load ticket])

send :: PluginId -> PluginState -> Model -> (Model, [Effect])
send p s m = let (ticket, after) = newAsk m.asks in (m {asks = after, flight = Just ticket}, [Write ticket p s])

-- | The ticket an effect was sent under, if it is an ask.
askOf :: Effect -> Maybe Ask
askOf = \case
  Load a -> Just a
  Write a _ _ -> Just a
  Tell _ -> Nothing
